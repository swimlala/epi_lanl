CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-01-14T22:35:49Z creation; 2022-02-04T23:30:07Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�       =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  T   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�       Y�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�       v�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       �x   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       �L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � \   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�          	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` /0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   /�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   5�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ;�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T A�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   A�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   A�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   A�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   A�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � B   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   B�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   B�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    B�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        B�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        B�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       B�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    B�Argo profile    3.1 1.2 19500101000000  20220114223549  20220204223522  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_196                 6810_008521_196                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ٱ�N���@ٱ�N���11  @ٱ�|���@ٱ�|���@0́o h�@0́o h��d��Ew�U�d��Ew�U11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@=p�@}p�@��R@�  @�\A ��A��A!G�A,(�A@  A`  A�  A��A��A��A�  A�  A߮A�Q�B (�B  B(�B  B (�B(Q�B0(�B8(�B@(�BH  BP(�BW�
B`  Bh(�Bo�
Bx(�B�{B��B��
B��
B�  B�  B�  B�  B�  B�{B��B�  B�  B�{B�{B�(�B�  B��B��B�{B�  B�  B�  B�  B�{B�{B�  B�  B�{B�  B��B��
B��C  C  C
=C  C

=C
=C
=C  C��C��C
=C  C��C��C�C��C"{C$  C&  C(
=C*  C,  C-��C/�C2  C4
=C6
=C8  C9��C<  C>  C@  CB
=CD
=CF  CG��CJ  CL
=CM��CP  CR
=CT
=CV  CX
=CZ
=C\{C^  C_��Cb{Cd  Cf  Cg��Ci�Cl
=Cm��Cp  Cq��Cs��Cu��Cw�Cy��C{�C}�C��C�  C�  C���C�  C�C�  C�  C�  C���C�C�  C���C�  C�C�C�C�  C�  C�C�  C�  C���C�
=C�C���C�  C�C�C�  C�  C�
=C�  C�  C�  C�C���C�  C�  C���C�  C�C�  C�C���C�  C�  C�  C�C�  C�  C���C���C�  C�  C���C�  C�C�  C���C�  C�C���C���C�  C���C�  C�C�C�  C�  C�C�C���C�  C�C�C�  C���C���C�C�C�C�C�C���C���C�C�
=C�
=C�  C���C���C��C��C�  C�  C�C�
=C�C���C���C�  C�  C���C�  C�
=C�  C���C�C�C���C�  C�
=C�
=C�C�C�  C���C���C���C�  C�C�  C�
=C�C�  C�  D   D � D  D}qD�D� D  D}qD  D��D�D��D�D��D�D� D  D�D�qD	}qD
D
� D�D� D  D� D  D��D�qD��DD��D  D��D  D}qD  D� D�qD��D  D}qD  D� D  D}qD�D��D  D�D�D� D  D� DD� D  D� D  D��D�D��D��DxRD   D � D!  D!��D!�qD"}qD#  D#�D$�D$��D%  D%��D&�D&��D'�D'� D(  D(� D(�qD)}qD)�qD*� D*�qD+}qD+�qD,� D-�D-� D.�D.�D.�qD/}qD0  D0� D0�qD1z�D1�qD2}qD2�qD3� D4D4��D5D5� D6  D6��D7  D7}qD7��D8}qD9  D9� D:  D:� D;  D;z�D<  D<��D<�qD=}qD>  D>��D?�D?� D@  D@��DA�DA�DBDB}qDC  DC� DC�qDD� DE�DE�DF  DF}qDG  DG��DG�qDH��DI�DI}qDJ  DJ��DK  DK��DL  DLz�DM  DM��DN  DN��DO�DO� DP  DP� DP�qDQ� DR�DR� DR�qDSz�DS��DT}qDT�qDU� DVDV�DW�DW��DX  DX� DX�qDY� DZ  DZ��D[�D[�D\  D\}qD]  D]�D^  D^}qD_�D_��D`  D`� Da  Da� Db  Db}qDc�Dc��Dc�qDd}qDe  De� Df�Df� Df�qDg� Dh�Dh�Di  DixRDi��Dj� Dk�Dk� Dk�qDlz�Dm  Dm��Dm�qDn� Do�Do��Dp  Dp}qDq�Dq��Dr�Dr}qDr�qDs}qDs�qDt� Du  Du��Dv�Dv� Dw  Dw� DxDx� Dx�qDy}qDz  Dz� Dz�qD{��D|D|� D|�qD}}qD~�D~� D  D� D�HD�AHD�� D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D��HD��HD�  D�>�D�}qD��qD�  D�@ D�~�D�� D�  D�>�D�~�D���D�  D�B�D��HD���D���D�AHD�u�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?\)?��?aG�?��?��R?���?�ff@�@��@0��@8Q�@L��@fff@u@��@��@�p�@��
@�33@��R@�ff@�33@�G�@���@�
=A ��A�A��A�\AffAp�A#33A'�A-p�A4z�A8Q�A>�RAC�
AH��AN�RAR�\AZ=qA_\)Ac33Aj=qAo\)Atz�A{�A�  A�=qA�{A�Q�A�33A��RA�G�A��
A�
=A���A�(�A��A��A�z�A��A��\A�z�A�  A�33A��A���A�33A�p�A�G�A�(�A�{Aə�A��
A�{Aљ�A�(�A�ffA��A�(�A�
=A�=qA�z�A�
=A�\A���A�A��HA�p�A��A�33A�p�B (�B�B
=BQ�B{B33B��B
=qB\)B��B�\B�B�B�RB�
B�B�HB  BG�B
=B  BG�B
=B   B!G�B"�HB$  B%�B&�HB'�
B)G�B*�HB+�B-p�B.�HB0  B1p�B3
=B4z�B5��B7
=B8��B:=qB;33B<��B>ffB?�B@��BBffBC\)BDz�BF{BG�BHz�BIBK�BL��BM�BO�BP��BR{BS�
BT��BV=qBX  BYp�BZffB\  B]B^�HB`(�Ba�Bc
=BdQ�Bf{Bg�Bh��BjffBk�
Bm�BnffBo�
Bqp�BrffBt(�Bup�Bv�\Bx(�By��Bz�RB{�
B}��B
=B�  B���B��B�{B��HB��B�=qB���B��
B�Q�B��B��B�z�B�33B�{B��\B�33B�{B���B�\)B�{B��HB�p�B�  B���B��B�(�B��HB�B�=qB���B��
B�z�B�
=B�B��\B�\)B��
B���B�p�B�  B���B�p�B�(�B���B��B�Q�B�
=B���B�ffB�G�B�B��RB�p�B�{B��RB��B�ffB���B�B���B�G�B��
B��RB��B�{B��HB�B�ffB���B�B���B�33B��
B��RB�\)B��B���B��B�(�B��RB�p�B�Q�B���B��B�Q�B��B��B�Q�B�33B��B��\B�33B�{B��HB�p�B�(�B��B��
B�ffB�G�B�{BȸRB�\)B�(�B�
=BˮB�Q�B��B�  BΏ\B�33B�  B��HBхB�{B���B�B�z�B�
=B�B֣�B�G�B��
Bأ�BمB�(�BڸRB�p�B�Q�B��HB�p�B�(�B��HB߅B��B���B�G�B�B�ffB���B�\)B��B��B���B�p�B�  B�=qB���B�G�B癚B��B�z�B��HB��B陚B�{B�\B���B�33B�B�=qB�z�B���B�p�B��B�(�B��B��B�\)B�  B�ffB�RB�33B�B�{B�z�B�
=B�B��
B�Q�B��HB�G�B���B�=qB���B���B�\)B�  B�z�B���B�33B�B�Q�B��\B���B��B�{B�z�B���B�33B�B�=qB��\B���B�p�C   C (�C Q�C ��C �HC{C33CffC�RC�C
=CG�C�\CC�C{CffC��C�
C  C33C�C�C�
C�C\)C�C�RC  CG�C\)C�\C�
C{C(�CffC�RC��C  C=qC�C�RC�HC	{C	ffC	��C	C	�C
�C
ffC
��C
�RC  C33CQ�C�C��C  C�CQ�C��C�
C��C(�Cz�C��C��C{CG�CffC��C�HC{C33CffC�C��C��C=qCffC�\C�C��C(�C=qC�C�RC�HC  CG�Cz�C��CC  C33CQ�C��C��C�C(�C\)Cz�C��C�C{C=qCp�C�CC��C=qC\)Cz�C�RC�HC  CG�Cp�C�CC��C
=CG�Cz�C�\CC  C{C=qCz�C��C�C�C{C(�CffC��C�C�
C�CG�C\)C�\C��C�HC
=CG�Cz�C��C�RC  C(�CG�Cp�C��C�HC��C�C\)C��C��C�HC{C�CffC��CC�C (�C G�C ffC �C �HC ��C!33C!p�C!�\C!�C!�C"(�C"G�C"p�C"�RC"�HC#  C#(�C#p�C#��C#��C#��C$=qC$p�C$��C$�RC%  C%=qC%\)C%�C%�
C%��C&�C&\)C&��C&C&��C'=qC'p�C'�\C'��C(
=C(G�C(p�C(�\C(�
C)�C)=qC)ffC)�C)�HC*  C*=qC*z�C*�RC*�HC+
=C+\)C+��C+��C+��C,=qC,�C,�C,�HC-33C-\)C-�\C-�
C.�C.G�C.z�C.C/
=C/=qC/ffC/�C0  C0(�C0\)C0��C0�HC1(�C1\)C1�\C1C2{C2Q�C2z�C2�RC3  C3G�C3p�C3��C3�HC4(�C4p�C4��C4�
C5{C5Q�C5��C5�HC6�C6G�C6�C6�
C7�C7G�C7z�C7��C8{C8=qC8ffC8��C8�HC9(�C9z�C9��C9�
C:{C:\)C:�C:�C;�C;Q�C;�\C;�HC<(�C<p�C<��C<�
C={C=ffC=�C=�C>�C>Q�C>�\C>�HC?(�C?Q�C?�\C?C@
=C@Q�C@��C@C@��CA33CA�CACA��CB(�CBp�CB�RCC  CC33CCffCC��CC�HCD33CDp�CD��CD�
CE{CEffCE�CE�CF�CF\)CF��CF�HCG(�CGffCG��CG111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                          111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  ?��H@=p�@}p�@��R@�  @�\A ��A��A!G�A,(�A@  A`  A�  A��A��A��A�  A�  A߮A�Q�B (�B  B(�B  B (�B(Q�B0(�B8(�B@(�BH  BP(�BW�
B`  Bh(�Bo�
Bx(�B�{B��B��
B��
B�  B�  B�  B�  B�  B�{B��B�  B�  B�{B�{B�(�B�  B��B��B�{B�  B�  B�  B�  B�{B�{B�  B�  B�{B�  B��B��
B��C  C  C
=C  C

=C
=C
=C  C��C��C
=C  C��C��C�C��C"{C$  C&  C(
=C*  C,  C-��C/�C2  C4
=C6
=C8  C9��C<  C>  C@  CB
=CD
=CF  CG��CJ  CL
=CM��CP  CR
=CT
=CV  CX
=CZ
=C\{C^  C_��Cb{Cd  Cf  Cg��Ci�Cl
=Cm��Cp  Cq��Cs��Cu��Cw�Cy��C{�C}�C��C�  C�  C���C�  C�C�  C�  C�  C���C�C�  C���C�  C�C�C�C�  C�  C�C�  C�  C���C�
=C�C���C�  C�C�C�  C�  C�
=C�  C�  C�  C�C���C�  C�  C���C�  C�C�  C�C���C�  C�  C�  C�C�  C�  C���C���C�  C�  C���C�  C�C�  C���C�  C�C���C���C�  C���C�  C�C�C�  C�  C�C�C���C�  C�C�C�  C���C���C�C�C�C�C�C���C���C�C�
=C�
=C�  C���C���C��C��C�  C�  C�C�
=C�C���C���C�  C�  C���C�  C�
=C�  C���C�C�C���C�  C�
=C�
=C�C�C�  C���C���C���C�  C�C�  C�
=C�C�  C�  D   D � D  D}qD�D� D  D}qD  D��D�D��D�D��D�D� D  D�D�qD	}qD
D
� D�D� D  D� D  D��D�qD��DD��D  D��D  D}qD  D� D�qD��D  D}qD  D� D  D}qD�D��D  D�D�D� D  D� DD� D  D� D  D��D�D��D��DxRD   D � D!  D!��D!�qD"}qD#  D#�D$�D$��D%  D%��D&�D&��D'�D'� D(  D(� D(�qD)}qD)�qD*� D*�qD+}qD+�qD,� D-�D-� D.�D.�D.�qD/}qD0  D0� D0�qD1z�D1�qD2}qD2�qD3� D4D4��D5D5� D6  D6��D7  D7}qD7��D8}qD9  D9� D:  D:� D;  D;z�D<  D<��D<�qD=}qD>  D>��D?�D?� D@  D@��DA�DA�DBDB}qDC  DC� DC�qDD� DE�DE�DF  DF}qDG  DG��DG�qDH��DI�DI}qDJ  DJ��DK  DK��DL  DLz�DM  DM��DN  DN��DO�DO� DP  DP� DP�qDQ� DR�DR� DR�qDSz�DS��DT}qDT�qDU� DVDV�DW�DW��DX  DX� DX�qDY� DZ  DZ��D[�D[�D\  D\}qD]  D]�D^  D^}qD_�D_��D`  D`� Da  Da� Db  Db}qDc�Dc��Dc�qDd}qDe  De� Df�Df� Df�qDg� Dh�Dh�Di  DixRDi��Dj� Dk�Dk� Dk�qDlz�Dm  Dm��Dm�qDn� Do�Do��Dp  Dp}qDq�Dq��Dr�Dr}qDr�qDs}qDs�qDt� Du  Du��Dv�Dv� Dw  Dw� DxDx� Dx�qDy}qDz  Dz� Dz�qD{��D|D|� D|�qD}}qD~�D~� D  D� D�HD�AHD�� D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D��HD��HD�  D�>�D�}qD��qD�  D�@ D�~�D�� D�  D�>�D�~�D���D�  D�B�D��HD���D���D�AHG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?\)?��?aG�?��?��R?���?�ff@�@��@0��@8Q�@L��@fff@u@��@��@�p�@��
@�33@��R@�ff@�33@�G�@���@�
=A ��A�A��A�\AffAp�A#33A'�A-p�A4z�A8Q�A>�RAC�
AH��AN�RAR�\AZ=qA_\)Ac33Aj=qAo\)Atz�A{�A�  A�=qA�{A�Q�A�33A��RA�G�A��
A�
=A���A�(�A��A��A�z�A��A��\A�z�A�  A�33A��A���A�33A�p�A�G�A�(�A�{Aə�A��
A�{Aљ�A�(�A�ffA��A�(�A�
=A�=qA�z�A�
=A�\A���A�A��HA�p�A��A�33A�p�B (�B�B
=BQ�B{B33B��B
=qB\)B��B�\B�B�B�RB�
B�B�HB  BG�B
=B  BG�B
=B   B!G�B"�HB$  B%�B&�HB'�
B)G�B*�HB+�B-p�B.�HB0  B1p�B3
=B4z�B5��B7
=B8��B:=qB;33B<��B>ffB?�B@��BBffBC\)BDz�BF{BG�BHz�BIBK�BL��BM�BO�BP��BR{BS�
BT��BV=qBX  BYp�BZffB\  B]B^�HB`(�Ba�Bc
=BdQ�Bf{Bg�Bh��BjffBk�
Bm�BnffBo�
Bqp�BrffBt(�Bup�Bv�\Bx(�By��Bz�RB{�
B}��B
=B�  B���B��B�{B��HB��B�=qB���B��
B�Q�B��B��B�z�B�33B�{B��\B�33B�{B���B�\)B�{B��HB�p�B�  B���B��B�(�B��HB�B�=qB���B��
B�z�B�
=B�B��\B�\)B��
B���B�p�B�  B���B�p�B�(�B���B��B�Q�B�
=B���B�ffB�G�B�B��RB�p�B�{B��RB��B�ffB���B�B���B�G�B��
B��RB��B�{B��HB�B�ffB���B�B���B�33B��
B��RB�\)B��B���B��B�(�B��RB�p�B�Q�B���B��B�Q�B��B��B�Q�B�33B��B��\B�33B�{B��HB�p�B�(�B��B��
B�ffB�G�B�{BȸRB�\)B�(�B�
=BˮB�Q�B��B�  BΏ\B�33B�  B��HBхB�{B���B�B�z�B�
=B�B֣�B�G�B��
Bأ�BمB�(�BڸRB�p�B�Q�B��HB�p�B�(�B��HB߅B��B���B�G�B�B�ffB���B�\)B��B��B���B�p�B�  B�=qB���B�G�B癚B��B�z�B��HB��B陚B�{B�\B���B�33B�B�=qB�z�B���B�p�B��B�(�B��B��B�\)B�  B�ffB�RB�33B�B�{B�z�B�
=B�B��
B�Q�B��HB�G�B���B�=qB���B���B�\)B�  B�z�B���B�33B�B�Q�B��\B���B��B�{B�z�B���B�33B�B�=qB��\B���B�p�C   C (�C Q�C ��C �HC{C33CffC�RC�C
=CG�C�\CC�C{CffC��C�
C  C33C�C�C�
C�C\)C�C�RC  CG�C\)C�\C�
C{C(�CffC�RC��C  C=qC�C�RC�HC	{C	ffC	��C	C	�C
�C
ffC
��C
�RC  C33CQ�C�C��C  C�CQ�C��C�
C��C(�Cz�C��C��C{CG�CffC��C�HC{C33CffC�C��C��C=qCffC�\C�C��C(�C=qC�C�RC�HC  CG�Cz�C��CC  C33CQ�C��C��C�C(�C\)Cz�C��C�C{C=qCp�C�CC��C=qC\)Cz�C�RC�HC  CG�Cp�C�CC��C
=CG�Cz�C�\CC  C{C=qCz�C��C�C�C{C(�CffC��C�C�
C�CG�C\)C�\C��C�HC
=CG�Cz�C��C�RC  C(�CG�Cp�C��C�HC��C�C\)C��C��C�HC{C�CffC��CC�C (�C G�C ffC �C �HC ��C!33C!p�C!�\C!�C!�C"(�C"G�C"p�C"�RC"�HC#  C#(�C#p�C#��C#��C#��C$=qC$p�C$��C$�RC%  C%=qC%\)C%�C%�
C%��C&�C&\)C&��C&C&��C'=qC'p�C'�\C'��C(
=C(G�C(p�C(�\C(�
C)�C)=qC)ffC)�C)�HC*  C*=qC*z�C*�RC*�HC+
=C+\)C+��C+��C+��C,=qC,�C,�C,�HC-33C-\)C-�\C-�
C.�C.G�C.z�C.C/
=C/=qC/ffC/�C0  C0(�C0\)C0��C0�HC1(�C1\)C1�\C1C2{C2Q�C2z�C2�RC3  C3G�C3p�C3��C3�HC4(�C4p�C4��C4�
C5{C5Q�C5��C5�HC6�C6G�C6�C6�
C7�C7G�C7z�C7��C8{C8=qC8ffC8��C8�HC9(�C9z�C9��C9�
C:{C:\)C:�C:�C;�C;Q�C;�\C;�HC<(�C<p�C<��C<�
C={C=ffC=�C=�C>�C>Q�C>�\C>�HC?(�C?Q�C?�\C?C@
=C@Q�C@��C@C@��CA33CA�CACA��CB(�CBp�CB�RCC  CC33CCffCC��CC�HCD33CDp�CD��CD�
CE{CEffCE�CE�CF�CF\)CF��CF�HCG(�CGffCG��CG111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                          111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A��/A��HA��TA��#A���A�ƨAмjA���A�A�ȴA�ƨA���A�ȴA�ȴA���A�ĜA�ĜAоwA�A���A���A���A���A�A�A�A�A�ĜA�ĜA�ƨA�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��#A��;A��HA��`A��;A�ƨAоwAв-AЬAП�A�~�A�"�A�ĜA�Q�A��mA�C�A��A��`A�O�Aƙ�A�G�A�A�x�A���A���A��
A��\A�I�A���A�I�A�ƨA�=qA�O�A��A�K�A��jA���A���A�dZA�/A��A���A�|�A�A��mA�-A�1'A��A�ffA�t�A���A��A��PA�`BA�?}A�O�A���A�M�A�ZA�JA��DA��jA���A�?}A��A�PA~��A}O�A{��Ax�!Ao�
Ah�Ae��AdA�A`1AZ�AYK�AYoAX1AT��AP�HAO�AN��AI�TAG��AES�AC�AA�PA@ȴA@ffA>�yA=G�A<�A:��A9�A7G�A6bA4�!A3G�A1�mA/�PA-;dA+/A*�A)��A'�A$�\A"�DA"bA!�A!��A!K�A �+A�#A�PAdZAK�A%AVA5?A��A�mA�+A��A�RA=qA�A��AhsA;dAĜA^5A�A�wA33A��A�jA��A�+A`BA��A��AbNA�A�AO�A7LAVAJA�hA"�AA�A��A�DA��A
1AȴA~�A=qA1A�
A\)AĜA�DA(�A��Ax�AdZA&�A��Az�AI�A�A��A\)A33A�!A7LA ZA �@�S�@�@�bN@�v�@�{@�O�@��9@���@�C�@��R@��\@�=q@�&�@�Ĝ@��j@�b@�"�@��y@���@�@��@��@��@��;@�|�@�;d@�"�@��y@�M�@�(�@�!@�$�@���@�@�G�@���@�j@��@�D@�Q�@�9X@�9X@�A�@�  @�F@�P@��y@�-@�-@��`@㕁@��@��@��@���@��@��@�E�@�7L@���@ܓu@�  @��@�n�@�@�%@�(�@׮@���@Ցh@���@��@�$�@�J@�O�@ԣ�@�;d@���@��#@���@�bN@Χ�@�^5@���@�bN@�I�@��@˥�@�t�@˅@�t�@�|�@ˍP@˕�@˝�@˥�@�|�@�o@�ȴ@�ff@Ɂ@ȼj@ȋD@�A�@��
@�\)@��@Ƈ+@�V@�?}@�l�@�V@��@��`@�1@�|�@�dZ@���@�ȴ@�ȴ@�ȴ@���@�5?@�{@�`B@���@���@�z�@�(�@��w@���@���@�?}@���@�bN@�(�@�  @���@���@���@���@��@�t�@��@�t�@���@�M�@��@��^@�`B@���@�Ĝ@�z�@�z�@�z�@�I�@���@��R@�V@�5?@�@��@���@���@�G�@��`@���@�z�@�Z@�1@���@���@�^5@�$�@���@�  @�K�@���@�$�@��@��-@�p�@�G�@�7L@�z�@�|�@�5?@��T@���@���@�E�@�J@���@���@��w@�=q@��@���@��7@�&�@���@���@��@�r�@� �@��w@��@��P@�l�@�S�@�C�@�33@��@��R@���@�v�@�ff@�E�@���@�G�@���@��/@��j@���@�j@�Q�@�1@�"�@��H@��@��\@�V@��@��-@�x�@�&�@��D@�1@��@��P@�;d@�@���@��+@�M�@��@�{@�J@�@��@��#@���@�`B@�?}@�V@���@��D@�j@��@�ƨ@�t�@���@�~�@�{@��-@�7L@�Ĝ@��u@��@�j@�(�@���@��P@�l�@�C�@�"�@���@�v�@�E�@���@��h@�`B@�/@���@��j@���@�z�@�I�@��@�l�@�
=@���@�M�@�@���@�hs@��@�Z@��@�ƨ@�;d@�"�@�o@�@���@��R@���@��\@�v�@�-@��^@���@�`B@�V@��@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��;A��;A��;A��/A��#A��HA��/A��HA��`A��`A��;A��HA��mA��/A��
A���A���A���A�ĜA���A�A���AмjAиRA�A�ĜAмjA�ƨA���A�ĜA�ƨA���A�ƨA�ȴA���A�ȴA���A���A���A���A�ƨA���A�ȴA�ƨA���A�ȴA�ȴA���A�ȴA�ƨA���A���A���A���A�ȴA�ȴA���A�ƨA�ƨA�ƨA���AмjA�ƨA�ƨA�ƨA�ĜA���AоwA���AоwAмjA���A���Aк^A�ĜA���A���A�ĜA�A���A�ĜA���AоwA�ĜAоwAоwA�AмjAоwA�AоwAоwA�AоwAоwA�A���A���A�ĜAоwA���A�AоwA�A�ĜA���A���A�ƨA���A���A�ƨA�ĜAоwA�ĜA���A�A�ĜA���A���A�ƨA�A���A�ƨA�A�ĜA�ĜA���A�ƨA�ƨA�A���A�ĜA�A���A�A�ƨA�ƨA�ĜA�ĜA�ȴA�ĜA�A�ƨA�ĜA�A�ƨA�ȴA�ĜA�ĜA���A�ĜA�ƨA�ȴA�ƨA�ĜA�ȴA�ĜA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A���A���A���A���A���A�ȴA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A��
A���A��
A��#A��
A��
A��#A��#A��
A��#A��;A��#A��#A��/A��HA��/A��#A��;A��HA��/A��/A��;A��TA��HA��TA��`A��mA��mA��`A��mA��A��A��HA���A���A���A�ƨA�ƨA�ȴA�ĜA�A�A�ƨA�ĜAоwAмjAжFAв-AжFAжFAв-Aа!Aв-AЬAЮAа!AЬAЮAа!AЩ�AЩ�AЮAЬAЧ�AЧ�AЩ�AЬAУ�AП�AП�AП�AЙ�AЛ�AП�AЗ�AЗ�AЙ�AЙ�AЕ�AЕ�AЇ+A�jA�ZA�G�A�=qA�33A�/A�/A�&�A��A��A�oA�JA�
=A�  A��mA��A���AϸRAϥ�A�z�A�r�A�hsA�XA�M�A�O�A�Q�A�O�A�I�A�E�A�C�A�A�A�-A�VA���A��/A�Aδ9Aΰ!AΧ�AΕ�A΁A�ffA�ZA�C�A�5?A�$�A��A�oA�A��A�ĜA͕�A�l�A��A���A̼jA�n�A�5?A���A�v�A�K�A� �A��AʸRAʑhA�Q�A�$�A��A���Aɏ\A�bNA�S�A�;dA�-A�$�A�1A��
A�A�AǗ�A��
AƝ�A�bNA�M�A�G�A��A�A�A�A��A��`Aŉ7A�E�A� �A���AĮA�|�A�ZA��A��mA£�A�jA�ZA�VA�K�A�A�A�;dA�/A�bA���A��TA�ƨA���A�bNA�A���A���A���A�\)A�  A���A�M�A��A��+A�M�A���A���A�I�A��A��A��#A��A���A���A���A��9A��uA��A��hA� �A��/A���A�;dA�ƨA�|�A�dZA�I�A�9XA�5?A�+A�"�A�"�A��A�{A�JA��
A�t�A�=qA��A��A��FA��hA�ffA�O�A�"�A��-A�t�A�O�A�&�A��A���A��hA�M�A��;A���A�O�A�A��mA���A��FA��A���A�n�A�S�A�;dA�9XA��A���A��A���A�t�A��A���A�ĜA���A��A�p�A�dZA�Q�A�1'A��A���A��
A���A��\A�v�A�K�A�"�A���A��9A�jA�ZA�K�A�=qA�+A� �A��A��A�A��#A��!A���A��uA�z�A�`BA�M�A�$�A��A��!A���A�r�A�;dA�  A��mA���A�ȴA��A���A��\A��PA��DA�~�A�^5A�S�A�C�A�33A�/A�(�A�"�A��A�bA��yA��TA�ȴA��-A��!A���A��DA�=qA��A��9A�~�A�(�A��-A�7LA� �A�A��A��/A���A��wA���A��DA�p�A�dZA�Q�A� �A���A��A���A�t�A�7LA��A��^A��PA�v�A�dZA�M�A�?}A�9XA�+A�%A��;A�ȴA��PA�7LA���A���A�S�A���A��9A���A��A�M�A��A���A��TA���A�ƨA���A��^A��A���A���A��7A�jA�Q�A�C�A�;dA�33A�$�A�bA�A��A��A��TA�ĜA�r�A��yA���A���A���A���A��7A�x�A�v�A�l�A�bNA�C�A�VA���A��A��;A���A�5?A��TA�z�A��wA���A�hsA�+A��
A�&�A�=qA��`A��
A���A�p�A�&�A���A���A���A��DA�~�A�r�A�5?A�A���A���A���A���A�v�A�dZA�K�A�I�A�?}A�A�A� �A�bA�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                          111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��/A��HA��TA��#A���A�ƨAмjA���A�A�ȴA�ƨA���A�ȴA�ȴA���A�ĜA�ĜAоwA�A���A���A���A���A�A�A�A�A�ĜA�ĜA�ƨA�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��#A��;A��HA��`A��;A�ƨAоwAв-AЬAП�A�~�A�"�A�ĜA�Q�A��mA�C�A��A��`A�O�Aƙ�A�G�A�A�x�A���A���A��
A��\A�I�A���A�I�A�ƨA�=qA�O�A��A�K�A��jA���A���A�dZA�/A��A���A�|�A�A��mA�-A�1'A��A�ffA�t�A���A��A��PA�`BA�?}A�O�A���A�M�A�ZA�JA��DA��jA���A�?}A��A�PA~��A}O�A{��Ax�!Ao�
Ah�Ae��AdA�A`1AZ�AYK�AYoAX1AT��AP�HAO�AN��AI�TAG��AES�AC�AA�PA@ȴA@ffA>�yA=G�A<�A:��A9�A7G�A6bA4�!A3G�A1�mA/�PA-;dA+/A*�A)��A'�A$�\A"�DA"bA!�A!��A!K�A �+A�#A�PAdZAK�A%AVA5?A��A�mA�+A��A�RA=qA�A��AhsA;dAĜA^5A�A�wA33A��A�jA��A�+A`BA��A��AbNA�A�AO�A7LAVAJA�hA"�AA�A��A�DA��A
1AȴA~�A=qA1A�
A\)AĜA�DA(�A��Ax�AdZA&�A��Az�AI�A�A��A\)A33A�!A7LA ZA �@�S�@�@�bN@�v�@�{@�O�@��9@���@�C�@��R@��\@�=q@�&�@�Ĝ@��j@�b@�"�@��y@���@�@��@��@��@��;@�|�@�;d@�"�@��y@�M�@�(�@�!@�$�@���@�@�G�@���@�j@��@�D@�Q�@�9X@�9X@�A�@�  @�F@�P@��y@�-@�-@��`@㕁@��@��@��@���@��@��@�E�@�7L@���@ܓu@�  @��@�n�@�@�%@�(�@׮@���@Ցh@���@��@�$�@�J@�O�@ԣ�@�;d@���@��#@���@�bN@Χ�@�^5@���@�bN@�I�@��@˥�@�t�@˅@�t�@�|�@ˍP@˕�@˝�@˥�@�|�@�o@�ȴ@�ff@Ɂ@ȼj@ȋD@�A�@��
@�\)@��@Ƈ+@�V@�?}@�l�@�V@��@��`@�1@�|�@�dZ@���@�ȴ@�ȴ@�ȴ@���@�5?@�{@�`B@���@���@�z�@�(�@��w@���@���@�?}@���@�bN@�(�@�  @���@���@���@���@��@�t�@��@�t�@���@�M�@��@��^@�`B@���@�Ĝ@�z�@�z�@�z�@�I�@���@��R@�V@�5?@�@��@���@���@�G�@��`@���@�z�@�Z@�1@���@���@�^5@�$�@���@�  @�K�@���@�$�@��@��-@�p�@�G�@�7L@�z�@�|�@�5?@��T@���@���@�E�@�J@���@���@��w@�=q@��@���@��7@�&�@���@���@��@�r�@� �@��w@��@��P@�l�@�S�@�C�@�33@��@��R@���@�v�@�ff@�E�@���@�G�@���@��/@��j@���@�j@�Q�@�1@�"�@��H@��@��\@�V@��@��-@�x�@�&�@��D@�1@��@��P@�;d@�@���@��+@�M�@��@�{@�J@�@��@��#@���@�`B@�?}@�V@���@��D@�j@��@�ƨ@�t�@���@�~�@�{@��-@�7L@�Ĝ@��u@��@�j@�(�@���@��P@�l�@�C�@�"�@���@�v�@�E�@���@��h@�`B@�/@���@��j@���@�z�@�I�@��@�l�@�
=@���@�M�@�@���@�hs@��@�Z@��@�ƨ@�;d@�"�@�o@�@���@��R@���@��\@�v�@�-@��^@���@�`B@�V@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��;A��;A��;A��/A��#A��HA��/A��HA��`A��`A��;A��HA��mA��/A��
A���A���A���A�ĜA���A�A���AмjAиRA�A�ĜAмjA�ƨA���A�ĜA�ƨA���A�ƨA�ȴA���A�ȴA���A���A���A���A�ƨA���A�ȴA�ƨA���A�ȴA�ȴA���A�ȴA�ƨA���A���A���A���A�ȴA�ȴA���A�ƨA�ƨA�ƨA���AмjA�ƨA�ƨA�ƨA�ĜA���AоwA���AоwAмjA���A���Aк^A�ĜA���A���A�ĜA�A���A�ĜA���AоwA�ĜAоwAоwA�AмjAоwA�AоwAоwA�AоwAоwA�A���A���A�ĜAоwA���A�AоwA�A�ĜA���A���A�ƨA���A���A�ƨA�ĜAоwA�ĜA���A�A�ĜA���A���A�ƨA�A���A�ƨA�A�ĜA�ĜA���A�ƨA�ƨA�A���A�ĜA�A���A�A�ƨA�ƨA�ĜA�ĜA�ȴA�ĜA�A�ƨA�ĜA�A�ƨA�ȴA�ĜA�ĜA���A�ĜA�ƨA�ȴA�ƨA�ĜA�ȴA�ĜA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A���A���A���A���A���A�ȴA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A��
A���A��
A��#A��
A��
A��#A��#A��
A��#A��;A��#A��#A��/A��HA��/A��#A��;A��HA��/A��/A��;A��TA��HA��TA��`A��mA��mA��`A��mA��A��A��HA���A���A���A�ƨA�ƨA�ȴA�ĜA�A�A�ƨA�ĜAоwAмjAжFAв-AжFAжFAв-Aа!Aв-AЬAЮAа!AЬAЮAа!AЩ�AЩ�AЮAЬAЧ�AЧ�AЩ�AЬAУ�AП�AП�AП�AЙ�AЛ�AП�AЗ�AЗ�AЙ�AЙ�AЕ�AЕ�AЇ+A�jA�ZA�G�A�=qA�33A�/A�/A�&�A��A��A�oA�JA�
=A�  A��mA��A���AϸRAϥ�A�z�A�r�A�hsA�XA�M�A�O�A�Q�A�O�A�I�A�E�A�C�A�A�A�-A�VA���A��/A�Aδ9Aΰ!AΧ�AΕ�A΁A�ffA�ZA�C�A�5?A�$�A��A�oA�A��A�ĜA͕�A�l�A��A���A̼jA�n�A�5?A���A�v�A�K�A� �A��AʸRAʑhA�Q�A�$�A��A���Aɏ\A�bNA�S�A�;dA�-A�$�A�1A��
A�A�AǗ�A��
AƝ�A�bNA�M�A�G�A��A�A�A�A��A��`Aŉ7A�E�A� �A���AĮA�|�A�ZA��A��mA£�A�jA�ZA�VA�K�A�A�A�;dA�/A�bA���A��TA�ƨA���A�bNA�A���A���A���A�\)A�  A���A�M�A��A��+A�M�A���A���A�I�A��A��A��#A��A���A���A���A��9A��uA��A��hA� �A��/A���A�;dA�ƨA�|�A�dZA�I�A�9XA�5?A�+A�"�A�"�A��A�{A�JA��
A�t�A�=qA��A��A��FA��hA�ffA�O�A�"�A��-A�t�A�O�A�&�A��A���A��hA�M�A��;A���A�O�A�A��mA���A��FA��A���A�n�A�S�A�;dA�9XA��A���A��A���A�t�A��A���A�ĜA���A��A�p�A�dZA�Q�A�1'A��A���A��
A���A��\A�v�A�K�A�"�A���A��9A�jA�ZA�K�A�=qA�+A� �A��A��A�A��#A��!A���A��uA�z�A�`BA�M�A�$�A��A��!A���A�r�A�;dA�  A��mA���A�ȴA��A���A��\A��PA��DA�~�A�^5A�S�A�C�A�33A�/A�(�A�"�A��A�bA��yA��TA�ȴA��-A��!A���A��DA�=qA��A��9A�~�A�(�A��-A�7LA� �A�A��A��/A���A��wA���A��DA�p�A�dZA�Q�A� �A���A��A���A�t�A�7LA��A��^A��PA�v�A�dZA�M�A�?}A�9XA�+A�%A��;A�ȴA��PA�7LA���A���A�S�A���A��9A���A��A�M�A��A���A��TA���A�ƨA���A��^A��A���A���A��7A�jA�Q�A�C�A�;dA�33A�$�A�bA�A��A��A��TA�ĜA�r�A��yA���A���A���A���A��7A�x�A�v�A�l�A�bNA�C�A�VA���A��A��;A���A�5?A��TA�z�A��wA���A�hsA�+A��
A�&�A�=qA��`A��
A���A�p�A�&�A���A���A���A��DA�~�A�r�A�5?A�A���A���A���A���A�v�A�dZA�K�A�I�A�?}A�A�A� �A�bA�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                          111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��BK�BK)BK�BK�BJXBJXBIRBJ#BI�BJ#BJ�BJ#BJXBJXBJ#BI�BJ#BI�BI�BI�BIRBIRBI�BIRBI�BIRBI�BI�BJ#BI�BI�BI�BJXBJ�BJ�BJ�BJXBI�BJ#BI�BJ�BJ#BJ#BI�BI�BI�BI�BI�BIRBI�BIRBI�BIRBK�BL�BM�BPBQ�BT�B\]BhsBo Br�Bs�Bt�By>B�{B�xB�oB�+B��B��B�B��B��B�B�B�;B�lB�B&B,B:^BB'BEmBG�BO�BT�BQNBK�BF?B?�B2-B1�B&�B �BoB
�B�B��B�oB��BɺB��B�YB��B��BqBZB2�B(�B!bBeBFB
�>B
�B
��B
t�B
g�B
b�B
aB
[�B
NB
B�B
5�B	�xB	چB	��B	�wB	�0B	��B	~(B	z�B	v+B	ncB	_pB	T�B	R�B	L�B	@�B	A B	6zB	2aB	*eB	+B	)*B	)�B	#�B	!�B	�B	B	�B	�B	:B	@B	bB	
�B		B	�B	1B	B	�B	�B	VB	�B	�B	eB	xB	#B	7�B	JXB	Q�B	jKB	l"B	kB	n/B	t�B	xB	z�B	��B	�PB	��B	��B	�"B	��B	��B	��B	��B	��B	�LB	��B	��B	��B	�LB	�FB	��B	��B	ŢB	�mB	��B	�pB	�6B	�^B	�BB	�TB	�aB	��B	�,B	ӏB	�gB	��B	�)B	��B	�jB	�BB	�B	�HB	�&B	��B	��B	��B	�B	��B	�B	��B	�B	�sB	�>B	��B	�B	�B	�>B	�KB	�B	��B	��B	�B	��B	�QB	�B	�B	�B	��B	��B	��B	�mB	�B	�B	�B	�B	�fB	�B	�B	�B	�B	�B	�B	�"B	��B	��B	�WB	�B	�"B	��B	�]B	�B	��B	�B	�|B	��B	�B	�B	�|B	�B	��B	��B	�B	�MB	�|B	�B	�B	�MB	��B	��B	�`B	��B	�lB	��B	��B	��B	�2B	�8B	�B	��B	�>B	��B	�PB	�(B	��B
  B
  B
AB
B
�B
AB	��B	�cB
 �B
;B
{B
oB
B
 iB
  B
uB
B
 4B	��B	�xB	�>B	�TB	�%B	��B	�B
 �B
�B
%B
+B
�B
�B
�B
fB
	7B
	lB
	7B
	�B
�B
~B
�B
�B
�B
B
�B
DB

�B
PB
�B
�B
	B
%B
�B
B
{B
�B
{B
GB
�B
�B
{B
{B
�B
MB
�B
SB
�B
�B
_B
%B
�B
SB
%B
%B
�B
YB
YB
%B
�B
�B
1B
fB
	�B
�B
�B
�B
.B
hB
�B
�B
�B
�B
~B
�B
!bB
#�B
"�B
"4B
"4B
!�B
!�B
!�B
"hB
!bB
!�B
!�B
!-B
!�B
$tB
%zB
$@B
#nB
#�B
$�B
#nB
$B
$�B
$B
$�B
$tB
$tB
#�B
#�B
"4B
�B
~B
�B
�B
$B
&B
%�B
%B
-B
*�B
,�B
+6B
*�B
,=B
+�B
,=B
,B
,B
-CB
-�B
-�B
.�B
.�B
.�B
.�B
/B
0�B
0�B
2aB
2�B
33B
5�B
6�B
9$B
8�B
9$B
9XB
9�B
9�B
9�B
9�B
;dB
9�B
9�B
:^B
9�B
:�B
:�B
:�B
;dB
<�B
=�B
>BB
>wB
>�B
?}B
?�B
@OB
A B
AUB
AUB
A�B
A�B
A�B
A�B
B[B
B�B
C-B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
GzB
G�B
H�B
H�B
I�B
J#B
J�B
J�B
JXB
K)B
K�B
K�B
K�B
K�B
L0B
LdB
L�B
MB
NB
N<B
NpB
NpB
N�B
OvB
OBB
OBB
OvB
OBB
QB
P�B
QNB
QNB
Q�B
R B
R B
R�B
S�B
S�B
TaB
T�B
T�B
T�B
T�B
UgB
U�B
U�B
U�B
U�B
V�B
W
B
WsB
XEB
XEB
X�B
YG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BM�BM�BK^BJ�BK�BI�BMBL�BJ�BJ�BL0BLdBJXBL�BK�BIBI�BL0BMjBG�BJ#BJ#BH�BJXBI�BI�BK^BH�BJ#BJXBJ#BIBJ�BJXBIRBK)BI�BJ�BI�BJXBK�BI�BI�BK^BIRBJ#BJ�BI�BJ�BK^BI�BI�BJXBIRBJXBJ�BIRBI�BI�BH�BJ�BJXBF?BIRBJ�BG�BHKBMjBIRBI�BJ�BH�BH�BJ�BGzBJXBJ�BIBJ#BJ�BIBJ�BJ�BHKBJ�BK)BHKBJ#BI�BHKBI�BI�BHKBJ#BI�BHKBI�BI�BH�BJ#BI�BH�BJ�BH�BH�BJ�BI�BH�BJ�BJ#BHKBI�BJXBG�BI�BIRBH�BJ#BJ#BH�BI�BJXBHKBJXBI�BH�BJ�BIBH�BJXBJ�BIBIBJ�BJXBI�BIBJ�BJXBH�BJ#BJ�BIBJ#BJ�BIRBH�BJ�BJ#BH�BJ�BI�BH�BJ#BJXBH�BJXBK^BH�BJ�BK�BK^BI�BJ�BJ�BIRBJXBK)BHKBI�BJ�BJ#BJXBK�BM6BJ#BI�BK)BH�BIBJXBI�BI�BK�BK)BIBH�BK)BIRBH�BK)BJXBIRBJ#BJ#BIBJ�BJ#BH�BI�BJXBH�BI�BJXBI�BK)BJ�BJ�BI�BI�BK)BJ�BH�BJ�BK^BH�BJXBJ#BH�BIBJ#BJ#BIBH�BJ�BJ#BH�BJ�BJ�BIBIBJ�BI�BH�BH�BJ�BI�BH�BJ�BIBI�BJ�BJ#BHBH�BJXBI�BHKBI�BJ�BH�BH�BI�BI�BH�BIRBJ�BI�BH�BJ#BJXBH�BH�BJ�BI�BH�BIRBJXBJ#BH�BIRBJ�BIRBH�BI�BJ#BJXBJ�BL0BM�BL0BK�BM�BM�BL�BMBNBL�BK�BOBO�BOvBNpBPHBQBP}BPBQ�BR�BQNBQBR�BT�BS�BS[BS�BV�BU�BVBV�B[WBa�BcTBe�Bg�Bh>Bf2BiDBkBi�BiBn�BqBp�Bq�BsBq�Bq�Bs�Bs�BrBt�Bs�Br�Bt�BsMBr�BtTBt�BsBsMBt�Bt�BsMBr�Bu%Bu�Bt�Bt�Bu�BuZBtBu�Bu�Bt�Bu%Bv`Bt�BxlB}VB�B�4B��B�GB��B�;B�GB��B�{B��B�SB��B��B��B��B�lB�B�"B�B��B� B��B��B��B� B�:B�uB��B��B�hB�MB�_B��B�qB�kB�qB��B��B�=B��B��B��B��B�\B�bB��B��B��B�:B��B��B��B�tB�}B��B��B��B��B�OB��B��B��B�6B��B��B�0B��B�^B�B�B��B�^B�FB�nB��B�B�XB�B�zB��B��B�tBŢB��B��B�BŢB�B�tB��B̘BϫB�B�gB�BҽB�vB�B�
B�TB�BܒB�jB��B��BܒB�jB�B�yBݘB�B�TB��B�BBݘB��B�8B�B�B�B�B�B��B��BfB�BSB�B�B{BBMBuBB�B�B�B�B�B �B%�B0�B+B'�B'�B(�B%�B&�B'B$�B$�B%�B%�B-CB5tB/OB,qB1�B2aB1�B49B0�B7B?B8RB7�B8RB:^B;0B<jBB�BGBD3BJ#BA�BA�B@B?�B?BA�B>�BD3B@�B>wBF�B?�B?�B?}BNBNpBD�BC-BC�BFBC�BB�BDgBF�BD�BGBK)BGEBF�BH�BIBIBM6BV�BNBM6BMBO�BL�BN<BN�BM6BQNBU�BT�BQ�BPBR BRTBT,BV�B[�BR�BRTBT�BU�BV9BO�BPBN�BUgBMjBNpBM6BK�BM6BOBMjBN�BHBIBJ#BI�BF�BJXBK�BDgBGzBD�BA�BA BE�BK)BFB@�B:�BCaBZ�B;0B5�B3�B6zB2-B2�B49B0�B2aB0!B0�B-�B33B/�B2-B1'B0�B49B2�B1�B.IB)_B(XB(�B'�B$�B%B(�B%B#B'�B(�B!�B �B-CB"�BFB�B+B�B�B�B�B�BVBVB�BBJB
=B
�B~BB�B�B�BBMB�B �B�]B�B�BxB?�B�;B�B��B�B��B��B��B�B�B��B��B��B��B�B��B�B�/B�B�/BٴB��B��BܒB��B�EB��B�<B��B��B��B�9B�B�'B��B�@B�:B�$B�IB��B�!B�YB�$B��B�SB�7B��B��B��B��B�~B�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                          444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                          444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2022011422354920220114223549IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022011604011220220116040112QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022011604011220220116040112QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365720220126093657IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295920220204232959IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295920220204232959IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295920220204232959IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                