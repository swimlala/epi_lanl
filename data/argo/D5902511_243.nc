CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2023-01-05T02:01:46Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  T�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  Z�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  rD   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  x0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  <   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` 5x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   5�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ;�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   A�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T G�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   H,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   H4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   H<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   HD   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � HL   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   H�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   H�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    H�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        I   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        I   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       I    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    I(Argo profile    3.1 1.2 19500101000000  20230105020146  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_243                 6810_008521_243                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�
�(9.@�
�(9.11  @�
�U�=@�
�U�=@2ySy���@2ySy����d�ؘ����d�ؘ���11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@@  @z�H@�  @�G�@�G�A�AG�A\)A+�AAG�A`��A\)A�  A�Q�A���A�Q�A�  A�  A�A��B  B�
B�B�
B(  B/�
B7�
B?�
BG�
BP  BW�
B_�
Bh  Bo�
Bw�B�{B�(�B�{B�{B��B�{B�=qB�{B�  B�  B��
B��
B�  B��B��B��
B�B�  B�(�B�(�B�  B�  B�{B�{B��B��B��B�  B��B��B��B��
C   C  C�C�C
=C

=C  C
=C{C
=C
=C  C��C�C��C
=C   C!��C$  C&{C(  C*
=C,
=C.  C0  C1��C4  C6  C8
=C:  C;��C=��C@  CB
=CD{CF
=CG��CJ
=CL
=CN  CP  CQ��CS��CV  CX{CZ  C[�C]��C`
=Cb
=Cd
=Cf
=Cg��Cj  Cl
=Cn
=Co��Cq��Ct
=Cv
=Cw��Cz
=C{��C}�C�  C�C�  C���C�
=C�C�  C�  C�  C�  C�  C�
=C�C�  C�  C�C�  C���C���C�  C�C�  C�C�  C���C�  C�  C�  C�  C�  C�  C�  C�  C���C���C�  C�  C�  C���C���C���C�  C���C���C�  C���C�  C�  C�C�
=C�  C�  C�
=C�  C���C�C���C���C�C�C���C���C���C�  C�  C�  C���C�  C���C���C�  C�  C���C�  C�  C���C�C�C���C�C���C���C�C�C�  C���C���C���C���C�  C�  C���C�C�C���C���C�  C���C���C�  C�
=C�C�  C�  C�  C�C�C�  C�  C�
=C�C�  C�  C�C�
=C�C���C�  C�  C�  C�  C�  C���C���C���C�  C�  C�  D �D ��D  D�D�D��D�D� D  D}qD  D}qD�qD� D�qD� D�D��D	�D	�D
D
��D�D� D�qD� D  D� D�qD}qD  D� D�qD� D�D� D  D��D�D��DD��D  D� D�D�DD��D�D��D  D� D  D� D  D��D  D� D  D� D�qD}qD�qD� D   D }qD!  D!� D"  D"��D#�D#��D$�D$�D%  D%� D&  D&}qD'  D'}qD'�qD(� D)�D)}qD)�qD*��D+D+��D,  D,� D-  D-� D.�D.�D/�D/z�D/�qD0}qD0�qD1� D1�qD2� D3  D3}qD4  D4��D5D5�D6  D6}qD7�D7��D7�qD8z�D8�qD9� D:  D:}qD;  D;� D<  D<}qD<��D=� D>  D>}qD?�D?� D?�qD@� D@�qDA}qDA�qDB� DC  DC��DDDD�DE  DEz�DF  DF�DG�DG� DH  DH}qDI�DI��DJ�DJ� DJ��DK}qDL  DL}qDL��DM}qDM�qDNz�DN��DO}qDO�qDP}qDP�qDQ��DR�DR��DS  DS}qDT�DT� DU  DU}qDU�qDV� DW�DW� DW�qDX��DY�DY��DZ  DZ}qDZ�qD[}qD\�D\��D]  D]}qD^  D^� D_  D_}qD_�qD`}qDa  Da� Db  Db� Db�qDc� Dd�Dd� De  De��DfDf��Df��Dgz�Dg�qDh��Dh�qDi� Dj  Dj}qDk  Dk}qDk�qDl}qDl�qDmz�Dn  Dn��DoDo��Do�qDp� Dp�qDqz�Dq�qDr��Ds�Ds��DtDt��Du�Du� Du�qDvz�Dw  Dw� Dx  Dx��Dx�qDy��Dz�Dz� D{�D{� D|  D|}qD|�qD}}qD~  D~� D~�qD��D�HD�@ D�~�D���D�  D�>�D�~�D�� D�  D�>�D�~�D�� D�HD�B�D��HD��HD�  D�AHD�� D��qD���D�AHD��HD��HD�HD�@ D���D�D�  D�@ D���D�D�  D�>�D�� D��HD�  D�>�D�~�D�� D��D�@ D�� D�� D���D�@ D�� D�� D�  D�C�D��HD�� D�  D�@ D��HD�� D�HD�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�>�D�~�D�� D�  D�AHD��HD��HD�  D�@ D�� D��HD��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?\)?L��?�=q?�Q�?Ǯ@   @��@�R@8Q�@G�@c�
@s33@��
@���@�Q�@��@�\)@�Q�@��@�{@�@�G�@�=q@�33A ��A�
A�A{A�\AffA��A   A%�A*�HA.{A4z�A8Q�A>{AA�AFffAL��AP  AVffAZ=qA_\)Ae�AhQ�An{Ar�\Aw
=A|��A\)A��\A���A��RA���A�(�A�p�A���A��\A�p�A�\)A�G�A�z�A�ffA���A��
A�p�A�  A��\A�z�A��A���A�(�A��RA���A��
A�A�  A�33A���AǮAə�A�(�A�\)AУ�A��
A�{A�Q�A�33A���A�Q�A�=qA���A�\)A�G�A�z�A�{A���A�A�p�A���A�33A���B   BG�B{B�B��B�B\)Bz�B	��B33B(�BG�B�HB�
B��B�\B�Bz�B=qB33BQ�BB�\B(�B�B=qB�
B ��B"{B#33B$(�B%B&�RB(  B)G�B*{B+�B,��B.{B/33B0Q�B1B2�RB4Q�B5G�B6�RB8  B8��B:ffB;�B<z�B>{B?33B@(�BA�BB�RBD  BE��BFffBG�
BIG�BJ{BK�BL��BM�BO\)BP(�BQp�BR�RBS�BT��BV=qBW
=BX��BY��BZ�RB\(�B]�B^=qB_�B`��BaBc33Bd  Bep�Bf�RBg�Bh��Bj=qBk
=Bl��BmBn�RBpQ�Bqp�BrffBt  Bt��Bv{Bw�Bxz�ByB{33B|  B}p�B~�RB�B���B��B���B�ffB���B�p�B�=qB��RB�\)B�  B�Q�B��B��B�(�B���B�G�B�  B���B�
=B�B�z�B���B��B�(�B���B�p�B��B�ffB�33B��
B�=qB���B��B�{B���B��B�  B���B�G�B�B��\B�33B���B�Q�B���B�\)B�{B���B�33B��B�Q�B�
=B��B�{B���B�p�B��
B��\B�G�B��B�ffB��HB�\)B�=qB���B��B��B�ffB���B��B�=qB��RB��B�  B��\B�\)B�B�z�B��B���B�Q�B���B�p�B�(�B��HB�G�B��
B��RB��B��B�z�B�
=B��B�(�B���B�\)B�{B��RB��B��
B�z�B���B��B�Q�B��RB��B�(�B��\B�G�B�  B�z�B���B�B�=qBĸRBŅB��BƏ\B�G�BǮB�Q�B�
=B�p�B�  BʸRB�\)B��
B�Q�B��BͮB�{B��HBυB��BЏ\B�G�B��
B�=qB���Bә�B�  B�z�B�33B�B�{BָRB�p�B��B�ffB�
=B�B�(�B��HBۙ�B�(�B܏\B�G�B��
B�ffB��HB�B�Q�B���B�\)B�(�B�RB�33B��B�\B�
=B�B�ffB���B�p�B�=qB��B��B�  B�z�B�
=B뙚B�Q�B���B�B�{B���B�p�B��B�RB�p�B�  B�\B�G�B�  B�z�B��B��B�z�B�
=B��
B�ffB��HB��B�Q�B���B��B�Q�B���B�\)B�(�B��HB�\)C {C p�C �C{Cp�C�C�CffC�C{C\)C��C
=CG�C��C  C=qC��C��C33C�\C��C33C�C�C(�Cz�C�HC	{C	�C	��C
{C
p�C
��C  CffC�RC��CQ�C�C�HC33C��C�
C(�C�\C�
C{Cz�C��C{CQ�CC  C=qC��C��C(�C�C�
C�C\)CC{CQ�C�C  C33C��C�C�Cp�C�
C
=CQ�C�C  C=qC��C�HC(�C�C�HC�Cp�C��C(�CffC�C
=CffC��C�HCG�C�\C��C
=CffC�RC  C33C�\C�C �C ffC �RC!{C!Q�C!�\C!�HC"G�C"z�C"�RC#
=C#Q�C#�\C#�RC$
=C$Q�C$�\C$�C$�C%�C%\)C%�C%��C%�HC&{C&33C&G�C&z�C&��C&�RC&C&�C'{C'=qC'=qC'\)C'�C'�C'�RC'�HC(
=C((�C(33C(Q�C(p�C(��C(C(�HC(�C){C)G�C)ffC)z�C)��C)C)�C*
=C*�C*=qC*p�C*��C*�C*��C+  C+33C+=qC+\)C+z�C+��C+��C,  C,(�C,=qC,\)C,z�C,�RC,��C,�HC-
=C-=qC-ffC-z�C-�\C-C-��C.
=C.�C.=qC.z�C.�C.��C.�HC/  C/33C/ffC/�\C/��C/C0  C033C0Q�C0z�C0��C0�RC0�C1�C133C1\)C1��C1�RC1�
C2  C2=qC2p�C2�\C2�C2�
C3{C3G�C3\)C3�C3�C3��C4�C4=qC4\)C4��C4��C5  C5{C5G�C5�C5�C5C5��C633C6ffC6�C6�C6�
C7{C7G�C7z�C7��C7C7�C8�C8\)C8�\C8��C8��C9  C933C9ffC9�\C9�C9�
C:�C:G�C:p�C:�\C:�
C;{C;=qC;\)C;�\C;C<
=C<=qC<ffC<�C<��C=
=C=33C=\)C=�C=�RC>  C>=qC>\)C>�\C>�RC>��C?33C?ffC?�\C?�RC?�HC@(�C@\)C@�C@�C@�CA(�CAQ�CAp�CA��CA�HCB{CBQ�CB�CB��CB��CB��CC33CCp�CC��CC��CC�CD(�CDffCD��CD��CD�CE{CEG�CEz�CE�RCE�CF�CF33CFp�CF�RCF�HCG
=CG(�CG\)CG��CG��CH
=CH33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?��@�\@@  @z�H@�  @�G�@�G�A�AG�A\)A+�AAG�A`��A\)A�  A�Q�A���A�Q�A�  A�  A�A��B  B�
B�B�
B(  B/�
B7�
B?�
BG�
BP  BW�
B_�
Bh  Bo�
Bw�B�{B�(�B�{B�{B��B�{B�=qB�{B�  B�  B��
B��
B�  B��B��B��
B�B�  B�(�B�(�B�  B�  B�{B�{B��B��B��B�  B��B��B��B��
C   C  C�C�C
=C

=C  C
=C{C
=C
=C  C��C�C��C
=C   C!��C$  C&{C(  C*
=C,
=C.  C0  C1��C4  C6  C8
=C:  C;��C=��C@  CB
=CD{CF
=CG��CJ
=CL
=CN  CP  CQ��CS��CV  CX{CZ  C[�C]��C`
=Cb
=Cd
=Cf
=Cg��Cj  Cl
=Cn
=Co��Cq��Ct
=Cv
=Cw��Cz
=C{��C}�C�  C�C�  C���C�
=C�C�  C�  C�  C�  C�  C�
=C�C�  C�  C�C�  C���C���C�  C�C�  C�C�  C���C�  C�  C�  C�  C�  C�  C�  C�  C���C���C�  C�  C�  C���C���C���C�  C���C���C�  C���C�  C�  C�C�
=C�  C�  C�
=C�  C���C�C���C���C�C�C���C���C���C�  C�  C�  C���C�  C���C���C�  C�  C���C�  C�  C���C�C�C���C�C���C���C�C�C�  C���C���C���C���C�  C�  C���C�C�C���C���C�  C���C���C�  C�
=C�C�  C�  C�  C�C�C�  C�  C�
=C�C�  C�  C�C�
=C�C���C�  C�  C�  C�  C�  C���C���C���C�  C�  C�  D �D ��D  D�D�D��D�D� D  D}qD  D}qD�qD� D�qD� D�D��D	�D	�D
D
��D�D� D�qD� D  D� D�qD}qD  D� D�qD� D�D� D  D��D�D��DD��D  D� D�D�DD��D�D��D  D� D  D� D  D��D  D� D  D� D�qD}qD�qD� D   D }qD!  D!� D"  D"��D#�D#��D$�D$�D%  D%� D&  D&}qD'  D'}qD'�qD(� D)�D)}qD)�qD*��D+D+��D,  D,� D-  D-� D.�D.�D/�D/z�D/�qD0}qD0�qD1� D1�qD2� D3  D3}qD4  D4��D5D5�D6  D6}qD7�D7��D7�qD8z�D8�qD9� D:  D:}qD;  D;� D<  D<}qD<��D=� D>  D>}qD?�D?� D?�qD@� D@�qDA}qDA�qDB� DC  DC��DDDD�DE  DEz�DF  DF�DG�DG� DH  DH}qDI�DI��DJ�DJ� DJ��DK}qDL  DL}qDL��DM}qDM�qDNz�DN��DO}qDO�qDP}qDP�qDQ��DR�DR��DS  DS}qDT�DT� DU  DU}qDU�qDV� DW�DW� DW�qDX��DY�DY��DZ  DZ}qDZ�qD[}qD\�D\��D]  D]}qD^  D^� D_  D_}qD_�qD`}qDa  Da� Db  Db� Db�qDc� Dd�Dd� De  De��DfDf��Df��Dgz�Dg�qDh��Dh�qDi� Dj  Dj}qDk  Dk}qDk�qDl}qDl�qDmz�Dn  Dn��DoDo��Do�qDp� Dp�qDqz�Dq�qDr��Ds�Ds��DtDt��Du�Du� Du�qDvz�Dw  Dw� Dx  Dx��Dx�qDy��Dz�Dz� D{�D{� D|  D|}qD|�qD}}qD~  D~� D~�qD��D�HD�@ D�~�D���D�  D�>�D�~�D�� D�  D�>�D�~�D�� D�HD�B�D��HD��HD�  D�AHD�� D��qD���D�AHD��HD��HD�HD�@ D���D�D�  D�@ D���D�D�  D�>�D�� D��HD�  D�>�D�~�D�� D��D�@ D�� D�� D���D�@ D�� D�� D�  D�C�D��HD�� D�  D�@ D��HD�� D�HD�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�>�D�~�D�� D�  D�AHD��HD��HD�  D�@ D�� D��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?\)?L��?�=q?�Q�?Ǯ@   @��@�R@8Q�@G�@c�
@s33@��
@���@�Q�@��@�\)@�Q�@��@�{@�@�G�@�=q@�33A ��A�
A�A{A�\AffA��A   A%�A*�HA.{A4z�A8Q�A>{AA�AFffAL��AP  AVffAZ=qA_\)Ae�AhQ�An{Ar�\Aw
=A|��A\)A��\A���A��RA���A�(�A�p�A���A��\A�p�A�\)A�G�A�z�A�ffA���A��
A�p�A�  A��\A�z�A��A���A�(�A��RA���A��
A�A�  A�33A���AǮAə�A�(�A�\)AУ�A��
A�{A�Q�A�33A���A�Q�A�=qA���A�\)A�G�A�z�A�{A���A�A�p�A���A�33A���B   BG�B{B�B��B�B\)Bz�B	��B33B(�BG�B�HB�
B��B�\B�Bz�B=qB33BQ�BB�\B(�B�B=qB�
B ��B"{B#33B$(�B%B&�RB(  B)G�B*{B+�B,��B.{B/33B0Q�B1B2�RB4Q�B5G�B6�RB8  B8��B:ffB;�B<z�B>{B?33B@(�BA�BB�RBD  BE��BFffBG�
BIG�BJ{BK�BL��BM�BO\)BP(�BQp�BR�RBS�BT��BV=qBW
=BX��BY��BZ�RB\(�B]�B^=qB_�B`��BaBc33Bd  Bep�Bf�RBg�Bh��Bj=qBk
=Bl��BmBn�RBpQ�Bqp�BrffBt  Bt��Bv{Bw�Bxz�ByB{33B|  B}p�B~�RB�B���B��B���B�ffB���B�p�B�=qB��RB�\)B�  B�Q�B��B��B�(�B���B�G�B�  B���B�
=B�B�z�B���B��B�(�B���B�p�B��B�ffB�33B��
B�=qB���B��B�{B���B��B�  B���B�G�B�B��\B�33B���B�Q�B���B�\)B�{B���B�33B��B�Q�B�
=B��B�{B���B�p�B��
B��\B�G�B��B�ffB��HB�\)B�=qB���B��B��B�ffB���B��B�=qB��RB��B�  B��\B�\)B�B�z�B��B���B�Q�B���B�p�B�(�B��HB�G�B��
B��RB��B��B�z�B�
=B��B�(�B���B�\)B�{B��RB��B��
B�z�B���B��B�Q�B��RB��B�(�B��\B�G�B�  B�z�B���B�B�=qBĸRBŅB��BƏ\B�G�BǮB�Q�B�
=B�p�B�  BʸRB�\)B��
B�Q�B��BͮB�{B��HBυB��BЏ\B�G�B��
B�=qB���Bә�B�  B�z�B�33B�B�{BָRB�p�B��B�ffB�
=B�B�(�B��HBۙ�B�(�B܏\B�G�B��
B�ffB��HB�B�Q�B���B�\)B�(�B�RB�33B��B�\B�
=B�B�ffB���B�p�B�=qB��B��B�  B�z�B�
=B뙚B�Q�B���B�B�{B���B�p�B��B�RB�p�B�  B�\B�G�B�  B�z�B��B��B�z�B�
=B��
B�ffB��HB��B�Q�B���B��B�Q�B���B�\)B�(�B��HB�\)C {C p�C �C{Cp�C�C�CffC�C{C\)C��C
=CG�C��C  C=qC��C��C33C�\C��C33C�C�C(�Cz�C�HC	{C	�C	��C
{C
p�C
��C  CffC�RC��CQ�C�C�HC33C��C�
C(�C�\C�
C{Cz�C��C{CQ�CC  C=qC��C��C(�C�C�
C�C\)CC{CQ�C�C  C33C��C�C�Cp�C�
C
=CQ�C�C  C=qC��C�HC(�C�C�HC�Cp�C��C(�CffC�C
=CffC��C�HCG�C�\C��C
=CffC�RC  C33C�\C�C �C ffC �RC!{C!Q�C!�\C!�HC"G�C"z�C"�RC#
=C#Q�C#�\C#�RC$
=C$Q�C$�\C$�C$�C%�C%\)C%�C%��C%�HC&{C&33C&G�C&z�C&��C&�RC&C&�C'{C'=qC'=qC'\)C'�C'�C'�RC'�HC(
=C((�C(33C(Q�C(p�C(��C(C(�HC(�C){C)G�C)ffC)z�C)��C)C)�C*
=C*�C*=qC*p�C*��C*�C*��C+  C+33C+=qC+\)C+z�C+��C+��C,  C,(�C,=qC,\)C,z�C,�RC,��C,�HC-
=C-=qC-ffC-z�C-�\C-C-��C.
=C.�C.=qC.z�C.�C.��C.�HC/  C/33C/ffC/�\C/��C/C0  C033C0Q�C0z�C0��C0�RC0�C1�C133C1\)C1��C1�RC1�
C2  C2=qC2p�C2�\C2�C2�
C3{C3G�C3\)C3�C3�C3��C4�C4=qC4\)C4��C4��C5  C5{C5G�C5�C5�C5C5��C633C6ffC6�C6�C6�
C7{C7G�C7z�C7��C7C7�C8�C8\)C8�\C8��C8��C9  C933C9ffC9�\C9�C9�
C:�C:G�C:p�C:�\C:�
C;{C;=qC;\)C;�\C;C<
=C<=qC<ffC<�C<��C=
=C=33C=\)C=�C=�RC>  C>=qC>\)C>�\C>�RC>��C?33C?ffC?�\C?�RC?�HC@(�C@\)C@�C@�C@�CA(�CAQ�CAp�CA��CA�HCB{CBQ�CB�CB��CB��CB��CC33CCp�CC��CC��CC�CD(�CDffCD��CD��CD�CE{CEG�CEz�CE�RCE�CF�CF33CFp�CF�RCF�HCG
=CG(�CG\)CG��CG��CH
=CH33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�Aҩ�Aҧ�Aҥ�Aң�Aҟ�AғuAґhA҉7A҇+A�~�A�p�A�p�A�E�A�C�A�?}A�7LA�5?A�5?A�33A�/A�-A�-A�/A�/A�-A�1'A�5?A�7LA�9XA�7LA�;dA�;dA�;dA�=qA�=qA�A�A�A�A�C�A�E�A�E�A�E�A�C�A�C�A�A�A�C�A�E�A�=qA�5?A�oA��A��A��#A���AѾwAѧ�AёhA�l�Aд9A�x�A��
A�{A�(�A�bA�ƨAŬAò-A��HAA�$�A�XA�l�A�ZA��HA��;A��HA�1A��^A�ĜA�I�A���A��-A���A���A�%A��#A�x�A���A�VA�%A�ƨA���A�$�A�ȴA���A�t�A�v�A���A���A��HA���A�9XA�I�A�9XA���A�%A��A���A���A��A���A�~�A��A��^A�XA��A�ffA���A}�^A{�AzI�Aw��As��Ao��An9XAl��Aj�Aj$�Agt�Aat�A^jA]�7A]K�A\{AZ5?AY%AW�wAU�FAT�uAS�AR�9AQp�AP��AO�AM��AK�PAJjAH��AF�AEK�AC�^A@ȴA=%A:�!A7��A7"�A7
=A6�A5��A2�DA2A�A1��A0 �A/�hA/dZA/7LA.��A-�TA,�A+?}A*E�A)�mA(�A'dZA&bA%l�A$�uA${A"~�A!�A!%A�A�A��AVA�A�wA��AdZA�9A�AbAVA�\AA�A�A�DA�mA&�A�A��A�-A7LA��A  A�AjA��A��A�yA�An�A�;A��AdZA��A	�#AAS�Av�A��A5?A�#A��A%Av�A �A A�@�@�-@�?}@���@�?}@�@��;@�v�@�=q@�x�@��@�5?@�@�hs@��`@�9X@��@�+@�!@�ff@��@�V@�u@�(�@�@��H@�@���@�@�V@�j@�I�@�K�@���@�j@��@��y@�=q@���@�7@�x�@�@��@���@�\@���@��/@�Z@ߍP@ް!@�=q@�?}@���@���@�ff@�-@��#@ٺ^@�E�@�$�@ى7@׍P@֏\@�v�@��#@�l�@��@�\)@��#@Ь@мj@мj@ЋD@Л�@У�@�I�@��m@�o@�v�@��@�bN@�b@ʟ�@ȴ9@�Q�@� �@Ǖ�@���@�J@��@�$�@�J@ũ�@��`@�Q�@å�@���@�$�@�{@���@��#@���@���@�r�@��@�\)@���@��R@�-@��@��h@���@�Q�@��@�ƨ@�l�@���@�V@�{@���@��#@��7@���@�Ĝ@�bN@�(�@��m@��@�S�@���@��\@�{@���@�7L@�%@��`@�A�@��@�t�@���@�v�@�E�@��@�@���@�r�@�I�@�1@���@�C�@��@���@��+@�n�@�J@���@��@���@�X@��`@�Ĝ@��@��u@�bN@���@�S�@�;d@�@�ff@�5?@�J@��@�p�@�/@��@��u@��;@�t�@��@��T@��^@��-@��@�`B@�O�@�?}@���@���@�z�@�bN@�A�@��@���@��P@��P@��@�K�@�ȴ@�ff@��@���@�G�@���@��/@��u@�Z@� �@�1@��;@���@�S�@���@��@�@��@���@���@�1'@�1@��;@��@�33@���@�~�@�@�@�x�@�X@�/@���@��j@�Q�@��F@�t�@�S�@�33@�"�@��@��@�E�@��@���@��^@��7@�p�@�G�@��@��@��
@�|�@�+@��R@�V@���@�`B@�/@��@�V@��@�%@���@�z�@�bN@�Q�@�1'@�1@���@�C�@��R@�=q@���@�=q@�-@��#@�O�@���@���@��/@��j@�(�@��m@��F@��@�S�@�33@��@��@���@�n�@���@�`B@�%@��`@���@��D@�Q�@�9X@�b@��m@���@��
@���@�ƨ@��@�o@��R@�n�@�-@��@���@�hs@��@�%@��`@��D@�z�@�Z@�A�@�1'@� �@�  @��F@���@�t�@�33@�ȴ@�5?@��@��@��@���@��7@�G�@��@�%@���@�bN@�Z@�I�@�9X@�9X@�(�@�1@�P@+@~��@~��@~�y@~�@~��@~5?@}�@|��@|�@{�F@{S�@{33@z�@z�\@z=q@y�@y��@yX@x��@x��@x �@w��@v��@v�+@vV@v@u�h@up�@up�@u`B@u?}@t�@t�j@t��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aҩ�AҬAҩ�AҬAҥ�Aҩ�Aҧ�Aҧ�Aҩ�Aҡ�Aҝ�Aҡ�Aң�AҰ!Aҥ�Aң�A҉7AҍPAқ�Aҏ\AҍPAғuAғuAҋDA҉7A҇+A҇+AҍPA҇+A҅A҇+A�z�A�z�A�v�A�dZA�ffA�n�A�t�AҁA�|�A�x�A�K�A�K�A�C�A�E�A�I�A�C�A�I�A�E�A�C�A�E�A�?}A�C�A�C�A�?}A�A�A�A�A�;dA�A�A�;dA�?}A�;dA�7LA�=qA�7LA�5?A�7LA�33A�7LA�7LA�33A�9XA�33A�5?A�7LA�33A�7LA�5?A�33A�7LA�1'A�5?A�/A�33A�7LA�/A�33A�/A�/A�/A�+A�1'A�+A�/A�-A�+A�/A�+A�/A�1'A�+A�/A�/A�(�A�/A�/A�+A�/A�/A�+A�/A�-A�-A�1'A�-A�+A�1'A�-A�-A�1'A�/A�+A�1'A�-A�/A�/A�+A�1'A�/A�/A�1'A�/A�33A�33A�1'A�7LA�33A�7LA�7LA�33A�7LA�33A�9XA�7LA�9XA�9XA�5?A�;dA�7LA�9XA�;dA�7LA�;dA�;dA�33A�9XA�7LA�5?A�=qA�5?A�9XA�=qA�7LA�;dA�=qA�7LA�=qA�;dA�;dA�?}A�9XA�;dA�=qA�9XA�?}A�=qA�9XA�=qA�=qA�9XA�?}A�;dA�9XA�?}A�;dA�=qA�A�A�;dA�?}A�A�A�;dA�?}A�?}A�=qA�A�A�?}A�=qA�C�A�A�A�?}A�C�A�?}A�?}A�E�A�A�A�C�A�E�A�A�A�E�A�E�A�C�A�E�A�E�A�A�A�G�A�E�A�C�A�G�A�C�A�G�A�G�A�A�A�G�A�E�A�C�A�G�A�A�A�E�A�G�A�C�A�E�A�I�A�C�A�E�A�C�A�A�A�E�A�C�A�?}A�C�A�E�A�?}A�C�A�E�A�?}A�E�A�C�A�A�A�E�A�A�A�?}A�C�A�C�A�?}A�C�A�C�A�?}A�C�A�E�A�C�A�I�A�C�A�G�A�E�A�A�A�G�A�C�A�?}A�A�A�A�A�;dA�?}A�;dA�9XA�=qA�9XA�9XA�9XA�/A�(�A�-A�(�A��A��A�VA�
=A�%A���A���A���A��A���A��A��A��A��A��A��A��A��mA��`A��mA��HA��
A��A��A���A���A���A���A���A���A�ƨA�ȴA�AѼjA���AѾwAѺ^AѺ^AѸRAѮAѧ�Aѩ�Aѣ�Aџ�Aѥ�Aѝ�Aљ�Aћ�AёhAёhAѓuAэPAсAуA�|�A�r�A�dZA�bNA�VA�C�A�/A���AХ�A�ffA�M�A�VA��`AϬAω7A�jA�VA�M�A�/A���A�AΕ�A�A�Aͧ�A��A�ffA�&�A˧�A�ZA���Aʛ�Aʙ�Aʏ\A�p�A�bNA�=qA�33A��A�ĜAɸRAɗ�A�(�A���A���Aȥ�AȓuA�VA�+A��AǾwAǉ7A�`BA��AƸRA�ffA���A�"�A��A��TA���A���Aú^AÇ+A�ffA�;dA��A�A���A��A�ĜA¾wA®A�ADAA�z�A�ffA�`BA�Q�A�A�A�/A��A�VA���A���A�r�A�JA��!A��A���A�r�A�t�A�r�A�l�A�ffA�dZA�ffA�dZA�^5A�`BA�S�A�I�A�C�A�;dA�9XA�5?A���A�\)A�1A��-A�jA���A���A�VA�&�A�1A�  A���A���A���A�ƨA��!A��DA�`BA�1A�ȴA�~�A�O�A�9XA�VA���A���A�l�A�K�A�"�A���A��#A��jA��!A��PA�|�A�z�A�v�A�jA�G�A�9XA���A���A�ffA�(�A���A�~�A�=qA�  A��`A��/A��
A�ȴA���A�A�A��A�O�A���A�ȴA��9A���A��PA��uA�;dA��\A�VA��#A���A�n�A�I�A�bA��A��
A��jA���A��+A�I�A�{A��#A���A�9XA�1A���A��PA�ffA�^5A�ZA�E�A�+A�  A���A��jA��+A�7LA�bA��/A���A��DA�v�A�bNA�O�A�;dA��A�\)A��A��A��A�33A�
=A��mA�ĜA���A���A���A��7A�~�A�r�A�jA�`BA�O�A�;dA��A��#A���A�oA���A�bNA��A��RA���A�x�A�O�A�oA��A��;A���A���A�l�A�-A��yA�A�~�A�-A�bA���A��A���A���A��\A�z�A�\)A�C�A�7LA�+A�$�A�$�A� �A��A�VA�%A�A���A��A��A��/A���A���A���A�ĜA�ĜA�A�A�ĜA�ĜA���A�ĜA���A��RA���A�l�A�M�A�{A���A�|�A��A�bA�JA�  A�  A�  A���A���A��A��A��TA��/A���A���A�+A��^A�r�A�K�A�/A�ĜA���A��A�l�A�hsA�hsA�ZA�I�A�G�A�7LA�(�A�VA�A���A���A�ĜA��-A�z�A�\)A�E�A�-A�bA�JA�A��A��;A��/A���A���A�A��FA���A���A��A�t�A�VA�I�A� �A�  A��;A���A�l�A�S�A�K�A�5?A��yA�ffA��A���A�dZA�+A��yA���A���A���A��+A�l�A�33A�(�A�(�A�$�A��A��A�oA�JA��wA�1A���A�hsA�=qA���A��FA���A�|�A�p�A�hsA�ZA�M�A�E�A�=qA�33A�-A�$�A��A�oA�JA�A��A��mA��/A���A���A��^A���A��7A�^5A�{A��A�|�A�/A��A���A��RA���A��7A�hsA�S�A�G�A�9XA�-A�/A�&�A�J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aҩ�Aҧ�Aҥ�Aң�Aҟ�AғuAґhA҉7A҇+A�~�A�p�A�p�A�E�A�C�A�?}A�7LA�5?A�5?A�33A�/A�-A�-A�/A�/A�-A�1'A�5?A�7LA�9XA�7LA�;dA�;dA�;dA�=qA�=qA�A�A�A�A�C�A�E�A�E�A�E�A�C�A�C�A�A�A�C�A�E�A�=qA�5?A�oA��A��A��#A���AѾwAѧ�AёhA�l�Aд9A�x�A��
A�{A�(�A�bA�ƨAŬAò-A��HAA�$�A�XA�l�A�ZA��HA��;A��HA�1A��^A�ĜA�I�A���A��-A���A���A�%A��#A�x�A���A�VA�%A�ƨA���A�$�A�ȴA���A�t�A�v�A���A���A��HA���A�9XA�I�A�9XA���A�%A��A���A���A��A���A�~�A��A��^A�XA��A�ffA���A}�^A{�AzI�Aw��As��Ao��An9XAl��Aj�Aj$�Agt�Aat�A^jA]�7A]K�A\{AZ5?AY%AW�wAU�FAT�uAS�AR�9AQp�AP��AO�AM��AK�PAJjAH��AF�AEK�AC�^A@ȴA=%A:�!A7��A7"�A7
=A6�A5��A2�DA2A�A1��A0 �A/�hA/dZA/7LA.��A-�TA,�A+?}A*E�A)�mA(�A'dZA&bA%l�A$�uA${A"~�A!�A!%A�A�A��AVA�A�wA��AdZA�9A�AbAVA�\AA�A�A�DA�mA&�A�A��A�-A7LA��A  A�AjA��A��A�yA�An�A�;A��AdZA��A	�#AAS�Av�A��A5?A�#A��A%Av�A �A A�@�@�-@�?}@���@�?}@�@��;@�v�@�=q@�x�@��@�5?@�@�hs@��`@�9X@��@�+@�!@�ff@��@�V@�u@�(�@�@��H@�@���@�@�V@�j@�I�@�K�@���@�j@��@��y@�=q@���@�7@�x�@�@��@���@�\@���@��/@�Z@ߍP@ް!@�=q@�?}@���@���@�ff@�-@��#@ٺ^@�E�@�$�@ى7@׍P@֏\@�v�@��#@�l�@��@�\)@��#@Ь@мj@мj@ЋD@Л�@У�@�I�@��m@�o@�v�@��@�bN@�b@ʟ�@ȴ9@�Q�@� �@Ǖ�@���@�J@��@�$�@�J@ũ�@��`@�Q�@å�@���@�$�@�{@���@��#@���@���@�r�@��@�\)@���@��R@�-@��@��h@���@�Q�@��@�ƨ@�l�@���@�V@�{@���@��#@��7@���@�Ĝ@�bN@�(�@��m@��@�S�@���@��\@�{@���@�7L@�%@��`@�A�@��@�t�@���@�v�@�E�@��@�@���@�r�@�I�@�1@���@�C�@��@���@��+@�n�@�J@���@��@���@�X@��`@�Ĝ@��@��u@�bN@���@�S�@�;d@�@�ff@�5?@�J@��@�p�@�/@��@��u@��;@�t�@��@��T@��^@��-@��@�`B@�O�@�?}@���@���@�z�@�bN@�A�@��@���@��P@��P@��@�K�@�ȴ@�ff@��@���@�G�@���@��/@��u@�Z@� �@�1@��;@���@�S�@���@��@�@��@���@���@�1'@�1@��;@��@�33@���@�~�@�@�@�x�@�X@�/@���@��j@�Q�@��F@�t�@�S�@�33@�"�@��@��@�E�@��@���@��^@��7@�p�@�G�@��@��@��
@�|�@�+@��R@�V@���@�`B@�/@��@�V@��@�%@���@�z�@�bN@�Q�@�1'@�1@���@�C�@��R@�=q@���@�=q@�-@��#@�O�@���@���@��/@��j@�(�@��m@��F@��@�S�@�33@��@��@���@�n�@���@�`B@�%@��`@���@��D@�Q�@�9X@�b@��m@���@��
@���@�ƨ@��@�o@��R@�n�@�-@��@���@�hs@��@�%@��`@��D@�z�@�Z@�A�@�1'@� �@�  @��F@���@�t�@�33@�ȴ@�5?@��@��@��@���@��7@�G�@��@�%@���@�bN@�Z@�I�@�9X@�9X@�(�@�1@�P@+@~��@~��@~�y@~�@~��@~5?@}�@|��@|�@{�F@{S�@{33@z�@z�\@z=q@y�@y��@yX@x��@x��@x �@w��@v��@v�+@vV@v@u�h@up�@up�@u`B@u?}@t�@t�jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aҩ�AҬAҩ�AҬAҥ�Aҩ�Aҧ�Aҧ�Aҩ�Aҡ�Aҝ�Aҡ�Aң�AҰ!Aҥ�Aң�A҉7AҍPAқ�Aҏ\AҍPAғuAғuAҋDA҉7A҇+A҇+AҍPA҇+A҅A҇+A�z�A�z�A�v�A�dZA�ffA�n�A�t�AҁA�|�A�x�A�K�A�K�A�C�A�E�A�I�A�C�A�I�A�E�A�C�A�E�A�?}A�C�A�C�A�?}A�A�A�A�A�;dA�A�A�;dA�?}A�;dA�7LA�=qA�7LA�5?A�7LA�33A�7LA�7LA�33A�9XA�33A�5?A�7LA�33A�7LA�5?A�33A�7LA�1'A�5?A�/A�33A�7LA�/A�33A�/A�/A�/A�+A�1'A�+A�/A�-A�+A�/A�+A�/A�1'A�+A�/A�/A�(�A�/A�/A�+A�/A�/A�+A�/A�-A�-A�1'A�-A�+A�1'A�-A�-A�1'A�/A�+A�1'A�-A�/A�/A�+A�1'A�/A�/A�1'A�/A�33A�33A�1'A�7LA�33A�7LA�7LA�33A�7LA�33A�9XA�7LA�9XA�9XA�5?A�;dA�7LA�9XA�;dA�7LA�;dA�;dA�33A�9XA�7LA�5?A�=qA�5?A�9XA�=qA�7LA�;dA�=qA�7LA�=qA�;dA�;dA�?}A�9XA�;dA�=qA�9XA�?}A�=qA�9XA�=qA�=qA�9XA�?}A�;dA�9XA�?}A�;dA�=qA�A�A�;dA�?}A�A�A�;dA�?}A�?}A�=qA�A�A�?}A�=qA�C�A�A�A�?}A�C�A�?}A�?}A�E�A�A�A�C�A�E�A�A�A�E�A�E�A�C�A�E�A�E�A�A�A�G�A�E�A�C�A�G�A�C�A�G�A�G�A�A�A�G�A�E�A�C�A�G�A�A�A�E�A�G�A�C�A�E�A�I�A�C�A�E�A�C�A�A�A�E�A�C�A�?}A�C�A�E�A�?}A�C�A�E�A�?}A�E�A�C�A�A�A�E�A�A�A�?}A�C�A�C�A�?}A�C�A�C�A�?}A�C�A�E�A�C�A�I�A�C�A�G�A�E�A�A�A�G�A�C�A�?}A�A�A�A�A�;dA�?}A�;dA�9XA�=qA�9XA�9XA�9XA�/A�(�A�-A�(�A��A��A�VA�
=A�%A���A���A���A��A���A��A��A��A��A��A��A��A��mA��`A��mA��HA��
A��A��A���A���A���A���A���A���A�ƨA�ȴA�AѼjA���AѾwAѺ^AѺ^AѸRAѮAѧ�Aѩ�Aѣ�Aџ�Aѥ�Aѝ�Aљ�Aћ�AёhAёhAѓuAэPAсAуA�|�A�r�A�dZA�bNA�VA�C�A�/A���AХ�A�ffA�M�A�VA��`AϬAω7A�jA�VA�M�A�/A���A�AΕ�A�A�Aͧ�A��A�ffA�&�A˧�A�ZA���Aʛ�Aʙ�Aʏ\A�p�A�bNA�=qA�33A��A�ĜAɸRAɗ�A�(�A���A���Aȥ�AȓuA�VA�+A��AǾwAǉ7A�`BA��AƸRA�ffA���A�"�A��A��TA���A���Aú^AÇ+A�ffA�;dA��A�A���A��A�ĜA¾wA®A�ADAA�z�A�ffA�`BA�Q�A�A�A�/A��A�VA���A���A�r�A�JA��!A��A���A�r�A�t�A�r�A�l�A�ffA�dZA�ffA�dZA�^5A�`BA�S�A�I�A�C�A�;dA�9XA�5?A���A�\)A�1A��-A�jA���A���A�VA�&�A�1A�  A���A���A���A�ƨA��!A��DA�`BA�1A�ȴA�~�A�O�A�9XA�VA���A���A�l�A�K�A�"�A���A��#A��jA��!A��PA�|�A�z�A�v�A�jA�G�A�9XA���A���A�ffA�(�A���A�~�A�=qA�  A��`A��/A��
A�ȴA���A�A�A��A�O�A���A�ȴA��9A���A��PA��uA�;dA��\A�VA��#A���A�n�A�I�A�bA��A��
A��jA���A��+A�I�A�{A��#A���A�9XA�1A���A��PA�ffA�^5A�ZA�E�A�+A�  A���A��jA��+A�7LA�bA��/A���A��DA�v�A�bNA�O�A�;dA��A�\)A��A��A��A�33A�
=A��mA�ĜA���A���A���A��7A�~�A�r�A�jA�`BA�O�A�;dA��A��#A���A�oA���A�bNA��A��RA���A�x�A�O�A�oA��A��;A���A���A�l�A�-A��yA�A�~�A�-A�bA���A��A���A���A��\A�z�A�\)A�C�A�7LA�+A�$�A�$�A� �A��A�VA�%A�A���A��A��A��/A���A���A���A�ĜA�ĜA�A�A�ĜA�ĜA���A�ĜA���A��RA���A�l�A�M�A�{A���A�|�A��A�bA�JA�  A�  A�  A���A���A��A��A��TA��/A���A���A�+A��^A�r�A�K�A�/A�ĜA���A��A�l�A�hsA�hsA�ZA�I�A�G�A�7LA�(�A�VA�A���A���A�ĜA��-A�z�A�\)A�E�A�-A�bA�JA�A��A��;A��/A���A���A�A��FA���A���A��A�t�A�VA�I�A� �A�  A��;A���A�l�A�S�A�K�A�5?A��yA�ffA��A���A�dZA�+A��yA���A���A���A��+A�l�A�33A�(�A�(�A�$�A��A��A�oA�JA��wA�1A���A�hsA�=qA���A��FA���A�|�A�p�A�hsA�ZA�M�A�E�A�=qA�33A�-A�$�A��A�oA�JA�A��A��mA��/A���A���A��^A���A��7A�^5A�{A��A�|�A�/A��A���A��RA���A��7A�hsA�S�A�G�A�9XA�-A�/A�&�A�J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B�B=B�B�B�BBBkB�B7B	BkB1B�B�B�B�B�B�B�B�B�B�B�B�B�B�B	B	B=B=B=B�B�BxBCBxB�BBB~BVB�B�B!-B0�B;�B@�BFtBJ�BK�BNBP}BR BS�BT,BT�B^�Bj�B�B�1B��B�:B�hB�4B��B��B��B�B�oB|�Bz�B}�B}�Br�Bu�BrBl�Bh�BiyB^BaBS�B@OB<jB1[B,qB#B�BB�DB��B��B��B��B�:B��B�%Bx�Bc�BHB8B!�B�B�B
��B
�B
�dB
�mB
��B
�kB
��B
e�B
]/B
X�B
NpB
EmB
49B
$@B
�B

rB	�B	�B	�9B	�B	��B	��B	��B	�=B	��B	y�B	wfB	v`B	iDB	b�B	YB	R�B	H�B	D�B	>wB	8�B	2aB	.IB	)�B	�B	1B	FB	"B	�B	AB��B�AB�B��B�B��B�jB�|B�/B��B�B��B��BݘB��B�dB�B��B��B�vBޞB�jB�jB��B�[B��BޞB�vB�/B��B�`B�B��B�2B�QB�"B��B��B�"B��B	AB��B��B�.B	�B	�B	;B	{B	�B	uB�	B�PB�B�B��B��B	�B		B	�B	�B	�B	%�B	VB	.B	(�B	�B	�B	B	$�B	/�B	5�B	5�B	5tB	1�B	�B	 4B	 �B	 iB	oB	�B		B	�B	%FB	&B	"�B	$tB	"hB	%zB	-B	0!B	2�B	3�B	5tB	8�B	:^B	=B	>�B	>�B	>�B	=�B	=<B	E�B	J�B	M�B	R B	VB	XEB	W�B	VmB	T�B	RTB	UgB	XEB	ZQB	Y�B	YKB	Z�B	_;B	f�B	k�B	gB	f�B	e�B	h
B	jB	kB	h�B	kQB	o B	pB	m]B	m]B	m�B	m�B	qB	w�B	xlB	xB	w�B	u�B	v�B	v�B	qvB	n�B	s�B	qvB	ncB	rB	sMB	r�B	v+B	z�B	{�B	{�B	�B	~�B	}"B	|�B	~(B	��B	� B	�B	�4B	��B	��B	�GB	��B	�DB	��B	�VB	��B	�VB	�\B	�@B	�SB	��B	��B	�YB	��B	��B	�CB	��B	��B	��B	�hB	��B	��B	��B	��B	��B	�B	�!B	�[B	��B	�B	�zB	��B	�B	��B	�$B	�jB	��B	�}B	��B	�[B	��B	�B	��B	�B	��B	ȴB	ȴB	�B	�0B	�jB	�pB	�B	�vB	�B	�B	�vB	��B	��B	�<B	�B	�HB	�}B	бB	��B	бB	�NB	�TB	�TB	��B	՛B	�sB	�B	��B	��B	רB	רB	�KB	�QB	�QB	یB	ޞB	�;B	�vB	�B	�B	�&B	��B	��B	�B	�sB	�B	�QB	�B	�B	�QB	�QB	�QB	�B	�B	�]B	�]B	�B	��B	�5B	�iB	�B	�B	�B	�B	��B	�|B	�MB	�B	��B	�B	��B	�B	��B	�xB	��B	�B	�B	��B	��B	��B	�cB	��B	��B
;B
uB
uB
�B
�B
�B
B
�B
�B
+B
�B
fB
fB
	B

=B
�B
~B
~B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
.B
.B
�B
.B
bB
 B
hB
�B
�B
FB
�B
B
YB
�B
�B
�B
B
7B
�B
	B
=B
	B
7B
�B
�B
qB
�B
�B
OB
�B
B
VB
�B
�B
B
B
�B
�B
 �B
 �B
 �B
 �B
!�B
 �B
�B
 \B
 �B
 \B
 �B
 �B
 �B
!bB
!�B
!�B
"�B
$B
%�B
&LB
%zB
%zB
$tB
%B
%FB
&LB
'B
(XB
($B
(�B
)�B
+6B
+�B
,=B
,�B
,�B
-CB
.}B
.�B
.}B
.}B
.IB
-�B
-�B
.�B
0!B
0�B
1[B
1�B
2-B
1�B
2aB
2�B
2�B
2�B
3hB
3hB
3�B
3hB
3hB
33B
2�B
2�B
2�B
2�B
2�B
3hB
3�B
3hB
3hB
3�B
49B
49B
5B
5?B
5�B
5�B
6B
6�B
6�B
6zB
6zB
6zB
6�B
6�B
7LB
7LB
7�B
7�B
7�B
7�B
8B
8�B
8�B
8�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B7B�BB	B7BqBqB�B�B�B1BxB�B~B�B!-B7B�BxB	B�BBBBBBeB�BeB�BkB�B1BB�B�B$B�B�BB#nB�B�B1B+BB+BeB�B�B�B�B+BB+B�BBYBB�B�BB�BBeB+B�B�B�BB�B�B�B_BeB�B�BeB�B7B�B�B1B�B�B_B�B�B�BB�BeB�B�B�B�B�B+B+B7B�B+BB+B�BB+B�B�B�BeB1B�B1BB�B�B�B�B�B7B_B�B�B�B�B�BB�B�B�B�B�BqBeB�BkB7B�B7BB7BkB�B	B�BB�B�BkBCB7B�B�B�BqBCBeBBqB7BCB�B�BxB�BqBqBBCB=B�BxB7B	B�B�B�B�B�BCBxB	B�BCB	B~BB	BIB�BBB�B�BBqBCB~B�BIBIBqBIBxB�B�BBxBBqBBBBBBCBOBB�B�B�BIBB�B�BBxBOB~BCB!B�B�B�B�B�B �B�BB \B�B�B �B�B�B�BB!B �B!B�B!bB�B�B!bB�B!bB#�B"4B$�B%zB1�B49B5tB7�B9�B9$B9XB;�B:*B=�B?HB=qB?�B@�B@OBCaBB�BA�BB�BFBD�BF?BHBG�BK�BIBJ#BJ�BI�BJXBL�BJ�BJ�BLdBK�BJ�BM6BM6BK�BM6BN�BNpBM�BPHBN�BO�BQNBPBP}BR BP�BQ�BR�BQ�BQ�BR�BRTBR�BTaBT�BR�BS�BTaBRTBT�BS�BS&BT�BT,BR�BS�BU�BS&BS�BUgBW�BTaBT�BXBWsB^�Bg�Ba�B\)BjKBdZBn�Bg8Bk�Bi�BhsBncBrBpBu�B}�B��B� B�bB�1B��B�B��B��B�B�B�uB�MB��B�.B��B�bB��B��B�\B��B�uB� B�"B��B�B��B� B�~B��B��B��B��B��B��B��B��B��B�FB�B�B��B�7B��B��B�_B�1B�+B��B��B��B��B�B��B�_B��B��B�%B�B�B�;B��B�AB��B��B�fB�-B��B|�B{�B}�B~�B~]B|�Bz�Bz�B{�By�B|�Bv�ByrBxlBu�Bv�B�'B~�B��Bv`Bx8B�GB��B|�Bu�Bv�Bq�BsBy>BpoBo BsMBtTBt�B{�Bv�By�Bo5Bq�BqvBu�Bs�Bn/Bo�BqBqBl�BlWBi�Bn/Bg�BgmBffBe�BhsBgmBn�Bk�BiyBjKBf2Bk�Bb�Bf�B^jB[WBYBZQB]�BkQB[�Bd�B\)BNpBMjB��Bu%BZQBU�BTaBQBE�BG�BC�BA�BDgB>wB=qB;dB<�B;�B?B:�B;0BC-B<�B5?B9�B2�B0UB-CB-wB.}B,�B2�B)�B*0B/OB1'B(�B&�B%FB �B"�B �B~B�B�B0�B1B�B%FBMBBVB�B�BxB	�B
�B
�B	7B1B�B�B
	BfB1BxB�BB�B
	B��B��B��B�DB��B��B��B�B�%B�B�|B�TB�sB�B�DB�B��B��B�B�BیBںB�QB�KB�EB�B��BӏBҽB��B�B��B��B� B�[B��B��B�B�dB�B�0B��B�0B�dB̘B��B��BǮBɆB��BҽB��B��B�<B��B��B�HB�nB��B��B��B�zB�B�B��B��B�tB�nB��B��B��B�tB��B��B�}B�B��B�:B��B��B�B�B�B��B�B�B�CB�	B��B�YB��B��B�B��B�"B��B� B�lB�+B�YB��B��B��B�B��B�B�SB��B�uB|B��B� Bz�B|�By>Bx8ByrBn�Bm�Bm)Bs�By>Bm�Bp�B^5B\�BZ�BTaBR�BN<BMBOBBPBGBD�BD�BC�BCaBC�BC-BG�BS�B>�B7�B7LB5�B0UB(�B(XB%zB#B$tB#�B#B"�B!�B�B�B!B�B!B�B�B�B�B�B�B$BYBBBbBuB�B4B�BGB
��B
�cB
�B
�B
��B
��B
��B
�+B
�B
�oB
��444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                       4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                       4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2023010502014620230105020146IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023011423315020230114233150QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023011423315020230114233150QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194920230210131949IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                