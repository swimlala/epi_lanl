CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  E   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-03-27T15:07:11Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�     (  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  W0   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     (  ]�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     (  ~p   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �L   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     (  �(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � /   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( 5�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` O�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   V   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   \   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T b   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   bl   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   bt   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   b|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   b�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � b�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   c   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   c(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    c0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        cP   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        cX   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       c`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    chArgo profile    3.1 1.2 19500101000000  20220327150711  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_206                 6810_008521_206                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @���%�ם@���%�ם11  @���PH�@���PH�@0q��?)@0q��?)�d����t�d����t11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@:�H@xQ�@��R@�  @�p�@�(�A{A\)A,��AAG�A`��A�  A�  A�  A�  A�  A�  A�Q�A�  A�\)B�B�
B  B Q�B((�B0  B8(�B@  BG�
BO�
BW�
B_�
Bh(�Bp(�Bx  B�  B��B��
B��
B�  B�{B�(�B�{B��B�  B�{B��B��B�  B�  B�  B��B��B��B�{B�  B��B��B�  B��B��B�{B�{B��B��
B��B�  B��C��C��C�HC�C

=C{C  C  C
=C
=C��C  C  C��C  C   C"
=C$  C&  C(
=C)��C,  C.
=C0  C2  C4
=C6{C8  C9��C<
=C>  C@
=CB  CD
=CF  CH
=CI�CL  CN  CO��CR  CT{CV
=CX  CY��C[��C^
=C`  Ca��Cd  Ce�Cg�Cj  Ck��Cn{Cp�Cr
=Ct  Cv
=Cw��Cz
=C|
=C}��C�  C�\C�C���C���C�C�
=C�\C�
=C�  C�C�
=C�C�  C���C�  C�C���C���C�  C�C�C���C�  C�
=C�C���C�  C�C�  C�  C�C�C�C�C�  C�  C���C���C�C�C�  C�  C�
=C�  C���C�  C�  C�  C�  C���C�  C�C�C�
=C�  C�  C�  C�C�C���C���C���C�  C�
=C���C��C���C���C���C���C�
=C�\C�C�  C�  C���C�  C�C�  C���C�C�  C���C���C�  C�  C�C�  C�  C�  C�  C���C���C�  C�C�
=C�C���C���C���C�  C�  C�C�  C�  C���C�  C�
=C�  C���C���C�  C���C���C���C���C�  C���C���C�  C���C�  C�  C�  C���C���C�  D �D � D�D�D�D� D  D� D  D}qD  D}qD  D}qD  D�D�D�D	  D	}qD
�D
�D�D}qD��D}qD  D}qD�qD��D��Dz�D�qD}qD  D��D�D� DD� D�D�D�qD}qD�D��D  D}qD�qD}qD��D� D�D�DD�DD}qD�qD�D  D��D�D��D D ��D �qD!� D"  D"��D#  D#z�D#��D$��D%  D%� D&�D&�D'�D'}qD(  D(� D)  D)��D*�D*}qD*�qD+z�D+��D,}qD,�qD-}qD-�qD.}qD.�qD/� D0�D0�D1  D1}qD2�D2� D3�D3��D4�D4� D5�D5��D5�qD6}qD7  D7�D8�D8}qD9�D9� D:  D:� D:�qD;� D<�D<}qD=  D=��D>  D>}qD>�qD?}qD?�qD@� DA�DA��DB�DB��DC  DC��DD  DD}qDD�qDE}qDE�qDF}qDF�qDG� DH  DH}qDI  DI� DJ  DJ� DK  DK� DLDL��DM�DM� DN  DN� DO  DO� DP  DP��DQ  DQ��DR�DR}qDS  DS��DT�DT� DT�qDU��DV  DVz�DW  DW��DX  DX}qDX�qDY� DZ  DZ� DZ�qD[z�D[�qD\}qD\�qD]� D^�D^� D_�D_� D_�qD`� D`�qDa}qDa�qDb� Dc�Dc� Dc�qDd}qDd��De� Df�Df� Dg  Dg�Dh�Dh� Dh�qDi� Di�qDj� Dk�Dk��Dl  Dl}qDl�qDm}qDn  Dn}qDn�qDo� Dp�Dp��Dq�Dq� Dr  Dr� Ds�Ds� Dt  Dt}qDt�qDu� DvDv��Dw  Dw��DxDx�Dy  Dy}qDy�qDz}qD{  D{� D{�qD|��D}�D}��D~�D~� D  D��D��D�AHD�� D�� D�HD�>�D�~�D�� D�  D�@ D��HD�D�  D�@ D��HD��HD�  D�AHD��HD���D���D�AHD��HD��HD�  D�@ D�� D��HD�  D�@ D�~�D��HD��D�@ D�� D��HD�  D�@ D�� D�� D�  D�AHD���D�� D���D�>�D�� D�� D�  D�@ D��HD�D�HD�AHD��HD�� D�HD�B�D�s3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?8Q�?u?��
?�
=?�@\)@&ff@333@L��@fff@p��@��
@��@�Q�@��
@�\)@�
=@�ff@У�@�Q�@�ff@�33@��HA�A	��A�RAA�A   A'�A,(�A2�\A9��A=p�ADz�AJ�HAO\)AU�A[�A_\)AeAmp�AqG�Aw
=A~{A�G�A�(�A�  A��A�(�A��A��\A�z�A��A�=qA�z�A�\)A�=qA�(�A�
=A�=qA���A��RA�=qA�z�A�
=A��\A��A�
=A�=qA��A�
=A��A���AθRA���A�(�A�{A�  A�33A�{A�  A�=qA�p�A�  A��A���A�A�A�(�A�
=A���A�33A�ffB Q�Bp�B�RB(�BG�BffB�
B	�B	�B\)B��Bp�B�RB(�B��B�B\)B  B��B=qB33B  B�B=qB�RB�
B��B�B{B
=B\)B   B!�B!p�B!�B#
=B#�B$  B$��B%B%�B&�RB'�B(  B(��B)��B)�B*�RB+�B,  B,��B-��B.=qB.�\B/33B0(�B0��B1G�B1�B3
=B3\)B3�B4��B5G�B5��B6ffB7\)B7�B8(�B9�B9G�B:{B:�HB;33B<  B<��B=G�B=B>�RB?\)B?�
B@Q�BAG�BA�BB=qBB�HBD  BDz�BD��BE�BF�\BF�HBH  BH��BH��BJ{BJ�RBK33BL  BM�BM��BN=qBO33BO�BPQ�BQ��BR=qBR�RBS\)BT��BUG�BUBV�RBW�BX  BX��BY�BZffB[33B\Q�B\��B]p�B^�\B_\)B_�
Ba�BaBb=qBc\)Bdz�Bd��BeBf�HBg�
BhQ�BiG�BjffBk\)Bl  Bl��Bn{Bn�HBo�Bp��Bq��Br=qBs\)BtQ�Bt��Bu�Bw
=Bw�Bxz�ByBz�\B{33B|(�B}G�B~{B~�RB�  B�z�B��HB�G�B��
B�Q�B���B�G�B��
B�  B���B��B�p�B�{B���B���B�\)B�  B��\B��HB�\)B�{B�z�B��HB�\)B�  B�z�B��HB�p�B�  B�ffB��HB��B�  B�ffB�
=B���B��B�ffB�
=B��B�  B�ffB�
=B���B��B��\B�33B��B�{B���B��B���B�=qB���B�33B�B�z�B���B�G�B��B�z�B��HB�\)B�  B��\B���B�p�B�{B��RB��B��B�=qB���B��B��B�Q�B��HB�G�B�B�ffB���B�33B��
B�z�B���B�\)B�  B��\B��HB�\)B�{B��\B���B�\)B�{B���B�
=B��B�(�B��RB�
=B��B�(�B���B���B��B�(�B���B���B��B�=qB��\B���B��B�=qB���B���B��B�(�B��\B�
=B���B�(�B��\B��HB�\)B��B��\B���B�G�B��
B�ffB£�B��B�B�=qB�z�B��HBŅB�  B�Q�BƸRB�\)B�B�{B�z�B��BɅB��
B�ffB���B�\)BˮB�(�Ḅ�B��B�p�B��
B�Q�B��HB��Bϙ�B�{BЏ\B��HB�G�B��
B�=qBҏ\B���B�\)B�  B�ffBԣ�B��BծB�(�B�z�B���B�G�B�B�Q�B؏\B���B�\)B��B�ffBڣ�B���Bۙ�B�{B�Q�BܸRB�33B�B�{B�ffB��HB�\)B߅B��B�z�B��HB��B�B�  B�z�B���B�
=B�B�  B�Q�B�\B���B�B�  B�=qB�z�B���B�B��
B�(�B��B��B�B��
B�=qB�RB�G�B�B��
B�Q�B���B��B�p�B�{B�\B���B�33BB�=qB��\B���B�\)B��B�Q�B�\B���B�p�B��B�Q�B��RB���B��B�{B�=qB��RB�33B��B�  B�Q�B��HB�\)B�B�{B�z�B���B�G�B���B�{B��\B���B�33B���B�{B�z�B���B��B��C {C G�C ffC ��C �C{C=qCz�C��C��C{C\)C�\C�RC�HC(�CffC�C�C�
C�CQ�CffC��C�HC{C33CffC��C�C�CQ�Cp�C��C  C(�C\)C�C��C�CG�C�\C�
C	
=C	33C	p�C	C	�HC
{C
\)C
��C
��C  CG�C�CC�HC(�Cz�C�C��C{CQ�Cz�C�C  C33C\)C�C��C{C33CffC�RC�HC  C=qCz�C�RC�HC
=CQ�C�\CC�C�CffC��CC��C33C�C�C�
C{C\)C�C�C��C33CffC��C��C�C\)C�\C�RC
=CQ�C�C�C�HC(�CffC�\C��C�C\)C��CC�C(�Cp�C�C�C�CG�C�\C�
C�CQ�C�CC{C\)C�C�RC��C=qC�\CC��C(�CffC�C�C {C Q�C ��C �
C!�C!\)C!�\C!�RC"  C"=qC"�C"��C#  C#(�C#p�C#�RC#��C$(�C$ffC$��C$�C%33C%ffC%��C%�
C&{C&ffC&��C&�
C'{C'\)C'�C'�HC({C(G�C(��C(�HC)�C)Q�C)�C)C)��C*=qC*�C*��C+
=C+=qC+p�C+�RC,
=C,Q�C,�\C,C,��C-G�C-�\C-�
C.{C.G�C.z�C.��C/�C/ffC/��C/��C0{C0\)C0�C0�C1(�C1\)C1�\C1�HC2(�C2ffC2��C2�
C3�C3p�C3�RC3�C4(�C4ffC4�RC4��C5G�C5�\C5��C6{C6Q�C6��C6�
C7{C7\)C7��C7�C833C8�C8��C9
=C9=qC9�C9C:  C:=qC:�C:�
C;{C;\)C;�C;�C<=qC<p�C<�RC<�C=33C=p�C=�RC=��C>G�C>�\C>�
C?�C?\)C?��C?�HC@{C@Q�C@�\C@��CA
=CAQ�CA��CA�CB=qCB�CB�
CC{CC\)CC��CC�HCD(�CDp�CD�CD��CE33CE�CE��CF{CFffCF�CF�CG=qCG�CG��CH
=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                     111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  ?��H@:�H@xQ�@��R@�  @�p�@�(�A{A\)A,��AAG�A`��A�  A�  A�  A�  A�  A�  A�Q�A�  A�\)B�B�
B  B Q�B((�B0  B8(�B@  BG�
BO�
BW�
B_�
Bh(�Bp(�Bx  B�  B��B��
B��
B�  B�{B�(�B�{B��B�  B�{B��B��B�  B�  B�  B��B��B��B�{B�  B��B��B�  B��B��B�{B�{B��B��
B��B�  B��C��C��C�HC�C

=C{C  C  C
=C
=C��C  C  C��C  C   C"
=C$  C&  C(
=C)��C,  C.
=C0  C2  C4
=C6{C8  C9��C<
=C>  C@
=CB  CD
=CF  CH
=CI�CL  CN  CO��CR  CT{CV
=CX  CY��C[��C^
=C`  Ca��Cd  Ce�Cg�Cj  Ck��Cn{Cp�Cr
=Ct  Cv
=Cw��Cz
=C|
=C}��C�  C�\C�C���C���C�C�
=C�\C�
=C�  C�C�
=C�C�  C���C�  C�C���C���C�  C�C�C���C�  C�
=C�C���C�  C�C�  C�  C�C�C�C�C�  C�  C���C���C�C�C�  C�  C�
=C�  C���C�  C�  C�  C�  C���C�  C�C�C�
=C�  C�  C�  C�C�C���C���C���C�  C�
=C���C��C���C���C���C���C�
=C�\C�C�  C�  C���C�  C�C�  C���C�C�  C���C���C�  C�  C�C�  C�  C�  C�  C���C���C�  C�C�
=C�C���C���C���C�  C�  C�C�  C�  C���C�  C�
=C�  C���C���C�  C���C���C���C���C�  C���C���C�  C���C�  C�  C�  C���C���C�  D �D � D�D�D�D� D  D� D  D}qD  D}qD  D}qD  D�D�D�D	  D	}qD
�D
�D�D}qD��D}qD  D}qD�qD��D��Dz�D�qD}qD  D��D�D� DD� D�D�D�qD}qD�D��D  D}qD�qD}qD��D� D�D�DD�DD}qD�qD�D  D��D�D��D D ��D �qD!� D"  D"��D#  D#z�D#��D$��D%  D%� D&�D&�D'�D'}qD(  D(� D)  D)��D*�D*}qD*�qD+z�D+��D,}qD,�qD-}qD-�qD.}qD.�qD/� D0�D0�D1  D1}qD2�D2� D3�D3��D4�D4� D5�D5��D5�qD6}qD7  D7�D8�D8}qD9�D9� D:  D:� D:�qD;� D<�D<}qD=  D=��D>  D>}qD>�qD?}qD?�qD@� DA�DA��DB�DB��DC  DC��DD  DD}qDD�qDE}qDE�qDF}qDF�qDG� DH  DH}qDI  DI� DJ  DJ� DK  DK� DLDL��DM�DM� DN  DN� DO  DO� DP  DP��DQ  DQ��DR�DR}qDS  DS��DT�DT� DT�qDU��DV  DVz�DW  DW��DX  DX}qDX�qDY� DZ  DZ� DZ�qD[z�D[�qD\}qD\�qD]� D^�D^� D_�D_� D_�qD`� D`�qDa}qDa�qDb� Dc�Dc� Dc�qDd}qDd��De� Df�Df� Dg  Dg�Dh�Dh� Dh�qDi� Di�qDj� Dk�Dk��Dl  Dl}qDl�qDm}qDn  Dn}qDn�qDo� Dp�Dp��Dq�Dq� Dr  Dr� Ds�Ds� Dt  Dt}qDt�qDu� DvDv��Dw  Dw��DxDx�Dy  Dy}qDy�qDz}qD{  D{� D{�qD|��D}�D}��D~�D~� D  D��D��D�AHD�� D�� D�HD�>�D�~�D�� D�  D�@ D��HD�D�  D�@ D��HD��HD�  D�AHD��HD���D���D�AHD��HD��HD�  D�@ D�� D��HD�  D�@ D�~�D��HD��D�@ D�� D��HD�  D�@ D�� D�� D�  D�AHD���D�� D���D�>�D�� D�� D�  D�@ D��HD�D�HD�AHD��HD�� D�HD�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?8Q�?u?��
?�
=?�@\)@&ff@333@L��@fff@p��@��
@��@�Q�@��
@�\)@�
=@�ff@У�@�Q�@�ff@�33@��HA�A	��A�RAA�A   A'�A,(�A2�\A9��A=p�ADz�AJ�HAO\)AU�A[�A_\)AeAmp�AqG�Aw
=A~{A�G�A�(�A�  A��A�(�A��A��\A�z�A��A�=qA�z�A�\)A�=qA�(�A�
=A�=qA���A��RA�=qA�z�A�
=A��\A��A�
=A�=qA��A�
=A��A���AθRA���A�(�A�{A�  A�33A�{A�  A�=qA�p�A�  A��A���A�A�A�(�A�
=A���A�33A�ffB Q�Bp�B�RB(�BG�BffB�
B	�B	�B\)B��Bp�B�RB(�B��B�B\)B  B��B=qB33B  B�B=qB�RB�
B��B�B{B
=B\)B   B!�B!p�B!�B#
=B#�B$  B$��B%B%�B&�RB'�B(  B(��B)��B)�B*�RB+�B,  B,��B-��B.=qB.�\B/33B0(�B0��B1G�B1�B3
=B3\)B3�B4��B5G�B5��B6ffB7\)B7�B8(�B9�B9G�B:{B:�HB;33B<  B<��B=G�B=B>�RB?\)B?�
B@Q�BAG�BA�BB=qBB�HBD  BDz�BD��BE�BF�\BF�HBH  BH��BH��BJ{BJ�RBK33BL  BM�BM��BN=qBO33BO�BPQ�BQ��BR=qBR�RBS\)BT��BUG�BUBV�RBW�BX  BX��BY�BZffB[33B\Q�B\��B]p�B^�\B_\)B_�
Ba�BaBb=qBc\)Bdz�Bd��BeBf�HBg�
BhQ�BiG�BjffBk\)Bl  Bl��Bn{Bn�HBo�Bp��Bq��Br=qBs\)BtQ�Bt��Bu�Bw
=Bw�Bxz�ByBz�\B{33B|(�B}G�B~{B~�RB�  B�z�B��HB�G�B��
B�Q�B���B�G�B��
B�  B���B��B�p�B�{B���B���B�\)B�  B��\B��HB�\)B�{B�z�B��HB�\)B�  B�z�B��HB�p�B�  B�ffB��HB��B�  B�ffB�
=B���B��B�ffB�
=B��B�  B�ffB�
=B���B��B��\B�33B��B�{B���B��B���B�=qB���B�33B�B�z�B���B�G�B��B�z�B��HB�\)B�  B��\B���B�p�B�{B��RB��B��B�=qB���B��B��B�Q�B��HB�G�B�B�ffB���B�33B��
B�z�B���B�\)B�  B��\B��HB�\)B�{B��\B���B�\)B�{B���B�
=B��B�(�B��RB�
=B��B�(�B���B���B��B�(�B���B���B��B�=qB��\B���B��B�=qB���B���B��B�(�B��\B�
=B���B�(�B��\B��HB�\)B��B��\B���B�G�B��
B�ffB£�B��B�B�=qB�z�B��HBŅB�  B�Q�BƸRB�\)B�B�{B�z�B��BɅB��
B�ffB���B�\)BˮB�(�Ḅ�B��B�p�B��
B�Q�B��HB��Bϙ�B�{BЏ\B��HB�G�B��
B�=qBҏ\B���B�\)B�  B�ffBԣ�B��BծB�(�B�z�B���B�G�B�B�Q�B؏\B���B�\)B��B�ffBڣ�B���Bۙ�B�{B�Q�BܸRB�33B�B�{B�ffB��HB�\)B߅B��B�z�B��HB��B�B�  B�z�B���B�
=B�B�  B�Q�B�\B���B�B�  B�=qB�z�B���B�B��
B�(�B��B��B�B��
B�=qB�RB�G�B�B��
B�Q�B���B��B�p�B�{B�\B���B�33BB�=qB��\B���B�\)B��B�Q�B�\B���B�p�B��B�Q�B��RB���B��B�{B�=qB��RB�33B��B�  B�Q�B��HB�\)B�B�{B�z�B���B�G�B���B�{B��\B���B�33B���B�{B�z�B���B��B��C {C G�C ffC ��C �C{C=qCz�C��C��C{C\)C�\C�RC�HC(�CffC�C�C�
C�CQ�CffC��C�HC{C33CffC��C�C�CQ�Cp�C��C  C(�C\)C�C��C�CG�C�\C�
C	
=C	33C	p�C	C	�HC
{C
\)C
��C
��C  CG�C�CC�HC(�Cz�C�C��C{CQ�Cz�C�C  C33C\)C�C��C{C33CffC�RC�HC  C=qCz�C�RC�HC
=CQ�C�\CC�C�CffC��CC��C33C�C�C�
C{C\)C�C�C��C33CffC��C��C�C\)C�\C�RC
=CQ�C�C�C�HC(�CffC�\C��C�C\)C��CC�C(�Cp�C�C�C�CG�C�\C�
C�CQ�C�CC{C\)C�C�RC��C=qC�\CC��C(�CffC�C�C {C Q�C ��C �
C!�C!\)C!�\C!�RC"  C"=qC"�C"��C#  C#(�C#p�C#�RC#��C$(�C$ffC$��C$�C%33C%ffC%��C%�
C&{C&ffC&��C&�
C'{C'\)C'�C'�HC({C(G�C(��C(�HC)�C)Q�C)�C)C)��C*=qC*�C*��C+
=C+=qC+p�C+�RC,
=C,Q�C,�\C,C,��C-G�C-�\C-�
C.{C.G�C.z�C.��C/�C/ffC/��C/��C0{C0\)C0�C0�C1(�C1\)C1�\C1�HC2(�C2ffC2��C2�
C3�C3p�C3�RC3�C4(�C4ffC4�RC4��C5G�C5�\C5��C6{C6Q�C6��C6�
C7{C7\)C7��C7�C833C8�C8��C9
=C9=qC9�C9C:  C:=qC:�C:�
C;{C;\)C;�C;�C<=qC<p�C<�RC<�C=33C=p�C=�RC=��C>G�C>�\C>�
C?�C?\)C?��C?�HC@{C@Q�C@�\C@��CA
=CAQ�CA��CA�CB=qCB�CB�
CC{CC\)CC��CC�HCD(�CDp�CD�CD��CE33CE�CE��CF{CFffCF�CF�CG=qCG�CG��CH
=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                     111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A��A��A�VA��A�+A�(�A�$�A�/A�1'A�33A�1'A�33A�33A�1'A�1'A�33A�33A�1'A�1'A�33A�/A�33A�9XA�7LA�;dA�=qA�=qA�A�A�I�A��AͰ!A�`BA��Ḁ�ÃA�M�A�(�A��A�JA���A��HAʓuA�n�A�jA�I�A�G�A�$�AɾwAɗ�A�~�A�v�A�dZA�C�A�oA��HAȧ�A�XA���AƲ-A�K�A�
=A���A��yA��A���A��HA��/A�7LA���A�%A�VA�33A���A�ȴA�%A���A�^5A��yA�XA�+A��jA��A�hsA�  A�bNA��yA��;A�oA�A��`A���A��A��^A��;A���A��mA��HA��PA�+A�dZA�1A�?}A��hA�VA��A�VA���A�+A�t�A�t�A�$�A��A�JA���A��A�
=A�9XA���A��yA�33A��A}��A{��Ay&�AuO�Ar�+An��Ak�AjQ�Af(�Abv�A`9XAZ�DAX�uATffAR^5AO��AK33AI/AF��AEG�AA��A<�+A;33A:�HA:jA:�A9VA6M�A4z�A2Q�A1`BA0�9A/`BA.I�A-�#A+A(�A'�A&��A&�+A%33A#�FA#+A"�9A!A ~�A �A�A��A1AA�uA�A��AoAoA��A�#A�A-AƨAC�A�jAffA1'A �A  A;dA�^A%A�uAE�A9XA=qAA�A{A�#Ax�A�9AffA1A�AƨA��AdZA��AA�A�FA
��A
�+A
(�A	�hA��A1'A  AƨA��AXA�AƨA��A$�A��A�A�`A�A�TA�A��A��AC�A ȴA ��A Q�@�ƨ@�p�@��
@�+@�V@�5?@�@��P@��+@��/@�j@�dZ@�$�@��@��^@�\)@�h@�@睲@�;d@�!@���@�p�@�@�33@��@�@���@�"�@ݡ�@�j@܃@�9X@��@�ƨ@��H@ڇ+@�x�@���@؋D@�bN@��@֗�@�v�@�V@��@�@���@���@�1'@�A�@�  @ӶF@�t�@�@�ff@��T@љ�@�`B@��@Л�@��;@϶F@υ@��H@θR@�^5@͑h@�`B@�&�@̴9@�9X@�dZ@�n�@�M�@��@��T@ɲ-@�G�@ȋD@�Q�@�A�@Ǿw@�;d@��@�v�@�5?@��@�@�`B@Ĭ@�Z@�b@���@��m@��
@î@ÍP@�+@�5?@�X@��j@�r�@�  @��;@��@��P@�|�@�C�@��+@�^5@�E�@�=q@�@�7L@�Q�@���@�C�@�"�@�
=@�@���@�-@�@��h@�hs@�&�@�V@��`@� �@�l�@��R@�E�@�?}@���@��@��`@��@�1'@��w@���@�l�@�C�@��!@�E�@��@���@�?}@��@�bN@��;@��@�;d@���@�ff@�5?@�J@���@��h@�X@��`@��@�ƨ@��@�K�@�
=@��!@�ff@�{@�7L@���@�Ĝ@��D@�b@���@�\)@��@���@�-@�{@��T@�X@���@���@��u@�Q�@��;@��F@���@��P@�|�@�dZ@�C�@��H@�=q@���@��7@�X@��@��@�Q�@��w@�t�@�\)@�33@�o@�
=@�@���@��y@���@�n�@�M�@�E�@�{@���@���@�`B@���@�bN@��
@���@�\)@�o@��!@�n�@��@��T@���@���@�7L@��@�bN@�A�@�b@���@���@�C�@�o@���@��y@���@�{@���@�X@��@��@�1'@�1@��F@�dZ@�33@��@�~�@�-@���@���@���@�X@�/@�V@��`@�z�@�(�@�  @��@��F@�;d@���@���@��h@�?}@��@�z�@�I�@��@�t�@���@�J@���@���@�x�@�X@�/@���@��j@��9@���@�bN@�(�@���@��@�t�@�dZ@�;d@��@���@��@��@��-@�X@���@��@�A�@�1'@�b@�1@�  @���@��@��@���@��F@���@��@�C�@��y@�
=@�33@�@�ȴ@��\@�5?@��@���@�G�@���@���@���@��D@�(�@��@��@|�@�@}�@}�h@}O�@|��@|j@|9XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A��A��A��A��A�{A�{A�{A�JA�JA��A��A�$�A�+A�+A�-A�-A�$�A�-A�(�A�"�A�&�A�+A�/A�33A�1'A�/A�33A�1'A�/A�5?A�/A�33A�5?A�/A�33A�5?A�/A�1'A�7LA�/A�1'A�33A�/A�1'A�33A�/A�/A�33A�/A�/A�33A�33A�/A�33A�5?A�1'A�5?A�5?A�1'A�33A�5?A�1'A�1'A�5?A�33A�/A�33A�1'A�/A�1'A�33A�-A�1'A�33A�/A�/A�7LA�33A�1'A�5?A�5?A�1'A�/A�5?A�33A�/A�33A�1'A�-A�/A�33A�1'A�/A�5?A�33A�33A�7LA�;dA�7LA�7LA�=qA�;dA�5?A�9XA�7LA�5?A�7LA�;dA�5?A�5?A�;dA�=qA�9XA�=qA�;dA�9XA�=qA�=qA�9XA�=qA�=qA�9XA�;dA�?}A�;dA�;dA�?}A�;dA�9XA�=qA�=qA�9XA�=qA�=qA�9XA�;dA�?}A�;dA�=qA�A�A�;dA�=qA�A�A�=qA�=qA�C�A�A�A�?}A�?}A�A�A�A�A�?}A�A�A�C�A�A�A�I�A�XA�VA�S�A�VA�XA�Q�A�Q�A�Q�A�I�A�E�A�;dA�+A��A�JA��A��TA��
A���A���A���A���A���A�ĜA�ĜA�ƨA�Aͺ^A͸RAͶFAͮAͰ!AͰ!Aͧ�Aͩ�Aͧ�A͛�A͙�A͓uA̓A�l�A�n�A�bNA�^5A�M�A�C�A�$�A�$�A��A�{A�
=A�%A��A��HA��#A���A���A̴9A̴9A̲-A̬A̲-A̧�A̡�A̟�A̟�A̙�A̛�A̟�A̙�A̗�ȂhA�v�A�p�A�t�A�x�A�x�A�p�A�l�A�hsA�\)A�XA�M�A�5?A�-A�1'A�1'A�&�A�&�A�+A�+A�(�A�(�A�-A� �A�"�A�"�A��A��A��A��A�{A�oA�{A�oA�JA�bA�VA�%A�
=A�
=A�A�A���A��A��A���A��A˝�A�Q�A�1'A��A���A��`A���A�ĜAʸRAʲ-Aʩ�Aʥ�Aʧ�Aʡ�Aʙ�AʓuAʃA�p�A�jA�jA�n�A�p�A�n�A�n�A�p�A�p�A�l�A�r�A�l�A�dZA�n�A�n�A�hsA�^5A�^5A�\)A�S�A�M�A�C�A�?}A�7LA�?}A�A�A�9XA�=qA�E�A�S�A�VA�S�A�S�A�G�A�;dA�1'A�(�A��A�  A���A��A��TA���Aɺ^Aɴ9AɬAɡ�Aɡ�Aɣ�Aɝ�Aə�Aɝ�Aɛ�AɓuAɇ+AɅAɅA�~�A�|�A�~�A�~�A�z�A�x�A�z�A�|�A�x�A�x�A�|�A�x�A�p�A�n�A�n�A�l�A�ffA�ffA�hsA�bNA�^5A�\)A�XA�Q�A�K�A�K�A�G�A�?}A�5?A�&�A��A��A�VA�VA�VA�bA�bA�JA�VA�bA�A���A��A��`A�ĜAȰ!AȮAȰ!AȬAȩ�AȮAȬAȧ�Aȣ�Aȥ�Aȡ�Aț�Aȝ�AȅA�l�A�VA�I�A�E�A�=qA�/A��A�
=A�  A��A��
A�ȴAǴ9AǬAǏ\A�r�A�7LA��AƾwAƥ�Aƣ�Aƕ�AƉ7A�z�A�bNA�dZA�\)A�G�A�E�A�G�A�M�A�O�A�K�A�E�A�9XA��A�{A�VA�JA�JA�A�  A���A���A���A��A��mA��;A���A���A���Aź^A�|�A�^5A�K�A�1'A�
=A���A��A��yA��;A���A�AĲ-Aė�Aď\A�ffA�7LA��A�JA�A���A���A��`AüjA�x�A�bNA�Q�A�G�A�oA���A£�AA�r�A�ffA�XA�;dA��A�  A���A��
A��7A�7LA�"�A���A��A��mA��mA��TA��A���A�ƨA��wA��7A�VA�K�A�7LA�1'A�$�A�"�A�&�A�$�A�"�A��A��A� �A��A��A�{A�%A��HA��jA���A�v�A�oA���A��uA��A��+A�Q�A�C�A�A�A�A�A�33A� �A�"�A�$�A��A�oA�A��yA��TA��
A���A�ƨA��FA���A��A�33A��#A���A�l�A�1A���A���A�;dA��wA�n�A�=qA��A�S�A���A��/A��9A�XA��A��\A�`BA�E�A�9XA�;dA�7LA�/A�-A��A�
=A���A��A��yA��/A���A��!A���A���A���A��uA��PA��7A��A��A�~�A�r�A�l�A�ffA�\)A�M�A�A�A�9XA�1'A�&�A��A�bA�A���A�ȴA��-A��-A��A��hA���A���A���A�p�A�ZA��A��A�bA��A��TA��A���A��A�~�A��A�t�A�JA��RA�dZA�5?A���A��
A���A�v�A�1'A��A�bA�%A���A��A��A��A��;A���A�ȴA�ĜA��FA���A�|�A�dZA�G�A�33A�$�A�
=A��A��/A�ƨA�`BA��HA�r�A���A���A��A��hA�r�A�dZA�`BA�XA�G�A�33A�$�A� �A��A�{A�1A��A�ȴA��FA���A�^5A�=qA� �A�
=A��A���A��PA�p�A�`BA�S�A�K�A�C�A�-A�JA��;A�ȴA���A��+A�dZA�A�A�&�A���A��FA���A�~�A�dZA�M�A�33A�{A���A���A��+A�p�A�VA�C�A�9XA�(�A�oA�  A���A�l�A�9XA�%A��
A���A�n�A�S�A�/A�
=A��A��mA��#A���A��wA��A���A��A�t�A�C�A�JA��;A��wA���A��7A�r�A�\)A�G�A�/A�{A��A�ȴA��A���A��\A�x�A�+A�JA��
A���A�dZA�VA��A��mA��HA��;A��/A��#A��A���A���A��hA��A��A�v�A�p�A�l�A�bNA�VA�E�A�33A��A���A���A��A���A���A��A�\)A�+A��A�bA�bA�
=A�
=A�1A�
=A�%A���A��A��^A���A�E�A��A�
=A��A�ȴA�r�A��;A�G�A��A��A���A�;dA��A���A�v�A�\)A�C�A�"�A�bA�%A���A��A��`A���A�ƨA���A��FA���A��uA�t�A�^5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                     111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A�VA��A�+A�(�A�$�A�/A�1'A�33A�1'A�33A�33A�1'A�1'A�33A�33A�1'A�1'A�33A�/A�33A�9XA�7LA�;dA�=qA�=qA�A�A�I�A��AͰ!A�`BA��Ḁ�ÃA�M�A�(�A��A�JA���A��HAʓuA�n�A�jA�I�A�G�A�$�AɾwAɗ�A�~�A�v�A�dZA�C�A�oA��HAȧ�A�XA���AƲ-A�K�A�
=A���A��yA��A���A��HA��/A�7LA���A�%A�VA�33A���A�ȴA�%A���A�^5A��yA�XA�+A��jA��A�hsA�  A�bNA��yA��;A�oA�A��`A���A��A��^A��;A���A��mA��HA��PA�+A�dZA�1A�?}A��hA�VA��A�VA���A�+A�t�A�t�A�$�A��A�JA���A��A�
=A�9XA���A��yA�33A��A}��A{��Ay&�AuO�Ar�+An��Ak�AjQ�Af(�Abv�A`9XAZ�DAX�uATffAR^5AO��AK33AI/AF��AEG�AA��A<�+A;33A:�HA:jA:�A9VA6M�A4z�A2Q�A1`BA0�9A/`BA.I�A-�#A+A(�A'�A&��A&�+A%33A#�FA#+A"�9A!A ~�A �A�A��A1AA�uA�A��AoAoA��A�#A�A-AƨAC�A�jAffA1'A �A  A;dA�^A%A�uAE�A9XA=qAA�A{A�#Ax�A�9AffA1A�AƨA��AdZA��AA�A�FA
��A
�+A
(�A	�hA��A1'A  AƨA��AXA�AƨA��A$�A��A�A�`A�A�TA�A��A��AC�A ȴA ��A Q�@�ƨ@�p�@��
@�+@�V@�5?@�@��P@��+@��/@�j@�dZ@�$�@��@��^@�\)@�h@�@睲@�;d@�!@���@�p�@�@�33@��@�@���@�"�@ݡ�@�j@܃@�9X@��@�ƨ@��H@ڇ+@�x�@���@؋D@�bN@��@֗�@�v�@�V@��@�@���@���@�1'@�A�@�  @ӶF@�t�@�@�ff@��T@љ�@�`B@��@Л�@��;@϶F@υ@��H@θR@�^5@͑h@�`B@�&�@̴9@�9X@�dZ@�n�@�M�@��@��T@ɲ-@�G�@ȋD@�Q�@�A�@Ǿw@�;d@��@�v�@�5?@��@�@�`B@Ĭ@�Z@�b@���@��m@��
@î@ÍP@�+@�5?@�X@��j@�r�@�  @��;@��@��P@�|�@�C�@��+@�^5@�E�@�=q@�@�7L@�Q�@���@�C�@�"�@�
=@�@���@�-@�@��h@�hs@�&�@�V@��`@� �@�l�@��R@�E�@�?}@���@��@��`@��@�1'@��w@���@�l�@�C�@��!@�E�@��@���@�?}@��@�bN@��;@��@�;d@���@�ff@�5?@�J@���@��h@�X@��`@��@�ƨ@��@�K�@�
=@��!@�ff@�{@�7L@���@�Ĝ@��D@�b@���@�\)@��@���@�-@�{@��T@�X@���@���@��u@�Q�@��;@��F@���@��P@�|�@�dZ@�C�@��H@�=q@���@��7@�X@��@��@�Q�@��w@�t�@�\)@�33@�o@�
=@�@���@��y@���@�n�@�M�@�E�@�{@���@���@�`B@���@�bN@��
@���@�\)@�o@��!@�n�@��@��T@���@���@�7L@��@�bN@�A�@�b@���@���@�C�@�o@���@��y@���@�{@���@�X@��@��@�1'@�1@��F@�dZ@�33@��@�~�@�-@���@���@���@�X@�/@�V@��`@�z�@�(�@�  @��@��F@�;d@���@���@��h@�?}@��@�z�@�I�@��@�t�@���@�J@���@���@�x�@�X@�/@���@��j@��9@���@�bN@�(�@���@��@�t�@�dZ@�;d@��@���@��@��@��-@�X@���@��@�A�@�1'@�b@�1@�  @���@��@��@���@��F@���@��@�C�@��y@�
=@�33@�@�ȴ@��\@�5?@��@���@�G�@���@���@���@��D@�(�@��@��@|�@�@}�@}�h@}O�@|��@|jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A��A��A��A��A�{A�{A�{A�JA�JA��A��A�$�A�+A�+A�-A�-A�$�A�-A�(�A�"�A�&�A�+A�/A�33A�1'A�/A�33A�1'A�/A�5?A�/A�33A�5?A�/A�33A�5?A�/A�1'A�7LA�/A�1'A�33A�/A�1'A�33A�/A�/A�33A�/A�/A�33A�33A�/A�33A�5?A�1'A�5?A�5?A�1'A�33A�5?A�1'A�1'A�5?A�33A�/A�33A�1'A�/A�1'A�33A�-A�1'A�33A�/A�/A�7LA�33A�1'A�5?A�5?A�1'A�/A�5?A�33A�/A�33A�1'A�-A�/A�33A�1'A�/A�5?A�33A�33A�7LA�;dA�7LA�7LA�=qA�;dA�5?A�9XA�7LA�5?A�7LA�;dA�5?A�5?A�;dA�=qA�9XA�=qA�;dA�9XA�=qA�=qA�9XA�=qA�=qA�9XA�;dA�?}A�;dA�;dA�?}A�;dA�9XA�=qA�=qA�9XA�=qA�=qA�9XA�;dA�?}A�;dA�=qA�A�A�;dA�=qA�A�A�=qA�=qA�C�A�A�A�?}A�?}A�A�A�A�A�?}A�A�A�C�A�A�A�I�A�XA�VA�S�A�VA�XA�Q�A�Q�A�Q�A�I�A�E�A�;dA�+A��A�JA��A��TA��
A���A���A���A���A���A�ĜA�ĜA�ƨA�Aͺ^A͸RAͶFAͮAͰ!AͰ!Aͧ�Aͩ�Aͧ�A͛�A͙�A͓uA̓A�l�A�n�A�bNA�^5A�M�A�C�A�$�A�$�A��A�{A�
=A�%A��A��HA��#A���A���A̴9A̴9A̲-A̬A̲-A̧�A̡�A̟�A̟�A̙�A̛�A̟�A̙�A̗�ȂhA�v�A�p�A�t�A�x�A�x�A�p�A�l�A�hsA�\)A�XA�M�A�5?A�-A�1'A�1'A�&�A�&�A�+A�+A�(�A�(�A�-A� �A�"�A�"�A��A��A��A��A�{A�oA�{A�oA�JA�bA�VA�%A�
=A�
=A�A�A���A��A��A���A��A˝�A�Q�A�1'A��A���A��`A���A�ĜAʸRAʲ-Aʩ�Aʥ�Aʧ�Aʡ�Aʙ�AʓuAʃA�p�A�jA�jA�n�A�p�A�n�A�n�A�p�A�p�A�l�A�r�A�l�A�dZA�n�A�n�A�hsA�^5A�^5A�\)A�S�A�M�A�C�A�?}A�7LA�?}A�A�A�9XA�=qA�E�A�S�A�VA�S�A�S�A�G�A�;dA�1'A�(�A��A�  A���A��A��TA���Aɺ^Aɴ9AɬAɡ�Aɡ�Aɣ�Aɝ�Aə�Aɝ�Aɛ�AɓuAɇ+AɅAɅA�~�A�|�A�~�A�~�A�z�A�x�A�z�A�|�A�x�A�x�A�|�A�x�A�p�A�n�A�n�A�l�A�ffA�ffA�hsA�bNA�^5A�\)A�XA�Q�A�K�A�K�A�G�A�?}A�5?A�&�A��A��A�VA�VA�VA�bA�bA�JA�VA�bA�A���A��A��`A�ĜAȰ!AȮAȰ!AȬAȩ�AȮAȬAȧ�Aȣ�Aȥ�Aȡ�Aț�Aȝ�AȅA�l�A�VA�I�A�E�A�=qA�/A��A�
=A�  A��A��
A�ȴAǴ9AǬAǏ\A�r�A�7LA��AƾwAƥ�Aƣ�Aƕ�AƉ7A�z�A�bNA�dZA�\)A�G�A�E�A�G�A�M�A�O�A�K�A�E�A�9XA��A�{A�VA�JA�JA�A�  A���A���A���A��A��mA��;A���A���A���Aź^A�|�A�^5A�K�A�1'A�
=A���A��A��yA��;A���A�AĲ-Aė�Aď\A�ffA�7LA��A�JA�A���A���A��`AüjA�x�A�bNA�Q�A�G�A�oA���A£�AA�r�A�ffA�XA�;dA��A�  A���A��
A��7A�7LA�"�A���A��A��mA��mA��TA��A���A�ƨA��wA��7A�VA�K�A�7LA�1'A�$�A�"�A�&�A�$�A�"�A��A��A� �A��A��A�{A�%A��HA��jA���A�v�A�oA���A��uA��A��+A�Q�A�C�A�A�A�A�A�33A� �A�"�A�$�A��A�oA�A��yA��TA��
A���A�ƨA��FA���A��A�33A��#A���A�l�A�1A���A���A�;dA��wA�n�A�=qA��A�S�A���A��/A��9A�XA��A��\A�`BA�E�A�9XA�;dA�7LA�/A�-A��A�
=A���A��A��yA��/A���A��!A���A���A���A��uA��PA��7A��A��A�~�A�r�A�l�A�ffA�\)A�M�A�A�A�9XA�1'A�&�A��A�bA�A���A�ȴA��-A��-A��A��hA���A���A���A�p�A�ZA��A��A�bA��A��TA��A���A��A�~�A��A�t�A�JA��RA�dZA�5?A���A��
A���A�v�A�1'A��A�bA�%A���A��A��A��A��;A���A�ȴA�ĜA��FA���A�|�A�dZA�G�A�33A�$�A�
=A��A��/A�ƨA�`BA��HA�r�A���A���A��A��hA�r�A�dZA�`BA�XA�G�A�33A�$�A� �A��A�{A�1A��A�ȴA��FA���A�^5A�=qA� �A�
=A��A���A��PA�p�A�`BA�S�A�K�A�C�A�-A�JA��;A�ȴA���A��+A�dZA�A�A�&�A���A��FA���A�~�A�dZA�M�A�33A�{A���A���A��+A�p�A�VA�C�A�9XA�(�A�oA�  A���A�l�A�9XA�%A��
A���A�n�A�S�A�/A�
=A��A��mA��#A���A��wA��A���A��A�t�A�C�A�JA��;A��wA���A��7A�r�A�\)A�G�A�/A�{A��A�ȴA��A���A��\A�x�A�+A�JA��
A���A�dZA�VA��A��mA��HA��;A��/A��#A��A���A���A��hA��A��A�v�A�p�A�l�A�bNA�VA�E�A�33A��A���A���A��A���A���A��A�\)A�+A��A�bA�bA�
=A�
=A�1A�
=A�%A���A��A��^A���A�E�A��A�
=A��A�ȴA�r�A��;A�G�A��A��A���A�;dA��A���A�v�A�\)A�C�A�"�A�bA�%A���A��A��`A���A�ƨA���A��FA���A��uA�t�A�^5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                     111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B
�)B
��B
�5B
�]B
�B
�B
�cB
�]B
�B
�]B
��B
�)B
�]B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�cB
�B
�B
�cB
��B
�MB-wBS&BYBW
BK^BHKB[�Bt�Br�Bq�BqABwfBffBg�Bv�B��B�B��B�B�zB��B�hB�:B��B��B�4B�tB�tB��B��B�BƨBȴB�tB�aB��B�B�B�BqB7B+6B(�B,�BGBN�BM�BMjBM6BNBOBT�BZ�BVBU�BZQBS&BPHBOvBI�BE9B?}B8�B0�B&B"4BeB@B�BoB��B�TB�WB��B�KB�nB�eB�nB��B�_Bt�BK�B)_B&B"�B~B�B
�B
��B
��B
�7B
iDB
b�B
U2B
F�B
<�B
!B
B	�B	�HB	�,B	��B	��B	��B	��B	s�B	kQB	TaB	P�B	,qB	"�B	{B	�B	�B		7B��B�B�	B��B��B��B�rB��B	bB	B	$B	@B	�B	�B	B	#B	 �B	 �B	$tB	OB	7B	=B	CB	�B	eB	+B	�B	(B	(B	"B	�B	IB	&�B	$�B	H�B	T�B	Y�B	\�B	\�B	_�B	`�B	aHB	aB	`vB	_�B	jKB	qB	qB	p;B	p;B	o�B	p�B	sB	v�B	{JB	�lB	�SB	��B	�CB	��B	��B	��B	�hB	�*B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�zB	�9B	��B	��B	�aB	��B	��B	��B	��B	��B	�LB	�FB	��B	��B	�XB	�qB	�OB	�9B	��B	�RB	�9B	�qB	�B	��B	��B	��B	��B	�#B	ɆB	�?B	�B	ȀB	��B	�B	��B	ĜB	ȀB	��B	ɺB	�B	��B	ȀB	��B	�KB	�3B	��B	��B	�B	��B	�tB	�)B	�B	�B	҉B	�sB	خB	��B	�WB	��B	�#B	�WB	ٴB	خB	�/B	��B	�B	�dB	�B	ܒB	�/B	�/B	��B	یB	��B	רB	�B	چB	�]B	�dB	ޞB	�B	ߤB	�B	�TB	�B	�&B	�B	�TB	�ZB	�B	�B	�`B	�`B	��B	�B	�8B	�B	�sB	�yB	�B	�B	�B	��B	��B	�]B	�]B	��B	��B	�B	�vB	�AB	��B	�vB	�B	��B	�|B	�|B	�B	�`B	�B	�B	�>B	�rB	�xB	�xB	�DB	�DB	��B	�"B	��B	�"B	��B	��B	�cB
oB
�B
B
uB
�B
�B
�B
�B
�B
�B
+B
�B
+B
+B
	B
	�B
	lB
	lB

	B

�B
xB
�B
�B
�B
�B
�B
�B
�B
(B
(B
�B
�B
�B
�B
4B
�B
:B
�B
�B
�B
B
�B
�B
$B
�B
�B
1B
eB
7B
7B
	B
�B
�B
xB
B
OB
B
�B
!B
�B
�B
 \B
!bB
!�B
!�B
!�B
"�B
"�B
!�B
#B
#:B
$�B
$�B
%B
$�B
$�B
%B
$�B
&B
'B
'�B
'�B
($B
(�B
)*B
)�B
*�B
+B
+6B
+�B
+�B
+�B
+�B
+�B
+�B
,=B
,�B
,�B
,qB
-B
-CB
-CB
-wB
.B
/B
/�B
/�B
0!B
0UB
0UB
1'B
1�B
1�B
1�B
1�B
1�B
1'B
0UB
0�B
2aB
2-B
1�B
1�B
1�B
1[B
1�B
1�B
2aB
3hB
4B
4�B
4�B
49B
4nB
5�B
6zB
6zB
7B
7�B
8B
8RB
8�B
8�B
9$B
9�B
9�B
9�B
:^B
:�B
:�B
:�B
:�B
;dB
<6B
<B
<�B
=�B
>�B
>�B
>�B
?HB
?}B
@�B
A�B
A�B
A�B
A�B
A�B
B'B
B�B
B[B
B[B
B[B
B�B
B�B
B�B
C-B
B�B
B�B
C-B
CaB
C�B
D3B
C�B
D3B
D3B
EB
E�B
E�B
E�B
FB
E�B
F?B
F�B
G�B
G�B
G�B
H�B
J#B
JXB
K�B
K^B
L0B
NpB
O�B
O�B
PB
P�B
P�B
QNB
RTB
R B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S[B
TaB
T,B
T,B
TaB
T,B
T�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@aԕB
�yB
�B
�/B
�5B
�5B
�B
� B
�;B
�5B
��B
�iB
��B
��B
��B
��B
�B
�B
��B
� B
�B
��B
��B
�B
��B
��B
�B
��B
�B
�/B
�B
�B
�]B
�WB
�cB
�B
�B
�/B
�]B
�"B
�B
��B
�B
��B
��B
�B
�B
�cB
��B
�/B
�B
��B
�)B
��B
�)B
��B
��B
�]B
�]B
�B
�/B
��B
�cB
��B
��B
��B
�iB
�]B
�B
�5B
�]B
�B
��B
�]B
��B
�/B
�B
��B
��B
� B
�)B
��B
�/B
� B
��B
�]B
�B
�)B
��B
�B
��B
�B
��B
�/B
�B
�B
�5B
��B
��B
�B
��B
��B
�B
��B
�B
�cB
�5B
��B
�cB
�oB
�5B
�B
�AB
�B
��B
�iB
�B
� B
�cB
�B
��B
��B
�B
�B
��B
��B
�5B
�B
��B
�B
�cB
�B
�iB
�B
�cB
�AB
��B
�cB
�iB
�cB
��B
�B
�B
�B
�cB
�B
�B
�B
��B
�B
�;B
�B
�2B
�B
�B
�vB�B�B(�B+B*�B*�B,�B,B.�B2�B49B9�B@�BC�BGBM�BS[BQ�BR�BUgBWsBT�BVBW�BV�BV�BX�BZQBX�BX�B[WBYKBYBZ�BX�BX�BZQBZQBYBZB^5BT�BV�BX�BU�BT�BT�BO�BL�BM6BN<BK�BK^BJXBH�BG�BH�BG�BFBGEBHBE�BGEBH�BE�BHKBL�BJ�BLdBPBP�BT�BW�BYKB_�Bf2Bm)BsMBs�Bs�BuZBu%BxlBrGBt�Br|Br|Bs�Bs�Bq�BsBtTBqvBrBsMBr�Bp�Bq�BrGBq�Bp�BrGBr|BpoBp�BrBp�Bp�Br�Bp�BqBq�Br|BsMBuZBu�BtByrB�iB|BncBj�BiyBg�Bb�BcTBc�BaBb�Bb�Bg�Bg�BffBg�Bl�BjBlWBrBr�BtTBv�BzxBzDBzDB|B{B|�B��B�rB�lB��B�hB�hB�bB��B��B��B��B�=B�CB��B��B�hB��B�tB��B�B��B��B��B��B�eB�0B��B��B�RB�RB�$B�B�:B�:B�B�:B��B�:B�B�bB��B��B��B��B�-B��B�nB��B�bB��B�B�bB��B�B�@B��B�B��B��B�4B�hB�@B�hB�bB�:B��B��B��B�hB�hB��B��B�4B��B�hB�-B��B��B�B��B�bB��B�B�4B��B��B��B�hB�tB��B�B��B��B�FB�zB��B��B��B�zB�nB��B�@B��B�$B�XB�=B�IB��B�OB��B�LB��B��B��B�wB� B��B�B�mB��B͟B˒B��B��B��B��BǮB�mB�B� B�3BŢB�B��B�gB�zB�B�dB�B�)BʌB�^B��B�zB�#B��B��B�gB�mBƨB��B�-B��B��B��B�B�<B˒B��B�jB� B�&B҉B�TB��B֡B�BܒB�]B��B�B�>B�yB��B��B�5B�5B�TB�VB_B�BGB�B�B	lB�B	7B+B_B�B�B~BB	7B�B�B�BBB�B�B�BBFB�B�B�B!�B�B=BBBqBqBeB1B1BkB�B+B�B�B1B�B�B�B�B~B.IB�B+kB3�B1�B)�B+�B*0B'B+�B)�B(XB&�B(�B'�B+kB'�B'B(�B($B$tB&�B'RB(�B4nB2�B.�B0�B33B<�BS�BFtBJ�BC-BB'BHKBR�BJXBD3BGzBQNBS�BVBMjBQBM6BK�BK�BMjBL�BNBN�BM�BM6BMjBM�BOBBPHBK�BL�BMBN<BL�BLdBN<BL0BLdBM6BMjBMBL0BMjBN�BL�BK)BMjBN�BL�BL�BM�BUgBNBM�BL�BM�BLdBIRBK)B\�BHBQ�BLdBM�BW�BK�BL�BK�BJ�BT�B[#B`Bc�B[�BVmBW
Bb�BZ�B[�BZ�BZBXEBW?BVBUgBVmBV�BT�BVmBVmBVBT,BT�BU�BW�BWsBW�BS�BT�BT�BS[BS�BS�Bc�Bc�B^�Be�BU2BT�BV9BT�BS�BR�BQ�BR�BQNBPBO�BO�BNpBN<BQNBR�BNpBRTBR�BO�BP}BN<BQNBS&BOBL�BK)BJ�BIRBGzBJ#BK�BK�BH�BIBC�BHKBD�BE�BLdBD�BA BB'BB'B@B?B?BEB?�B<�B;�B8RB8B7LB7�B7B5�BIB1�B9�B3�B3�B1[B3hB,�B.�B.IB)�B'�B&�B&LB&B%FB&�B$@B"�B($B#nB%FB!-B!-BIB�B�B�B�B�B�B�B�BMB{B B�BbB@B�B�BhBBBAB�BBB�BSB	7BAB��B��B��B��B��B�VB��B��B�(B��B�PB�VB��B�B�B��B��B�>B�;B� B� B�5B�]B�"B�QB�B�B�B�B�B�DB�B�BچB�B�>B�,B��B�B�gBǮB��B�[B�B�nB�hB��B�hB�}B�}B�wB�eB�kB��B��B��B��B�B��B��B��444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                     444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                     444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022032715071120220327150711IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022040610022720220406100227QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022040610022720220406100227QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194420230210131944IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                