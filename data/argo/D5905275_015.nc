CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-06-12T04:46:30Z creation; 2023-04-26T19:14:26Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  dt   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180612044630  20230426191426  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_015                 7316_008644_015                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�i���@�i���11  @�i�[�7@�i�[�7@*"��D�@*"��D��d(ե�b��d(ե�b�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @=p�@}p�@�  @�  @�  A   A��A   A,(�A?\)A_\)A�  A�  A�  A�  A�  AϮA�  A�  A��B(�BQ�B  B�B'�
B0  B8  B@  BH(�BO�
BW�B_�
Bh(�Bp  Bx  B�
B��B�  B��B��B��B��B��B��
B�B�  B�{B�{B�(�B�(�B�(�B�  B�  B�{B�  B�  B�{B�  B�  B��
B��B��B��
B��B��B�  B��B��C  C
=C  C
=C
  C
=C{C
=C  C��C��C��C  C
=C��C��C!��C#��C%��C'��C)��C+�C-��C0  C2(�C4  C6  C8  C9��C<  C=��C@  CA��CC��CF  CH  CJ
=CL
=CN  CP  CR  CT
=CV
=CX
=CY��C\  C^  C_��Cb  Cd  Cf  Cg��Ci��Ck��Cm��Co��Cr  Ct  Cv  Cx  Cy��C|  C~  C��C���C���C���C�  C�  C�  C���C���C�C�C�  C�  C���C���C�  C���C���C�  C�  C�  C�C�C�C�  C�  C�  C�  C�  C�  C�  C���C�  C�  C�C�  C���C���C���C�C�C�  C�  C�  C���C���C�  C�  C�C���C���C���C�C�C�  C���C���C���C���C���C�  C�  C�C���C���C���C�  C�C�  C���C�  C�  C���C�  C���C���C���C�C�
=C�C�  C�C�C�C�C���C�  C�C�C���C���C�C�C�  C���C�  C�C�C�C�  C�  C�C���C���C���C�  C�  C�  C���C���C�C�C�C�C�  C���C�  C�  C���C�  C�  C���C���C�C�C���C�  C���C���D }qD �qD� D�qD}qD�qD}qD  D}qD  D��D  D� D  D��D�D��D	�D	� D
  D
� D  D��D�D�D�D� D  D� D  D� D�D��D�D� D�qD� D  D� D�D��D�qD��DD��D  D}qD�qD}qD�qD� D�D}qD  D��D  D}qD  D}qD��D� D�qD}qD�qD }qD!  D!��D"D"��D#  D#}qD$  D$��D$�qD%z�D%�qD&��D'  D'� D'�qD(}qD(�qD)� D*  D*��D+D+�D,�D,}qD-  D-� D.�D.��D/D/� D/��D0}qD1  D1��D2D2� D2�qD3� D4�D4��D5�D5�D6  D6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD;  D;� D<  D<��D=  D=� D=�qD>}qD?  D?�D@D@��D@�qDA��DB�DB� DB�qDC}qDC��DD}qDE�DE� DF  DF��DG  DG� DH�DH�DI  DI}qDJ  DJ��DK  DK}qDL  DL��DM  DMz�DM�qDN��DODO�DP�DP� DQ�DQ��DR  DR}qDS  DS��DT  DT}qDT�qDU� DU�qDV� DW  DW� DX�DX�DY  DYz�DY�qDZ}qD[  D[��D\  D\� D]  D]��D^�D^��D^�qD_}qD`  D`� Da  Da}qDa�qDb}qDb�qDc� Dd  Dd}qDe  De��Df  Df}qDg�Dg��Dh  Dh� Di  Di� Di�qDj� Dk  Dk� Dl�Dl��Dm  Dm}qDm�qDn� Dn�qDo� Dp  Dp� Dq�Dq}qDr  Dr�Ds�Ds��Dt  Dt� Du  Du}qDv  Dv� Dv�qDwz�Dx  Dx��Dy�Dy� Dy�qDz� Dz�qD{z�D{�qD|� D|�qD}}qD~  D~��D�D�D�HD�>�D�~�D���D���D�@ D�� D�� D�HD�@ D�� D�� D�HD�AHD��HD��qD���D�@ D��HD���D���D�>�D�� D�� D���D�@ D�� D���D�  D�AHD��HD��HD�  D�@ D�~�D���D���D�>�D�� D�� D�  D�AHD��HD���D��qD�=qD�~�D���D�  D�AHD�~�D�� D�HD�>�D�� D��HD�  D�=qD�~�D���D�HD�B�D��HD�� D���D�>�D�� D��HD��D�AHD�� D��HD�HD�AHD��HD��HD�HD�AHD���D��HD���D�>�D�~�D�� D�  D�@ D�~�D��HD�HD�=qD�}qD��qD��qD�>�D�� D���D���D�=qD�~�D�� D���D�>�D�� D��HD�HD�>�D�~�D���D�  D�B�D���D��HD��qD�@ D���D��HD��D�AHD�~�D��qD�  D�AHD�� D���D���D�@ D�~�D��qD���D�@ D�� D�� D�HD�>�D�~�D�� D�  D�@ D�� D�� D���D�=qD�~�D���D��qD�>�D�� D��HD�HD�@ D�~�D���D�  D�AHD�� D���D�  D�@ D�}qD��qD��qD�=qD�� D��HD�  D�@ D�� D�� D���D�@ D�~�D���D�  D�@ D�� D�� D�  D�>�D�� D���D���D�>�D�� D���D���D�>�D�~�D���D�  D�@ D�� D�D�HD�AHD�� D�� D���D�>�D�� D�� D�HD�AHD�� D�� D�HD�@ D�~�D���D�  D�AHD�� D��HD�HD�@ D�~�D���D�  D�@ D�� D���D���D�@ D��HD�� D���D�>�D��HD��HD�  D�AHD�� D���D�  D�@ D��HD��HD���D�@ D�� D���D�  D�AHD�� D�� D�  D�>�D�~�D��qD�  D�AHD��HD��HD��D�@ D�}qD��qD���D�@ D��HD��HD�HD�AHD�� D���D�  D�@ DHD��HD���D�@ DÀ D��HD�  D�>�DāHD�� D�  D�@ Dŀ Dž�D�HD�@ D�~�D�� D�  D�>�D�~�DǾ�D�HD�B�DȁHD��HD�HD�AHDɁHDɾ�D��qD�>�Dʀ D�� D�  D�>�Dˀ D�� D�  D�@ D̀ D�� D���D�>�D̀ D��HD�  D�@ D�~�Dξ�D�  D�@ Dπ D�� D�HD�>�DЀ D��HD���D�@ Dр D��HD�  D�>�D�~�DҾ�D�  D�AHDӀ D�� D�HD�AHDԀ D��HD�  D�=qD�~�Dվ�D�  D�@ D�}qD־�D���D�@ DׁHD�� D���D�@ D؁HD��HD�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ D�~�D۾�D���D�>�D܁HD��HD�HD�AHD݀ D��HD�HD�@ Dހ D��HD�  D�@ D߀ D�� D�  D�@ D�� D��HD�HD�AHD� D�� D�  D�@ D�HD�� D�  D�AHD�HD�� D�  D�@ D� D�� D���D�@ D�HD��HD�HD�@ D� D�� D���D�>�D� D��HD�HD�AHD肏D�� D�  D�AHD�HD�� D���D�>�D�~�D�� D�  D�@ D�~�D�� D�  D�@ D�HD�D�  D�>�D�HD�� D���D�@ D�HD�� D�  D�>�D�}qDﾸD�  D�AHD��HD��HD�  D�@ D�HD��HD�HD�AHD�HD��HD���D�>�D� D�qD���D�@ D� D�� D���D�>�D�� D��HD��D�AHD�� D��HD��D�AHD��HD�� D���D�AHD��HD�D�HD�@ D���D��HD�HD�C�D�k�D���?#�
?.{?k�?�z�?���?���?��@�\@\)@(��@0��@=p�@O\)@aG�@p��@�G�@���@���@��H@�ff@���@�z�@��R@Ǯ@�{@�@�  @���@��@��HAG�AA	��A�RA33A
=A�HA ��A%�A(Q�A,��A2�\A6ffA:=qA>�RAE�AI��AMp�AR�\AXQ�A\��Aa�Ag
=Al(�Ap��AuA{�A�Q�A��\A��A��A��\A�z�A�
=A��A�z�A�ffA���A��
A�{A�Q�A�33A�A��A��A���A�
=A���A�33A�A�Q�A�=qA�z�A��RA���A�33A��AǮA�=qA�(�A�{A�Q�Aҏ\A���AָRA�G�AۅA�A߮A��A�z�A�
=A��A�33A�p�A�\)A�G�A�A�{A�Q�A��A�(�A��RB z�Bp�B�\B�B�B{B33Bz�B	B
�RB�
B�B�\B\)Bz�BB
=B  B�BffB�B��B��B�RB  B�B{B
=B (�B!p�B"�RB#�B$z�B%B'
=B(  B(��B)�B+
=B,(�B-�B.{B/\)B0z�B1��B2�\B3�
B5�B6=qB7\)B8Q�B9B;
=B<(�B=G�B>ffB?�
B@��BB{BC33BD��BE�BF�HBH  BIp�BJ�RBK�
BL��BN=qBO�BP��BR{BS33BT��BU�BW33BXQ�BYp�BZ�HB\(�B]�B^=qB_�B`��Ba�Bc
=Bd(�Bep�Bf�RBg�
Bh��Bj{Bk�Bl��BmBn�RBp  BqG�BrffBs�Bt��Bu�Bw33BxQ�Byp�BzffB{�B}�B~=qB33B�(�B���B�p�B�  B��\B��B�B�ffB�
=B��B�{B���B�p�B��B�z�B��B�B�ffB�
=B���B�(�B��RB�\)B�  B���B�33B�B�ffB�
=B���B�{B���B�G�B��B��\B��B���B�(�B��RB�\)B�  B�z�B���B��B�{B��RB�33B���B�(�B��RB�G�B��
B�Q�B���B�\)B�  B��\B�33B��B�=qB��HB���B�Q�B�
=B���B�=qB��HB��B�Q�B�
=B��B�=qB��HB��B�=qB���B���B�Q�B���B��B�(�B���B��B�Q�B�
=B��B�=qB���B���B�Q�B�
=B��B�=qB��HB�\)B��
B�Q�B���B�33B��B�{B�z�B���B�
=B�\)B��B�{B�ffB���B��B�\)B���B�  B�ffB���B��B�\)B��B�  B�Q�B£�B��BÅB��
B�(�B�ffBĸRB�
=B�p�B��
B�(�B�z�B���B��B�p�B�B�{B�z�B���B��Bə�B��B�=qBʏ\B��HB��B�p�B��
B�=qB̏\B��HB�G�B͙�B��B�Q�BΣ�B���B�G�Bϙ�B��B�Q�BУ�B�
=BхB��B�Q�Bң�B�
=B�\)Bә�B�  B�ffB���B�33Bՙ�B��B�Q�BָRB�
=B�\)B�B�{B�z�B���B�33BمB��B�=qBڸRB�33BۅB��B�=qBܣ�B���B�\)B�B�(�Bޏ\B�
=B�p�B��
B�=qB�RB�33BᙚB�{B�ffB��HB�G�B�B�(�B�RB��B�B�(�B��B��B癚B�(�B�\B�
=B�B�{B�\B�
=B�B�  B�z�B�
=B�B�  B�z�B�
=B�B�  B�z�B�
=B�B�  B�\B��B�B�=qB��RB�G�B�B�Q�B��HB�p�B�  B�z�B�
=B���B�{B���B�33B�B�=qB��HB�p�B�  B��\B�33B�C (�C p�C �RC  CG�C�\C�
C(�Cp�C�RC  CG�C��C�HC(�Cz�CC
=CQ�C��C�C(�Cz�CC{C\)C��C�C=qC�C��C	{C	\)C	��C	�C
33C
z�C
C
=CG�C��C�HC(�CffC�RC��C=qC�C�
C{CffC�C��C=qC�\C�
C�CffC�C��CG�C�\C�
C�CffC��C�C33Cp�C�RC  CQ�C��C�
C(�Cp�C�RC
=CG�C��C�HC(�Cz�CC
=CQ�C��C�
C�CffC�C  CG�C��C�HC(�Cp�C�RC  C=qCz�C��C
=CQ�C��C�C33Cz�C��C{C\)C��C�HC (�C p�C �RC!  C!Q�C!��C!�C"33C"z�C"C#  C#Q�C#��C#�HC$33C$�C$��C%{C%Q�C%��C%�
C&�C&p�C&�RC'
=C'\)C'��C'�HC(�C(ffC(�RC)
=C)ffC)��C)��C*33C*�C*�
C+(�C+z�C+��C,{C,\)C,�C-
=C-\)C-�C-��C.=qC.�\C.�HC/=qC/�C/��C0
=C0Q�C0��C0�C133C1p�C1�C1�
C2
=C2=qC2p�C2��C2C2�HC3  C3�C333C3Q�C3p�C3��C3C3�HC4
=C4�C4=qC4Q�C4p�C4��C4C4�HC5  C5�C5=qC5\)C5p�C5�\C5�RC5�HC6  C6{C633C6Q�C6p�C6�\C6�RC6�
C6��C7{C7(�C7=qC7ffC7�\C7�C7��C7�HC7��C8{C833C8\)C8�C8��C8�C8��C8�HC9  C9�C9G�C9ffC9�C9��C9�C9��C9�C:{C:33C:\)C:p�C:�\C:��C:C:�C;{C;33C;Q�C;ffC;�C;��C;��C;�C<
=C<(�C<=qC<\)C<z�C<��C<C<�HC=  C={C=33C=Q�C=z�C=��C=�RC=�
C=�C>
=C>(�C>\)C>p�C>�\C>�C>��C>��C?�C?G�C?ffC?z�C?��C?C?�C@{C@=qC@\)C@p�C@�\C@�RC@�HCA  CA33CAQ�CAp�CA��CA�RCA�
CB
=CB33CB\)CBz�CB��CBCB�HCC{CC=qCCffCC�\CC�CC��CC��CD(�CDG�CDp�CD�\CD�CD�
CE
=CE33CEQ�CEp�CE��CECE��CF�CF=qCF\)CF�CF�CF�
CG  CG�CG=qCGffCG��CGCG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     ?��@   @=p�@}p�@�  @�  @�  A   A��A   A,(�A?\)A_\)A�  A�  A�  A�  A�  AϮA�  A�  A��B(�BQ�B  B�B'�
B0  B8  B@  BH(�BO�
BW�B_�
Bh(�Bp  Bx  B�
B��B�  B��B��B��B��B��B��
B�B�  B�{B�{B�(�B�(�B�(�B�  B�  B�{B�  B�  B�{B�  B�  B��
B��B��B��
B��B��B�  B��B��C  C
=C  C
=C
  C
=C{C
=C  C��C��C��C  C
=C��C��C!��C#��C%��C'��C)��C+�C-��C0  C2(�C4  C6  C8  C9��C<  C=��C@  CA��CC��CF  CH  CJ
=CL
=CN  CP  CR  CT
=CV
=CX
=CY��C\  C^  C_��Cb  Cd  Cf  Cg��Ci��Ck��Cm��Co��Cr  Ct  Cv  Cx  Cy��C|  C~  C��C���C���C���C�  C�  C�  C���C���C�C�C�  C�  C���C���C�  C���C���C�  C�  C�  C�C�C�C�  C�  C�  C�  C�  C�  C�  C���C�  C�  C�C�  C���C���C���C�C�C�  C�  C�  C���C���C�  C�  C�C���C���C���C�C�C�  C���C���C���C���C���C�  C�  C�C���C���C���C�  C�C�  C���C�  C�  C���C�  C���C���C���C�C�
=C�C�  C�C�C�C�C���C�  C�C�C���C���C�C�C�  C���C�  C�C�C�C�  C�  C�C���C���C���C�  C�  C�  C���C���C�C�C�C�C�  C���C�  C�  C���C�  C�  C���C���C�C�C���C�  C���C���D }qD �qD� D�qD}qD�qD}qD  D}qD  D��D  D� D  D��D�D��D	�D	� D
  D
� D  D��D�D�D�D� D  D� D  D� D�D��D�D� D�qD� D  D� D�D��D�qD��DD��D  D}qD�qD}qD�qD� D�D}qD  D��D  D}qD  D}qD��D� D�qD}qD�qD }qD!  D!��D"D"��D#  D#}qD$  D$��D$�qD%z�D%�qD&��D'  D'� D'�qD(}qD(�qD)� D*  D*��D+D+�D,�D,}qD-  D-� D.�D.��D/D/� D/��D0}qD1  D1��D2D2� D2�qD3� D4�D4��D5�D5�D6  D6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD;  D;� D<  D<��D=  D=� D=�qD>}qD?  D?�D@D@��D@�qDA��DB�DB� DB�qDC}qDC��DD}qDE�DE� DF  DF��DG  DG� DH�DH�DI  DI}qDJ  DJ��DK  DK}qDL  DL��DM  DMz�DM�qDN��DODO�DP�DP� DQ�DQ��DR  DR}qDS  DS��DT  DT}qDT�qDU� DU�qDV� DW  DW� DX�DX�DY  DYz�DY�qDZ}qD[  D[��D\  D\� D]  D]��D^�D^��D^�qD_}qD`  D`� Da  Da}qDa�qDb}qDb�qDc� Dd  Dd}qDe  De��Df  Df}qDg�Dg��Dh  Dh� Di  Di� Di�qDj� Dk  Dk� Dl�Dl��Dm  Dm}qDm�qDn� Dn�qDo� Dp  Dp� Dq�Dq}qDr  Dr�Ds�Ds��Dt  Dt� Du  Du}qDv  Dv� Dv�qDwz�Dx  Dx��Dy�Dy� Dy�qDz� Dz�qD{z�D{�qD|� D|�qD}}qD~  D~��D�D�D�HD�>�D�~�D���D���D�@ D�� D�� D�HD�@ D�� D�� D�HD�AHD��HD��qD���D�@ D��HD���D���D�>�D�� D�� D���D�@ D�� D���D�  D�AHD��HD��HD�  D�@ D�~�D���D���D�>�D�� D�� D�  D�AHD��HD���D��qD�=qD�~�D���D�  D�AHD�~�D�� D�HD�>�D�� D��HD�  D�=qD�~�D���D�HD�B�D��HD�� D���D�>�D�� D��HD��D�AHD�� D��HD�HD�AHD��HD��HD�HD�AHD���D��HD���D�>�D�~�D�� D�  D�@ D�~�D��HD�HD�=qD�}qD��qD��qD�>�D�� D���D���D�=qD�~�D�� D���D�>�D�� D��HD�HD�>�D�~�D���D�  D�B�D���D��HD��qD�@ D���D��HD��D�AHD�~�D��qD�  D�AHD�� D���D���D�@ D�~�D��qD���D�@ D�� D�� D�HD�>�D�~�D�� D�  D�@ D�� D�� D���D�=qD�~�D���D��qD�>�D�� D��HD�HD�@ D�~�D���D�  D�AHD�� D���D�  D�@ D�}qD��qD��qD�=qD�� D��HD�  D�@ D�� D�� D���D�@ D�~�D���D�  D�@ D�� D�� D�  D�>�D�� D���D���D�>�D�� D���D���D�>�D�~�D���D�  D�@ D�� D�D�HD�AHD�� D�� D���D�>�D�� D�� D�HD�AHD�� D�� D�HD�@ D�~�D���D�  D�AHD�� D��HD�HD�@ D�~�D���D�  D�@ D�� D���D���D�@ D��HD�� D���D�>�D��HD��HD�  D�AHD�� D���D�  D�@ D��HD��HD���D�@ D�� D���D�  D�AHD�� D�� D�  D�>�D�~�D��qD�  D�AHD��HD��HD��D�@ D�}qD��qD���D�@ D��HD��HD�HD�AHD�� D���D�  D�@ DHD��HD���D�@ DÀ D��HD�  D�>�DāHD�� D�  D�@ Dŀ Dž�D�HD�@ D�~�D�� D�  D�>�D�~�DǾ�D�HD�B�DȁHD��HD�HD�AHDɁHDɾ�D��qD�>�Dʀ D�� D�  D�>�Dˀ D�� D�  D�@ D̀ D�� D���D�>�D̀ D��HD�  D�@ D�~�Dξ�D�  D�@ Dπ D�� D�HD�>�DЀ D��HD���D�@ Dр D��HD�  D�>�D�~�DҾ�D�  D�AHDӀ D�� D�HD�AHDԀ D��HD�  D�=qD�~�Dվ�D�  D�@ D�}qD־�D���D�@ DׁHD�� D���D�@ D؁HD��HD�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ D�~�D۾�D���D�>�D܁HD��HD�HD�AHD݀ D��HD�HD�@ Dހ D��HD�  D�@ D߀ D�� D�  D�@ D�� D��HD�HD�AHD� D�� D�  D�@ D�HD�� D�  D�AHD�HD�� D�  D�@ D� D�� D���D�@ D�HD��HD�HD�@ D� D�� D���D�>�D� D��HD�HD�AHD肏D�� D�  D�AHD�HD�� D���D�>�D�~�D�� D�  D�@ D�~�D�� D�  D�@ D�HD�D�  D�>�D�HD�� D���D�@ D�HD�� D�  D�>�D�}qDﾸD�  D�AHD��HD��HD�  D�@ D�HD��HD�HD�AHD�HD��HD���D�>�D� D�qD���D�@ D� D�� D���D�>�D�� D��HD��D�AHD�� D��HD��D�AHD��HD�� D���D�AHD��HD�D�HD�@ D���D��HD�HD�C�D�k�G�O�?#�
?.{?k�?�z�?���?���?��@�\@\)@(��@0��@=p�@O\)@aG�@p��@�G�@���@���@��H@�ff@���@�z�@��R@Ǯ@�{@�@�  @���@��@��HAG�AA	��A�RA33A
=A�HA ��A%�A(Q�A,��A2�\A6ffA:=qA>�RAE�AI��AMp�AR�\AXQ�A\��Aa�Ag
=Al(�Ap��AuA{�A�Q�A��\A��A��A��\A�z�A�
=A��A�z�A�ffA���A��
A�{A�Q�A�33A�A��A��A���A�
=A���A�33A�A�Q�A�=qA�z�A��RA���A�33A��AǮA�=qA�(�A�{A�Q�Aҏ\A���AָRA�G�AۅA�A߮A��A�z�A�
=A��A�33A�p�A�\)A�G�A�A�{A�Q�A��A�(�A��RB z�Bp�B�\B�B�B{B33Bz�B	B
�RB�
B�B�\B\)Bz�BB
=B  B�BffB�B��B��B�RB  B�B{B
=B (�B!p�B"�RB#�B$z�B%B'
=B(  B(��B)�B+
=B,(�B-�B.{B/\)B0z�B1��B2�\B3�
B5�B6=qB7\)B8Q�B9B;
=B<(�B=G�B>ffB?�
B@��BB{BC33BD��BE�BF�HBH  BIp�BJ�RBK�
BL��BN=qBO�BP��BR{BS33BT��BU�BW33BXQ�BYp�BZ�HB\(�B]�B^=qB_�B`��Ba�Bc
=Bd(�Bep�Bf�RBg�
Bh��Bj{Bk�Bl��BmBn�RBp  BqG�BrffBs�Bt��Bu�Bw33BxQ�Byp�BzffB{�B}�B~=qB33B�(�B���B�p�B�  B��\B��B�B�ffB�
=B��B�{B���B�p�B��B�z�B��B�B�ffB�
=B���B�(�B��RB�\)B�  B���B�33B�B�ffB�
=B���B�{B���B�G�B��B��\B��B���B�(�B��RB�\)B�  B�z�B���B��B�{B��RB�33B���B�(�B��RB�G�B��
B�Q�B���B�\)B�  B��\B�33B��B�=qB��HB���B�Q�B�
=B���B�=qB��HB��B�Q�B�
=B��B�=qB��HB��B�=qB���B���B�Q�B���B��B�(�B���B��B�Q�B�
=B��B�=qB���B���B�Q�B�
=B��B�=qB��HB�\)B��
B�Q�B���B�33B��B�{B�z�B���B�
=B�\)B��B�{B�ffB���B��B�\)B���B�  B�ffB���B��B�\)B��B�  B�Q�B£�B��BÅB��
B�(�B�ffBĸRB�
=B�p�B��
B�(�B�z�B���B��B�p�B�B�{B�z�B���B��Bə�B��B�=qBʏ\B��HB��B�p�B��
B�=qB̏\B��HB�G�B͙�B��B�Q�BΣ�B���B�G�Bϙ�B��B�Q�BУ�B�
=BхB��B�Q�Bң�B�
=B�\)Bә�B�  B�ffB���B�33Bՙ�B��B�Q�BָRB�
=B�\)B�B�{B�z�B���B�33BمB��B�=qBڸRB�33BۅB��B�=qBܣ�B���B�\)B�B�(�Bޏ\B�
=B�p�B��
B�=qB�RB�33BᙚB�{B�ffB��HB�G�B�B�(�B�RB��B�B�(�B��B��B癚B�(�B�\B�
=B�B�{B�\B�
=B�B�  B�z�B�
=B�B�  B�z�B�
=B�B�  B�z�B�
=B�B�  B�\B��B�B�=qB��RB�G�B�B�Q�B��HB�p�B�  B�z�B�
=B���B�{B���B�33B�B�=qB��HB�p�B�  B��\B�33B�C (�C p�C �RC  CG�C�\C�
C(�Cp�C�RC  CG�C��C�HC(�Cz�CC
=CQ�C��C�C(�Cz�CC{C\)C��C�C=qC�C��C	{C	\)C	��C	�C
33C
z�C
C
=CG�C��C�HC(�CffC�RC��C=qC�C�
C{CffC�C��C=qC�\C�
C�CffC�C��CG�C�\C�
C�CffC��C�C33Cp�C�RC  CQ�C��C�
C(�Cp�C�RC
=CG�C��C�HC(�Cz�CC
=CQ�C��C�
C�CffC�C  CG�C��C�HC(�Cp�C�RC  C=qCz�C��C
=CQ�C��C�C33Cz�C��C{C\)C��C�HC (�C p�C �RC!  C!Q�C!��C!�C"33C"z�C"C#  C#Q�C#��C#�HC$33C$�C$��C%{C%Q�C%��C%�
C&�C&p�C&�RC'
=C'\)C'��C'�HC(�C(ffC(�RC)
=C)ffC)��C)��C*33C*�C*�
C+(�C+z�C+��C,{C,\)C,�C-
=C-\)C-�C-��C.=qC.�\C.�HC/=qC/�C/��C0
=C0Q�C0��C0�C133C1p�C1�C1�
C2
=C2=qC2p�C2��C2C2�HC3  C3�C333C3Q�C3p�C3��C3C3�HC4
=C4�C4=qC4Q�C4p�C4��C4C4�HC5  C5�C5=qC5\)C5p�C5�\C5�RC5�HC6  C6{C633C6Q�C6p�C6�\C6�RC6�
C6��C7{C7(�C7=qC7ffC7�\C7�C7��C7�HC7��C8{C833C8\)C8�C8��C8�C8��C8�HC9  C9�C9G�C9ffC9�C9��C9�C9��C9�C:{C:33C:\)C:p�C:�\C:��C:C:�C;{C;33C;Q�C;ffC;�C;��C;��C;�C<
=C<(�C<=qC<\)C<z�C<��C<C<�HC=  C={C=33C=Q�C=z�C=��C=�RC=�
C=�C>
=C>(�C>\)C>p�C>�\C>�C>��C>��C?�C?G�C?ffC?z�C?��C?C?�C@{C@=qC@\)C@p�C@�\C@�RC@�HCA  CA33CAQ�CAp�CA��CA�RCA�
CB
=CB33CB\)CBz�CB��CBCB�HCC{CC=qCCffCC�\CC�CC��CC��CD(�CDG�CDp�CD�\CD�CD�
CE
=CE33CEQ�CEp�CE��CECE��CF�CF=qCF\)CF�CF�CF�
CG  CG�CG=qCGffCG��CGCG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@�@�;G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bA�bA�bA�bA�{A��A��A��A��A��A��A��A��A� �A��A� �A� �A�"�A� �A�"�A�"�A�"�A� �A� �A��A�bA��A��A��A�%A�VA�bA��A��A��A�VA�%A�A�A�A�A�  A���A��A��A�|�A�ȴA�XA�+A�?}A�=qA��#AƏ\A�x�Aź^A���A7A��/A��^A��A�S�A�ZA��9A���A�`BA�x�A�5?A�bNA��!A���A���A��A���A�E�A�r�A�VA�p�A��;A�+A�A�A��A�$�A�?}A�ffA���A�M�A��A�ĜA�I�A��A{AuoAo�Am/AkK�Ai|�Af��AdȴAc|�A]�-A\ffAVJAQ��ALQ�AI�^AGp�AD�AA�;AA"�A?�A=��A=K�A<��A<E�A;ƨA;+A:~�A9��A8��A8�DA8z�A81'A6��A5��A4r�A1��A.��A.JA-dZA,��A+�TA+�A+�7A+x�A+p�A+x�A+�hA+�A,ZA,n�A,�A+�^A(v�A'�A&�!A%��A%�A%dZA#ƨA#;dA#�A#��A#�hA"�uA!��A E�A��A�A�mA��AhsA��Ar�Al�AA�A�AA�A�hAdZA�HAVA�A
=A�HAZA��A�A�wA+A�DA(�A{A�^AdZA�AI�A�#A�AC�A&�A%A��A��AjA �A�A  A�-A?}A�!AJA��AhsAG�A;dA7LA/AVA��A�A�RA{A��A�A�TA��A�yAr�AbA��AC�A
��A
�\A
ffA
  A	�wA	?}A	�A��A�!A~�A�A�PAl�A7LA��A�+AA�A�A�hA+A�AZA(�A1A�TA�A�PA\)A�AȴA�+A�A�A�A n�@�C�@��\@�G�@���@�dZ@�"�@��!@��@��@�bN@�\)@��#@�D@��@�@�@��@�o@�@���@�Ĝ@�u@�r�@�b@@�-@��@���@�X@�%@�j@�bN@띲@�j@�|�@睲@��y@�@��/@�bN@�F@�l�@��y@�V@�J@��T@�x�@��@�b@ߍP@���@��@�=q@�`B@�%@���@�l�@�;d@ڟ�@ٺ^@�/@أ�@��@�33@�n�@�E�@�-@�J@��@�G�@ԃ@��m@�S�@��@җ�@�-@Ѳ-@�&�@Гu@�(�@Ͼw@�|�@�;d@ΰ!@�{@͡�@�?}@̣�@��@ʟ�@�x�@ȼj@��@ǅ@ƸR@���@�`B@Ĭ@�z�@�I�@�1'@�(�@�(�@� �@�1@�l�@�@�E�@�^5@�v�@�5?@�{@��@���@�V@��`@�r�@�  @��
@��P@��@���@���@�X@��@�1@�o@���@��!@�ff@��@��7@�&�@��9@��9@���@�Z@��w@��@���@�x�@��@���@��9@��@�Z@���@�$�@���@�X@�V@�j@��F@��P@�\)@�"�@��R@�=q@��#@��^@���@��7@�G�@���@���@�9X@� �@��w@�"�@�V@�-@�$�@��^@�G�@��/@�  @�|�@�;d@��H@�E�@���@���@�G�@���@���@�Z@��;@��@�l�@��y@�V@��@��@���@��D@�9X@�1@��@���@�t�@�C�@�n�@�O�@��@���@��@�Ĝ@�Q�@��
@��@�l�@��H@�-@��^@��h@�x�@�7L@���@���@��D@�j@�(�@�ƨ@�l�@�;d@�
=@���@��!@��\@�v�@�E�@�$�@���@���@�hs@�&�@��@��D@�bN@�9X@�b@��m@��
@�ƨ@���@�o@���@�v�@�v�@�n�@�^5@�=q@�J@���@��7@��7@�p�@�?}@���@���@��9@�z�@�A�@� �@��;@���@�\)@��\@��@��-@�hs@�7L@�%@�Ĝ@��@�(�@��m@�ƨ@��@�K�@�o@���@��\@�V@�-@�J@���@���@�`B@��@��j@�Q�@��
@���@�|�@�t�@�33@��@��R@��+@�V@�5?@�{@��#@���@��7@�hs@�V@��/@��@�A�@��
@��@�t�@�l�@�S�@��+@�E�@�-@�J@���@�@�p�@��`@��j@��u@�Q�@�9X@� �@�@��@�@~�y@~�R@~��@~v�@~V@~@}@}p�@|�/@|Z@{�
@z�@y��@yhs@xĜ@x  @w;d@w
=@v��@vE�@u�@s�
@st�@s33@r�H@r��@r�!@rn�@r-@q��@q&�@q%@p��@p�9@pr�@pA�@p �@o�;@o\)@n�y@n��@n�+@nv�@nff@n@m��@m�@mO�@mV@l�@l��@k�F@kC�@j��@jM�@i�7@h�@g�@g;d@f�y@f��@fV@e��@e�-@e�h@e?}@d�/@d�D@dZ@c��@cS�@b�!@b~�@b�@a�^@a7L@`Ĝ@`bN@_�@_��@_��@_+@^�+@^E�@^$�@]��@]�@]V@\�/@\�D@[��@[C�@Z��@Z�\@Zn�@Y�#@Yhs@X�`@X�9@XQ�@W�@W\)@W;d@Vȴ@VE�@U��@U��@T�@Tz�@S��@St�@SC�@So@R�\@Q��@Q�^@Q%@P�@PbN@O�@O��@O;d@O�@N��@N�@N��@NE�@N@M��@M�-@M�h@Mp�@M/@L��@L�D@Lj@L9X@K�
@K��@Kt�@J�H@J^5@J�@I�@I�7@I&�@H�9@H�@G�w@G;d@F��@F�R@F5?@E�-@D�@D�D@DZ@DI�@D�@Cƨ@CdZ@CS�@CC�@B�H@B�\@A��@A7L@@r�@?��@?K�@>ȴ@>ff@>5?@>{@>@=�@=V@<�j@<9X@;��@;C�@;33@;C�@;C�@;C�@;"�@:��@:=q@9�#@9��@9hs@9�@8�`@8�@8A�@8 �@7�@7�P@7+@6ȴ@6{@5@5�@5?}@4�@4�@4��@4��@4��@4�@4�@4��@4I�@4(�@3�
@3�F@3��@3�@3S�@3@2��@2�\@2^5@2=q@1�@1X@0�`@0r�@0Q�@0  @/��@/l�@/\)@/+@.�R@.�+@.v�@.�+@.v�@.�+@.ff@-��@-�@-�@,�@,z�@,1@+�m@+��@+t�@+S�@+33@+@*n�@*-@*=q@*=q@*�@*J@)��@)G�@(�u@(b@(b@(b@(b@(  @'�P@&�@&V@&V@&{@%�T@%��@%�-@%�@%O�@$�j@$Z@$I�@$Z@$j@$9X@#�m@#��@#��@#ƨ@#dZ@"��@"�@"�!@"n�@"M�@"-@!�@!��@!hs@!x�@!�7@!X@ bN@��@;d@�@��@v�@V@5?@�T@�-@�h@`B@�@V@�/@�j@��@��@z�@Z@1@�m@��@t�@33@"�@o@��@^5@�@�#@�7@�7@G�@�@�@%@�`@�@A�@  @�w@�@��@K�@
=@�y@��@�+@�+@E�@@�@@��@�h@p�@p�@`B@/@�@��@�/@I�@�@�@��@��@��@��@��@dZ@@�H@��@~�@^5@^5@M�@-@�@�@�#@��@��@x�@X@X@G�@7L@�@%@��@�`@�`@Ĝ@bN@  @  @�@�;@�w@l�@K�@��@��@��@E�@�T@��@�h@?}@V@�@�@��@�@�@�D@z�A�oA�VA�oA�JA�VA�{A�oA�VA�oA�oA�bA�VA�bA�oA�bA�VA�VA�bA�{A��A�oA��A��A��A��A��A��A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A� �A� �A��A��A� �A�"�A��A��A��A�"�A��A��A� �A�"�A��A��A� �A� �A��A�"�A�"�A� �A��A� �A�"�A��A��A�"�A�$�A� �A��A�$�A�&�A� �A� �A�$�A�$�A�"�A��A� �A�$�A�$�A��A�"�A�$�A�"�A��A� �A�$�A�$�A� �A�"�A�$�A�$�A�"�A� �A�$�A�$�A�"�A��A�"�A�$�A�"�A��A� �A�$�A�&�A�"�A�"�A� �A��A��A��A�"�A� �A��A� �A�"�A��A��A��A� �A��A��A�bA�bA�{A�VA�JA�
=A��A�{A�oA�VA��A��A�{A�VA�oA��A� �A��A��A��A��A��A��A��A� �A� �A��A��A��A��A��A�1A�
=A�JA�%A�  A���A�A�%A�{A�bA�JA�VA�oA�bA�bA�{A�oA�VA�1A�oA�{A��A��A�{A��A��A��A�{A��A��A��A��A��A��A� �A��A�{A��A��A�bA�VA�VA�{A�bA�VA�bA�A�1A�1A�%A�A�A�A�%A�1A�A�A�A�%A�A�A�  A�A�%A�%A�A�  A�A�A�A�  A�  A�A�A�%A�A�  A�  A�A�A�A�  A���A�A�A�A���A���A���A���A���A���A���A��A��A���A���A��A��A��A��A��A��A��A��yA��`A��`Aۺ^A�t�A�5?Aڝ�A�;dA�  A�1AבhA�z�Aԧ�A�/A���A�?}AмjAρA���Aδ9A�C�A��A���A��`A;wA�t�A�1'A�bA�  A��
A̍PA�1'A˺^A���AʓuA�^5AɸRA�A�jA�%A�~�A�^5A�$�A�A���A��HA�ƨAƥ�Aƙ�Aƕ�Aƕ�Aƕ�Aƕ�AƓuAƏ\AƍPAƋDAƉ7AƅAƇ+AƃA�~�A�|�A�z�A�v�A�z�A�x�A�v�A�r�A�l�A�hsA�`BA�=qA�$�A�A��A��
A���Aţ�Aŗ�A�x�A��A���A��Aĩ�A�|�A�hsA�VA���Aò-A�v�A�\)A�9XA�1'A��A��A©�ADAA�x�A�r�A�v�A�z�A�~�A�~�A�~�A�~�A�v�A�`BA�/A��FA�p�A�M�A�~�A��jA��7A�S�A�
=A��`A��A�bNA�G�A�33A���A�jA�9XA��A���A��A�l�A�E�A�&�A�{A�bA�VA�
=A�1A�%A���A��`A���A�ƨA���A�dZA�M�A�;dA�(�A�%A��#A���A��7A�XA��A��
A�S�A���A���A�S�A�$�A���A�VA�ffA���A�jA�33A��#A��9A��+A�ZA�E�A�7LA�"�A��wA�G�A���A�v�A�XA�5?A���A�|�A�(�A�A��
A��A�K�A�bA��`A��A�\)A� �A��A���A���A��uA�x�A�G�A��A���A��A���A��A�hsA�ZA�ZA�XA�Q�A�Q�A�O�A�I�A�A���A�~�A�bNA�bA���A�t�A�I�A��;A�5?A��TA�A�p�A�K�A�$�A�  A���A�\)A�?}A�/A��A��mA��FA�Q�A� �A��A��RA�p�A��/A��7A�E�A�VA��/A���A�p�A�^5A�K�A�7LA�$�A�A��A��#A�ĜA���A�ffA�VA���A��^A��hA�bNA�-A��mA�ĜA��PA�^5A�$�A�A���A���A��A���A�t�A��A���A�9XA��hA�;dA��
A��hA�?}A���A�n�A���A���A�ffA�{A��/A��A�|�A�E�A���A��^A�ZA��A��uA�
=A��A�ĜA��RA��A�VA�bA��-A�&�A�x�A���A��RA��A���A���A�hsA���A��^A��!A���A�ZA��-A��A�dZA��A��;A��RA��A�\)A�=qA���A��A��A��
A��!A��uA�bNA�;dA�%A�A�ffA��A�z�A�(�A��A�z�A� �A��FA�`BA�"�A�bA��7A�1'A��mA��PA�?}A��
A�C�A�%A���A�"�A��#A��A�v�A�^5A�=qA���A�oA��jA��A~�\A{
=Ax�DAw�TAw33AvbNAu��Au�FAu+AtbNAsC�Aq33ApJAo�-AoXAoAn��An�9An~�An�Am�FAl�Al9XAl�AlAk��AkAkt�Akp�AkS�AkS�Ak�Aj��Aj�/Aj�/AjĜAj��Aj�RAj�!Aj��AjffAj9XAjbAi��Ai;dAh��AhbNAhVAh=qAh�Ah1Ag��Ag�
Ag��Ag�7Agl�AgC�Ag"�AgoAf��Af�Af�/Af�+Af1'AfJAe�
AeƨAe��Ae�Ae?}Ae&�Ae�Ad��AeAd�AdȴAd��Ad�Ad��Ad�uAd�Adz�AdffAdVAdI�AdA�Ad9XAd5?Ad �AdAc��Ac�Ac�#Ac�FAc�hAc�Ac\)Ac/Ab�`Ab�!Abv�Aa��A_�A^jA]��A]�wA]�FA]��A]�A]�7A]�A]�A]�A]t�A]p�A]`BA]S�A]O�A]K�A]C�A]33A]+A]�A]A\�`A\��A\�jA\��A\~�A\Q�A\JA[��A[&�AZv�AY�mAY"�AXr�AW�FAW33AVVAUAU�AU;dAT�yAT��ATĜAT�RAT��AT��AT�uAT�uAT�\AT�+ATVAS�mAS/AQ�TAP��AO�mAO/AN��AN��ANjAN9XANAM��AMoAL��ALZAK��AK��AKl�AKG�AKK�AKS�AK?}AK�AJ�/AJ�!AJz�AJE�AJ$�AJ  AI�
AI��AI"�AHȴAH�+AHr�AHffAHbNAHZAHE�AH(�AG��AG�^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     A�bA�bA�bA�bA�{A��A��A��A��A��A��A��A��A� �A��A� �A� �A�"�A� �A�"�A�"�A�"�A� �A� �A��A�bA��A��A��A�%A�VA�bA��A��A��A�VA�%A�A�A�A�A�  A���A��A��A�|�A�ȴA�XA�+A�?}A�=qA��#AƏ\A�x�Aź^A���A7A��/A��^A��A�S�A�ZA��9A���A�`BA�x�A�5?A�bNA��!A���A���A��A���A�E�A�r�A�VA�p�A��;A�+A�A�A��A�$�A�?}A�ffA���A�M�A��A�ĜA�I�A��A{AuoAo�Am/AkK�Ai|�Af��AdȴAc|�A]�-A\ffAVJAQ��ALQ�AI�^AGp�AD�AA�;AA"�A?�A=��A=K�A<��A<E�A;ƨA;+A:~�A9��A8��A8�DA8z�A81'A6��A5��A4r�A1��A.��A.JA-dZA,��A+�TA+�A+�7A+x�A+p�A+x�A+�hA+�A,ZA,n�A,�A+�^A(v�A'�A&�!A%��A%�A%dZA#ƨA#;dA#�A#��A#�hA"�uA!��A E�A��A�A�mA��AhsA��Ar�Al�AA�A�AA�A�hAdZA�HAVA�A
=A�HAZA��A�A�wA+A�DA(�A{A�^AdZA�AI�A�#A�AC�A&�A%A��A��AjA �A�A  A�-A?}A�!AJA��AhsAG�A;dA7LA/AVA��A�A�RA{A��A�A�TA��A�yAr�AbA��AC�A
��A
�\A
ffA
  A	�wA	?}A	�A��A�!A~�A�A�PAl�A7LA��A�+AA�A�A�hA+A�AZA(�A1A�TA�A�PA\)A�AȴA�+A�A�A�A n�@�C�@��\@�G�@���@�dZ@�"�@��!@��@��@�bN@�\)@��#@�D@��@�@�@��@�o@�@���@�Ĝ@�u@�r�@�b@@�-@��@���@�X@�%@�j@�bN@띲@�j@�|�@睲@��y@�@��/@�bN@�F@�l�@��y@�V@�J@��T@�x�@��@�b@ߍP@���@��@�=q@�`B@�%@���@�l�@�;d@ڟ�@ٺ^@�/@أ�@��@�33@�n�@�E�@�-@�J@��@�G�@ԃ@��m@�S�@��@җ�@�-@Ѳ-@�&�@Гu@�(�@Ͼw@�|�@�;d@ΰ!@�{@͡�@�?}@̣�@��@ʟ�@�x�@ȼj@��@ǅ@ƸR@���@�`B@Ĭ@�z�@�I�@�1'@�(�@�(�@� �@�1@�l�@�@�E�@�^5@�v�@�5?@�{@��@���@�V@��`@�r�@�  @��
@��P@��@���@���@�X@��@�1@�o@���@��!@�ff@��@��7@�&�@��9@��9@���@�Z@��w@��@���@�x�@��@���@��9@��@�Z@���@�$�@���@�X@�V@�j@��F@��P@�\)@�"�@��R@�=q@��#@��^@���@��7@�G�@���@���@�9X@� �@��w@�"�@�V@�-@�$�@��^@�G�@��/@�  @�|�@�;d@��H@�E�@���@���@�G�@���@���@�Z@��;@��@�l�@��y@�V@��@��@���@��D@�9X@�1@��@���@�t�@�C�@�n�@�O�@��@���@��@�Ĝ@�Q�@��
@��@�l�@��H@�-@��^@��h@�x�@�7L@���@���@��D@�j@�(�@�ƨ@�l�@�;d@�
=@���@��!@��\@�v�@�E�@�$�@���@���@�hs@�&�@��@��D@�bN@�9X@�b@��m@��
@�ƨ@���@�o@���@�v�@�v�@�n�@�^5@�=q@�J@���@��7@��7@�p�@�?}@���@���@��9@�z�@�A�@� �@��;@���@�\)@��\@��@��-@�hs@�7L@�%@�Ĝ@��@�(�@��m@�ƨ@��@�K�@�o@���@��\@�V@�-@�J@���@���@�`B@��@��j@�Q�@��
@���@�|�@�t�@�33@��@��R@��+@�V@�5?@�{@��#@���@��7@�hs@�V@��/@��@�A�@��
@��@�t�@�l�@�S�@��+@�E�@�-@�J@���@�@�p�@��`@��j@��u@�Q�@�9X@� �@�@��@�@~�y@~�R@~��@~v�@~V@~@}@}p�@|�/@|Z@{�
@z�@y��@yhs@xĜ@x  @w;d@w
=@v��@vE�@u�@s�
@st�@s33@r�H@r��@r�!@rn�@r-@q��@q&�@q%@p��@p�9@pr�@pA�@p �@o�;@o\)@n�y@n��@n�+@nv�@nff@n@m��@m�@mO�@mV@l�@l��@k�F@kC�@j��@jM�@i�7@h�@g�@g;d@f�y@f��@fV@e��@e�-@e�h@e?}@d�/@d�D@dZ@c��@cS�@b�!@b~�@b�@a�^@a7L@`Ĝ@`bN@_�@_��@_��@_+@^�+@^E�@^$�@]��@]�@]V@\�/@\�D@[��@[C�@Z��@Z�\@Zn�@Y�#@Yhs@X�`@X�9@XQ�@W�@W\)@W;d@Vȴ@VE�@U��@U��@T�@Tz�@S��@St�@SC�@So@R�\@Q��@Q�^@Q%@P�@PbN@O�@O��@O;d@O�@N��@N�@N��@NE�@N@M��@M�-@M�h@Mp�@M/@L��@L�D@Lj@L9X@K�
@K��@Kt�@J�H@J^5@J�@I�@I�7@I&�@H�9@H�@G�w@G;d@F��@F�R@F5?@E�-@D�@D�D@DZ@DI�@D�@Cƨ@CdZ@CS�@CC�@B�H@B�\@A��@A7L@@r�@?��@?K�@>ȴ@>ff@>5?@>{@>@=�@=V@<�j@<9X@;��@;C�@;33@;C�@;C�@;C�@;"�@:��@:=q@9�#@9��@9hs@9�@8�`@8�@8A�@8 �@7�@7�P@7+@6ȴ@6{@5@5�@5?}@4�@4�@4��@4��@4��@4�@4�@4��@4I�@4(�@3�
@3�F@3��@3�@3S�@3@2��@2�\@2^5@2=q@1�@1X@0�`@0r�@0Q�@0  @/��@/l�@/\)@/+@.�R@.�+@.v�@.�+@.v�@.�+@.ff@-��@-�@-�@,�@,z�@,1@+�m@+��@+t�@+S�@+33@+@*n�@*-@*=q@*=q@*�@*J@)��@)G�@(�u@(b@(b@(b@(b@(  @'�P@&�@&V@&V@&{@%�T@%��@%�-@%�@%O�@$�j@$Z@$I�@$Z@$j@$9X@#�m@#��@#��@#ƨ@#dZ@"��@"�@"�!@"n�@"M�@"-@!�@!��@!hs@!x�@!�7@!X@ bN@��@;d@�@��@v�@V@5?@�T@�-@�h@`B@�@V@�/@�j@��@��@z�@Z@1@�m@��@t�@33@"�@o@��@^5@�@�#@�7@�7@G�@�@�@%@�`@�@A�@  @�w@�@��@K�@
=@�y@��@�+@�+@E�@@�@@��@�h@p�@p�@`B@/@�@��@�/@I�@�@�@��@��@��@��@��@dZ@@�H@��@~�@^5@^5@M�@-@�@�@�#@��@��@x�@X@X@G�@7L@�@%@��@�`@�`@Ĝ@bN@  @  @�@�;@�w@l�@K�@��@��@��@E�@�T@��@�h@?}@V@�@�@��@�@�@�DG�O�A�oA�VA�oA�JA�VA�{A�oA�VA�oA�oA�bA�VA�bA�oA�bA�VA�VA�bA�{A��A�oA��A��A��A��A��A��A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A� �A� �A��A��A� �A�"�A��A��A��A�"�A��A��A� �A�"�A��A��A� �A� �A��A�"�A�"�A� �A��A� �A�"�A��A��A�"�A�$�A� �A��A�$�A�&�A� �A� �A�$�A�$�A�"�A��A� �A�$�A�$�A��A�"�A�$�A�"�A��A� �A�$�A�$�A� �A�"�A�$�A�$�A�"�A� �A�$�A�$�A�"�A��A�"�A�$�A�"�A��A� �A�$�A�&�A�"�A�"�A� �A��A��A��A�"�A� �A��A� �A�"�A��A��A��A� �A��A��A�bA�bA�{A�VA�JA�
=A��A�{A�oA�VA��A��A�{A�VA�oA��A� �A��A��A��A��A��A��A��A� �A� �A��A��A��A��A��A�1A�
=A�JA�%A�  A���A�A�%A�{A�bA�JA�VA�oA�bA�bA�{A�oA�VA�1A�oA�{A��A��A�{A��A��A��A�{A��A��A��A��A��A��A� �A��A�{A��A��A�bA�VA�VA�{A�bA�VA�bA�A�1A�1A�%A�A�A�A�%A�1A�A�A�A�%A�A�A�  A�A�%A�%A�A�  A�A�A�A�  A�  A�A�A�%A�A�  A�  A�A�A�A�  A���A�A�A�A���A���A���A���A���A���A���A��A��A���A���A��A��A��A��A��A��A��A��yA��`A��`Aۺ^A�t�A�5?Aڝ�A�;dA�  A�1AבhA�z�Aԧ�A�/A���A�?}AмjAρA���Aδ9A�C�A��A���A��`A;wA�t�A�1'A�bA�  A��
A̍PA�1'A˺^A���AʓuA�^5AɸRA�A�jA�%A�~�A�^5A�$�A�A���A��HA�ƨAƥ�Aƙ�Aƕ�Aƕ�Aƕ�Aƕ�AƓuAƏ\AƍPAƋDAƉ7AƅAƇ+AƃA�~�A�|�A�z�A�v�A�z�A�x�A�v�A�r�A�l�A�hsA�`BA�=qA�$�A�A��A��
A���Aţ�Aŗ�A�x�A��A���A��Aĩ�A�|�A�hsA�VA���Aò-A�v�A�\)A�9XA�1'A��A��A©�ADAA�x�A�r�A�v�A�z�A�~�A�~�A�~�A�~�A�v�A�`BA�/A��FA�p�A�M�A�~�A��jA��7A�S�A�
=A��`A��A�bNA�G�A�33A���A�jA�9XA��A���A��A�l�A�E�A�&�A�{A�bA�VA�
=A�1A�%A���A��`A���A�ƨA���A�dZA�M�A�;dA�(�A�%A��#A���A��7A�XA��A��
A�S�A���A���A�S�A�$�A���A�VA�ffA���A�jA�33A��#A��9A��+A�ZA�E�A�7LA�"�A��wA�G�A���A�v�A�XA�5?A���A�|�A�(�A�A��
A��A�K�A�bA��`A��A�\)A� �A��A���A���A��uA�x�A�G�A��A���A��A���A��A�hsA�ZA�ZA�XA�Q�A�Q�A�O�A�I�A�A���A�~�A�bNA�bA���A�t�A�I�A��;A�5?A��TA�A�p�A�K�A�$�A�  A���A�\)A�?}A�/A��A��mA��FA�Q�A� �A��A��RA�p�A��/A��7A�E�A�VA��/A���A�p�A�^5A�K�A�7LA�$�A�A��A��#A�ĜA���A�ffA�VA���A��^A��hA�bNA�-A��mA�ĜA��PA�^5A�$�A�A���A���A��A���A�t�A��A���A�9XA��hA�;dA��
A��hA�?}A���A�n�A���A���A�ffA�{A��/A��A�|�A�E�A���A��^A�ZA��A��uA�
=A��A�ĜA��RA��A�VA�bA��-A�&�A�x�A���A��RA��A���A���A�hsA���A��^A��!A���A�ZA��-A��A�dZA��A��;A��RA��A�\)A�=qA���A��A��A��
A��!A��uA�bNA�;dA�%A�A�ffA��A�z�A�(�A��A�z�A� �A��FA�`BA�"�A�bA��7A�1'A��mA��PA�?}A��
A�C�A�%A���A�"�A��#A��A�v�A�^5A�=qA���A�oA��jA��A~�\A{
=Ax�DAw�TAw33AvbNAu��Au�FAu+AtbNAsC�Aq33ApJAo�-AoXAoAn��An�9An~�An�Am�FAl�Al9XAl�AlAk��AkAkt�Akp�AkS�AkS�Ak�Aj��Aj�/Aj�/AjĜAj��Aj�RAj�!Aj��AjffAj9XAjbAi��Ai;dAh��AhbNAhVAh=qAh�Ah1Ag��Ag�
Ag��Ag�7Agl�AgC�Ag"�AgoAf��Af�Af�/Af�+Af1'AfJAe�
AeƨAe��Ae�Ae?}Ae&�Ae�Ad��AeAd�AdȴAd��Ad�Ad��Ad�uAd�Adz�AdffAdVAdI�AdA�Ad9XAd5?Ad �AdAc��Ac�Ac�#Ac�FAc�hAc�Ac\)Ac/Ab�`Ab�!Abv�Aa��A_�A^jA]��A]�wA]�FA]��A]�A]�7A]�A]�A]�A]t�A]p�A]`BA]S�A]O�A]K�A]C�A]33A]+A]�A]A\�`A\��A\�jA\��A\~�A\Q�A\JA[��A[&�AZv�AY�mAY"�AXr�AW�FAW33AVVAUAU�AU;dAT�yAT��ATĜAT�RAT��AT��AT�uAT�uAT�\AT�+ATVAS�mAS/AQ�TAP��AO�mAO/AN��AN��ANjAN9XANAM��AMoAL��ALZAK��AK��AKl�AKG�AKK�AKS�AK?}AK�AJ�/AJ�!AJz�AJE�AJ$�AJ  AI�
AI��AI"�AHȴAH�+AHr�AHffAHbNAHZAHE�AH(�AG��AG�^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BیB�WB�WB�WB�WB�WB�#B�WBیB�#BیB�WB�WBیBیB�#B�#B�WB�#B�#B�#B��B�#BںBچB�BٴB�QB�QB��B��B�B�BٴBٴB�B�B��BרB�?B֡B՛B��B҉BϫB	_B	��B	�|B	��B
/�B
��B
��B
�B
�RB
�iB
��BB�B9�BK)Be�B��B�FB�UB��B�qB�qB�B�B��B�mB��B��B��B��B~�BiDBJ#B&LB�B
��B
�}B
�\B
��B
T�B
^jB
S[B
5�B
�B	�2B	�gB	��B	y�B	gmB	V�B	J�B	=�B	2aB	)�B	SB	JB��B�B�HB��B��B�xB	YB	
�B	�B	_B	-�B	NpB	tTB	|B	��B	��B	��B	�tB	�LB	�zB	��B	��B	�_B	��B	��B	�B	�$B	��B	�aB	��B	��B	�EB	��B	� B	��B	یB	��B
	�B
$B
&B
)�B
"�B
�B
�B
�B
OB
(�B
'�B
*0B
8B
B'B
GzB
B'B
J�B
A�B
B'B
T�B
WsB
W?B
U�B
R�B
R�B
U�B
P}B
R�B
S[B
R�B
Q�B
S&B
QNB
N�B
B�B
I�B
OvB
NB
IRB
Q�B
U�B
U�B
S�B
S&B
T�B
UgB
U�B
U�B
VB
V9B
VmB
WsB
W
B
W�B
X�B
W�B
XB
YKB
XEB
XEB
X�B
X�B
X�B
W�B
UgB
T�B
TaB
T,B
T,B
TaB
T,B
S�B
S[B
T�B
S[B
R�B
R B
Q�B
RTB
QNB
OBB
M�B
M6B
L�B
NB
M�B
N�B
K�B
LdB
K�B
I�B
JXB
I�B
H�B
I�B
GB
GB
F�B
FB
F?B
D�B
DgB
CaB
B�B
B[B
A�B
AUB
A B
A B
@�B
?�B
?�B
>�B
=�B
=B
<B
:*B
:*B
:�B
8B
5�B
4nB
1�B
/�B
.�B
.�B
-CB
*eB
'RB
%zB
"hB
OB
�B
�B
IB
"4B
%�B
'B
%�B
"4B
!�B
!�B
"�B
$@B
"hB
"hB
"4B
!�B
!�B
 \B
�B
�B
1B
{B
�B
�B
MB
oB
:B
B
�B
hB
�B
uB
{B
�B
�B
B
�B
:B
:B
�B
�B
hB
:B
B
B
�B
�B
�B
oB
�B
�B
 B
�B
.B
�B
\B
�B
�B
"B
"B
�B
�B
�B
�B
�B
�B
B
PB
B
PB
�B
PB
B

�B

	B
�B
�B
�B
+B
�B
�B
%B
B
B
�B
SB
�B
�B
fB
1B
	B
	B
�B

rB

	B
B
PB
"B
"B
�B
"B
�B
�B
�B
�B
�B
"B
VB
B
�B
JB
B
�B
B
~B
~B
�B
�B
JB
�B
~B
�B
�B
�B
�B
�B
JB
�B
�B
(B
�B
(B
�B
.B
:B
�B
(B
�B
.B
�B
�B
\B
�B
�B
�B
bB
bB
bB
bB
 B
hB
�B
�B
hB
�B
B
@B
�B
�B
B
B
B
�B
�B
�B
�B
+B
+B
�B
1B
7B
B
kB
	B
=B
�B
�B
IB
IB
B
�B
~B
�B
~B
~B
~B
B
�B
OB
B
�B
�B
xB
xB
�B
�B
xB
�B
IB
�B
IB
~B
IB
~B
~B
�B
�B
�B
B
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
"�B
#B
#:B
#:B
"�B
"�B
#�B
"�B
#�B
#�B
$@B
$B
$@B
$B
%B
%FB
%B
%FB
%FB
%zB
%zB
%�B
&LB
&B
&B
&LB
&�B
&�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
(�B
*�B
*eB
+B
+kB
+kB
+6B
+6B
+�B
,qB
,B
,=B
-B
-wB
.}B
.�B
-�B
-�B
,�B
-�B
-wB
-�B
-wB
.}B
.}B
/B
/B
/�B
/�B
/�B
0�B
0�B
0�B
1'B
1'B
1[B
1[B
1�B
1�B
1�B
1�B
2-B
1�B
2-B
33B
3�B
3�B
3hB
33B
2�B
5tB
4nB
4nB
4�B
4B
4�B
5B
6B
5tB
6FB
6�B
6zB
6zB
7B
7LB
7LB
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8B
8RB
8�B
9�B
9�B
9�B
9�B
;0B
;dB
:�B
;dB
;0B
<B
<jB
;�B
<6B
<B
;�B
<B
;�B
;�B
<�B
<�B
<�B
=B
=<B
=<B
=B
=<B
=qB
=�B
>�B
?}B
?�B
?�B
@OB
@�B
@�B
@�B
@�B
A B
A B
@�B
B�B
B'B
B�B
B�B
B'B
C�B
D3B
D�B
D�B
D�B
EmB
E9B
EmB
EmB
E�B
E�B
E�B
E�B
F?B
GB
GEB
GEB
HB
G�B
HKB
HB
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K^B
L0B
L�B
L�B
LdB
M6B
M6B
M�B
M�B
NB
NpB
N�B
NpB
OB
OBB
OvB
OBB
O�B
PB
P}B
P�B
P�B
P�B
QNB
Q�B
Q�B
R B
Q�B
R B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S[B
S[B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T,B
TaB
T�B
U2B
U�B
V�B
V�B
W
B
W
B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
YB
Z�B
Z�B
[#B
[WB
[�B
[�B
\)B
\)B
[�B
[�B
[�B
\)B
\]B
\]B
\]B
\]B
\]B
\�B
\�B
\�B
\�B
]�B
]dB
]�B
^�B
_B
_pB
_�B
`BB
aB
`�B
aB
a�B
bB
a�B
bB
b�B
b�B
b�B
c B
c�B
c�B
c�B
d�B
d�B
d�B
d&B
d&B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
e`B
e`B
e�B
ffB
f�B
ffB
f�B
gB
gB
gB
gmB
h>B
h�B
h�B
h�B
iDB
iDB
hsB
gmB
gmB
g�B
h>B
h
B
h
B
h�B
h�B
hsB
h�B
h�B
iB
i�B
i�B
jB
jB
jB
jB
jKB
k�B
kQB
k�B
l"B
lWB
l�B
l�B
m)B
m�B
ncB
n�B
o�B
o�B
o�B
o�B
p;B
p;B
p�B
p�B
poB
qB
qvB
q�B
qvB
rB
rB
rB
q�B
rB
q�B
rB
rB
rB
rB
rGB
r|B
r�B
r�B
s�B
s�B
s�B
tB
tB
s�B
t�B
u%B
uZB
t�B
t�B
tTB
s�B
tB
t�B
u%B
tTB
r�B
rGB
rB
r�B
r|B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u%B
u�B
uZB
u%B
u%B
v`B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x8B
xlB
x8B
w�B
xlB
x�B
x�B
y�B
yrB
yrB
y�B
zxB
zDB
z�B
{B
{B
{�B
{B
|PB
}VB
|�B
}VB
}�B
}�B
}VB
}�B
~�B
~�B
~�B
.B
cB
�B
�4B
� B
� B
�iB
�;B
��B
��B
�;B
�oB
�;B
�AB
�AB
��B
�B
�B
�GB
�GB
�GB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�B
��B
�SB
�B
��B
��B
�SB
�%B
�SB
��B
��B
��B
��B
��B
�_B
��B
��BچBܒB�QB��B�]BچB��B��B��BچBیB�]B�#BںB�WB�)B�]BیB�QB�QB��BٴBںB�]B�]BیB��BیBܒB�)B�QB�WB��B�QB�#B�dBیB�QB��BܒB��B�QB�]B�]B�QB�WB�)B�WBچBیB�]B�#BںBیB�]B��B�QB��BܒB��BچB��BܒB�#BچB��B�)BںBچB�]B�#B�QBیB�]B��B�BیB�)B�QBٴB�#B��BچBچB�)B�)BںB�#B�QB�)B�#B�BیB��BںB�B��B�)B�WB�QBںB��B�WB�QB�QB��B�]BںB�QB��B��BںBٴB��B��B�WBچB��B��B��B��B�BܒBیB�B�B�#BںB�KB�#B�)B�B�yB�QB��BیB��B�B�BچBٴB�KB�KBخB�BخB�sBخBچB�QB�sB�BچB�WBچB�BٴB��B��B�B�B�QB��B�BٴB�EB�BٴB��B��B�B�B��B�mB�QB�QBٴBרB֡B��BںB�B�yB�B��B��B�KB�B��B�QB��B�B�QBچB�KB��BרBٴBںBٴB�EB�B�#BںB�QB�B�QB��B�BרB�EB�QB��B�B�mB�sB�B��B�EB�
B֡B��BخB�B�?B�
B�BخB��B��B�
B�B�EB�sB�mB�
B�EB�B�
B�9B՛B�mB�?B�sB�mB՛B��B�mB֡B՛B��BԕB��B�
B�9BԕBӏB��B��B�2B�,B��B�TBҽB��B҉B�}B�BB�B��B��B͟B��BԕB�B�B�%B��B��B	�B	B�B	NpB	�iB	s�B	~]B	ĜB	ܒB	�B	� B	�B	��B	�5B	��B	�AB	��B	�>B
oB	�PB	�B
�B
DB
B
)�B
E9B
8RB
A B
^B
s�B
zDB
�7B
��B
�B
�B
�aB
�9B
�0B
��B
�?B
ŢB
�tB
�?B
��B
ŢB
�B
�B
�B
�KB
�KB
��B
�EB
�B
�KB
��B
��B
�XB
ȀB
ȀB
ȴB
ɺB
�)B
˒B
�dB
֡B
��B
�mB
�5B
��B
��B
��B
�>B
��B �B
�8B
��B;BAB  B.B
�cB
�.B
��B
�"B
�DB
�fB
��B  BBoB  B �B
��B
��B
��BAB�B�B�B�BhBeB=B�B	7B�B�BSB	�B_B�B0�B4�B6�B6FBF�B<�B=qBDgBB'B?�BA�BE�BGzBHKBNBOBOvBO�BN�BOBT,BYB\�B`vBd�Be�BffBiyBpBo BpB}VBqB}"B}VB��B�B�4B��B��B�	B�-B��B��B��B��B��B�nB�B�B�B�hB�*B�3B��B�B��B�!B�B�^B��B��B��B�$B��B�0B�B��B��B��B�B��B�qB�B��B��B� B��B�wB�B�}B��B��B�B�?B��B�nB��B�3B��BŢB��B��B��B�XB��B��B�B��B��B�gBɆB�#B��B��B�wB�RB��B�gB��B�B��B��B�tB��B��B�^B�B��B�}B�B��B��B��B��B�7B�7B�+B�B��B�:B�\B�(B�VB��B�.B�B��B��B�AB�oB�uBtB}�Bw�Bm�BjKBncBf�Bu�BZ�BW�BZ�BR�BQNBR B4nB<�B49B2aB/�B+B($B�B+B�B�B�B1B�B�B
��BoB
��B
�B
�JB
�B
�B
��B
�B
�&B
ںB
�sB
�5B
�B
��B
��B
��B
��B
��B
�B
�CB
��B
��B
�rB
�B
�rB
�AB
m�B
`vB
Z�B
V�B
V�B
Q�B
M6B
^�B
QNB
QB
[#B
[�B
[WB
^�B
\�B
a�B
c�B
]dB
`BB
Z�B
K�B
L�B
P�B
I�B
JXB
A�B
0�B
.�B
I�B
($B
-�B
 \B
CB
B
"hB
�B
~B
�B
�B	�(B	�B	�vB	��B
  B	�B	��B	��B	�B
�B	�^B	�0B	�B	�$B	��B	�.B	�MB	��B	��B	��B	��B	zxB	pB	qAB	m]B	lWB	k�B	q�B	h�B	m]B	e,B	^B	Z�B	ZB	a�B	_;B	R�B	W�B	PB	Y�B	U�B	S�B	O�B	R B	OvB	N�B	N<B	L�B	X�B	HB	FB	T,B	PB	PHB	CaB	CaB	C�B	C�B	B[B	?�B	A�B	A�B	>�B	?�B	A B	?B	>�B	<B	;�B	;dB	C�B	B�B	:�B	:�B	6�B	7B	6B	8RB	0UB	4B	2�B	3�B	33B	6�B	/�B	4nB	-�B	0UB	/OB	.�B	.}B	-�B	.B	-wB	-CB	,�B	,B	-�B	*�B	*�B	+6B	+B	(�B	'�B	&�B	&�B	%zB	!bB	!�B	,�B	>B	(�B	�B	SB	{B	MB	uB	B	�B	 B	4B	B	�B	�B	hB	�B	�B	�B	�B	(B	�B	�B	�B	B	DB	
�B	
	B	B	�B	�B	B	�B	�B	�B	�B��B�(B	�B	
rB��B��B�B��B� B��B�5B� B�)B�B�B�yB�WB��B��B��B�+B�B�yB�B�HB�B�HB�B�B�B�B�/B��B�BB�;B��B��B�B�B�B�B�sB�B�KB�B�B�QB�/B��B��B�"B�B�KB�B�B��B��B�/B�;G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     BۦB�qB�WB�=B�WB�qB�=B�qBیB�=BۦB�WB�WBۦBیB�=B�=B�qB�=B�=B�=B�	B�=B�	BںB�BٚBچBںB��B��B�BٚB��B�B�KB�+B��B��B�YBּB��B�gB�{BߤB	,"B	�qB	�>B
�B
?�B
�0B
�B
��B
ΊB
��B�B�B#�BCBP�Br|B�eB��B��B��B�B�[B�JB�-B��B̈́B�JB��B�vB�mB��By>BZ�B2�B,B
��B
��B
��B
�NB
aB
l"B
bNB
E�B
&2B
�B	�B	�eB	��B	mwB	]�B	S[B	D�B	88B	;JB	�B	 B		�B��B�B��B��B	B		lB	hB	�B	�B	/5B	P�B	vB	~BB	�YB	��B	�QB	�B	��B	��B	�KB	��B	�/B	�FB	��B	��B	��B	��B	�B	�JB	�iB	�zB	��B	�B	�{B	چB	��B
	7B
?B
)_B
4B
'�B
KB
B
OB
VB
-�B
)�B
)yB
7LB
C-B
J�B
E9B
P.B
C�B
A;B
T�B
W�B
X�B
W�B
T,B
V9B
Y�B
Q�B
S�B
S�B
S@B
R�B
T�B
SuB
R�B
B�B
JrB
QNB
PHB
H�B
R�B
W�B
W�B
T�B
S�B
U�B
V�B
W�B
W?B
W�B
W�B
W?B
W�B
W�B
X�B
YeB
XyB
YB
YB
X�B
YeB
Z�B
Z�B
[=B
Y1B
V9B
U�B
T�B
TFB
TaB
T�B
TaB
S�B
TFB
W$B
S�B
R�B
RTB
R�B
T�B
SB
P�B
OBB
N�B
NpB
N�B
NVB
PHB
MB
N"B
LB
JrB
KxB
JXB
J�B
KB
G�B
G�B
G�B
G�B
GEB
E�B
E�B
D�B
D3B
C�B
B�B
A�B
A�B
A�B
A;B
@�B
@�B
@ B
>�B
>�B
>(B
;�B
<�B
=�B
9�B
88B
6�B
2�B
0UB
/�B
0;B
.�B
+�B
)_B
(>B
$�B
;B
�B
�B
jB
!�B
%�B
)DB
'mB
"�B
!�B
"�B
%B
%B
"�B
#B
"�B
"4B
"4B
!-B
!bB
#�B
�B
�B
�B
�B
YB
uB
uB
�B
�B
oB
oB
�B
gB
KB

B
B
�B
�B
uB
,B
�B
@B
@B
�B
,B
�B
�B
�B
�B
�B
�B
NB
�B
}B
HB
�B
�B
�B
(B
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
VB
�B
"B
�B
�B
DB

�B
�B

XB
�B
�B
tB
zB
SB
YB
�B
�B
�B
�B
fB
KB
	RB

=B
PB
B
	�B

�B
�B
pB
pB
<B
(B
"B
VB
�B
VB
VB
vB
�B
�B
dB
�B
B
�B
�B
�B
B
�B
jB
B
�B
~B
B
JB
B
VB
�B
VB
(B
VB
�B
�B
�B
B
�B
&B
.B
�B
.B
hB
�B
\B
�B
�B
�B
NB
�B
�B
�B
�B
�B
�B
�B
 B
:B
B
uB
�B
�B
�B
�B
B
�B
�B
B
yB
B
�B
�B
�B
B
�B
�B
WB
qB
�B
�B
�B
5B
�B
�B
B
B
OB
�B
B
�B
�B
�B
 BB
~B
�B
�B
�B
IB
�B
B
�B
�B
�B
�B
�B
�B
�B
B
5B
�B
B
5B
�B
;B
!B
!B
pB
�B
!HB
"4B
"hB
# B
#nB
#nB
#�B
#�B
"�B
#�B
#�B
#B
$&B
$&B
$ZB
$@B
$�B
%,B
%�B
%�B
%B
%`B
%zB
%�B
%�B
&�B
&�B
&2B
&LB
&�B
'B
'B
'B
'mB
'�B
'�B
(
B
(sB
(�B
*KB
+�B
*�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
,WB
,�B
-�B
-�B
/ B
/5B
.IB
-�B
-)B
.B
-�B
.IB
.B
/5B
/OB
0B
/�B
/�B
/�B
0B
1'B
1B
0�B
1�B
1vB
1�B
1�B
2aB
2B
2GB
2�B
2�B
2-B
2�B
4B
49B
3�B
3�B
3�B
4�B
5�B
4�B
4�B
4�B
4�B
5ZB
6B
6`B
5�B
6�B
6�B
6�B
6�B
7fB
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8RB
8�B
8�B
9rB
:xB
:�B
:DB
:�B
;�B
;�B
;dB
;�B
<B
=�B
<�B
;�B
<�B
<B
<B
<PB
;�B
<jB
=B
<�B
<�B
=VB
=�B
=qB
=<B
=�B
=�B
>BB
>�B
?�B
@ B
@ B
@�B
A B
@�B
@�B
A;B
A�B
AUB
A�B
C-B
B�B
CB
CaB
C-B
D�B
D�B
D�B
D�B
EB
E�B
EmB
E�B
E�B
FB
F%B
FB
F?B
F�B
G�B
GzB
G�B
H�B
HfB
H�B
H�B
H�B
IB
IB
IlB
JXB
J=B
J#B
J=B
KDB
J�B
J�B
J�B
K�B
LB
L�B
L�B
L�B
MB
M�B
M�B
NB
NB
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
PbB
P�B
Q B
P�B
P�B
QhB
Q�B
Q�B
RoB
R�B
R B
R�B
R�B
R�B
R�B
SB
R�B
S&B
S[B
S�B
S�B
S�B
S�B
S�B
S�B
T,B
TFB
T,B
TaB
T�B
T�B
UgB
VmB
W$B
W$B
W?B
WsB
XB
XB
X+B
X_B
Y1B
X�B
X�B
YeB
YeB
Z7B
Z�B
[#B
[=B
[�B
[�B
\]B
\CB
\CB
\]B
\]B
\�B
\�B
]/B
]/B
\�B
\�B
\�B
\�B
\�B
\�B
]IB
^B
]�B
^OB
_;B
_VB
_�B
_�B
`BB
aB
aB
a|B
bhB
b�B
b4B
bNB
b�B
b�B
b�B
cnB
c�B
c�B
dZB
d�B
e,B
ezB
dtB
dtB
dB
c�B
c�B
c�B
c�B
d�B
d�B
e`B
ezB
fB
f�B
f�B
f�B
f�B
gB
g8B
gRB
g�B
h�B
iB
iB
iB
i�B
i�B
h�B
g�B
g�B
h>B
hsB
h$B
h>B
iDB
h�B
h�B
h�B
h�B
iB
jB
j�B
j�B
j0B
jKB
jB
j�B
k�B
k�B
lB
l=B
lqB
l�B
mCB
m]B
m�B
ncB
n�B
o�B
o�B
pUB
p�B
p�B
p;B
p�B
p�B
p�B
q�B
r-B
raB
q�B
rGB
rGB
r-B
rB
rGB
rB
r�B
r|B
r-B
rB
rGB
r�B
s3B
r�B
s�B
s�B
tB
t�B
tB
t9B
u%B
u?B
utB
uB
t�B
t�B
s�B
tB
u?B
vB
uB
sMB
r|B
r|B
r�B
r�B
r�B
s3B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t9B
t�B
t�B
t�B
uB
u%B
u?B
uZB
utB
vB
utB
utB
u�B
v�B
vFB
wLB
v�B
v�B
w�B
w�B
w�B
xB
x8B
xB
x�B
x�B
x�B
xRB
xB
x�B
x�B
y$B
y�B
yrB
y�B
z*B
z�B
zxB
z�B
{0B
{0B
{�B
{0B
|�B
}qB
|�B
}�B
~B
}�B
}VB
}�B
B
B
~�B
HB
�B
�4B
�iB
�OB
�B
��B
�;B
��B
��B
�UB
��B
�UB
�[B
�uB
��B
�-B
�B
�aB
�aB
�aB
��B
��B
��B
��B
��B
�MB
��B
��B
��B
�B
��B
�mB
�B
�mB
��B
�mB
�mB
�%B
��B
��B
�tB
��B
��B
��B
��B
��B
��B
��B
��G�O�BچBܒB�QB��B�]BچB��B��B��BچBیB�]B�#BںB�WB�)B�]BیB�QB�QB��BٴBںB�]B�]BیB��BیBܒB�)B�QB�WB��B�QB�#B�dBیB�QB��BܒB��B�QB�]B�]B�QB�WB�)B�WBچBیB�]B�#BںBیB�]B��B�QB��BܒB��BچB��BܒB�#BچB��B�)BںBچB�]B�#B�QBیB�]B��B�BیB�)B�QBٴB�#B��BچBچB�)B�)BںB�#B�QB�)B�#B�BیB��BںB�B��B�)B�WB�QBںB��B�WB�QB�QB��B�]BںB�QB��B��BںBٴB��B��B�WBچB��B��B��B��B�BܒBیB�B�B�#BںB�KB�#B�)B�B�yB�QB��BیB��B�B�BچBٴB�KB�KBخB�BخB�sBخBچB�QB�sB�BچB�WBچB�BٴB��B��B�B�B�QB��B�BٴB�EB�BٴB��B��B�B�B��B�mB�QB�QBٴBרB֡B��BںB�B�yB�B��B��B�KB�B��B�QB��B�B�QBچB�KB��BרBٴBںBٴB�EB�B�#BںB�QB�B�QB��B�BרB�EB�QB��B�B�mB�sB�B��B�EB�
B֡B��BخB�B�?B�
B�BخB��B��B�
B�B�EB�sB�mB�
B�EB�B�
B�9B՛B�mB�?B�sB�mB՛B��B�mB֡B՛B��BԕB��B�
B�9BԕBӏB��B��B�2B�,B��B�TBҽB��B҉B�}B�BB�B��B��B͟B��BԕB�B�B�%B��B��B	�B	B�B	NpB	�iB	s�B	~]B	ĜB	ܒB	�B	� B	�B	��B	�5B	��B	�AB	��B	�>B
oB	�PB	�B
�B
DB
B
)�B
E9B
8RB
A B
^B
s�B
zDB
�7B
��B
�B
�B
�aB
�9B
�0B
��B
�?B
ŢB
�tB
�?B
��B
ŢB
�B
�B
�B
�KB
�KB
��B
�EB
�B
�KB
��B
��B
�XB
ȀB
ȀB
ȴB
ɺB
�)B
˒B
�dB
֡B
��B
�mB
�5B
��B
��B
��B
�>B
��B �B
�8B
��B;BAB  B.B
�cB
�.B
��B
�"B
�DB
�fB
��B  BBoB  B �B
��B
��B
��BAB�B�B�B�BhBeB=B�B	7B�B�BSB	�B_B�B0�B4�B6�B6FBF�B<�B=qBDgBB'B?�BA�BE�BGzBHKBNBOBOvBO�BN�BOBT,BYB\�B`vBd�Be�BffBiyBpBo BpB}VBqB}"B}VB��B�B�4B��B��B�	B�-B��B��B��B��B��B�nB�B�B�B�hB�*B�3B��B�B��B�!B�B�^B��B��B��B�$B��B�0B�B��B��B��B�B��B�qB�B��B��B� B��B�wB�B�}B��B��B�B�?B��B�nB��B�3B��BŢB��B��B��B�XB��B��B�B��B��B�gBɆB�#B��B��B�wB�RB��B�gB��B�B��B��B�tB��B��B�^B�B��B�}B�B��B��B��B��B�7B�7B�+B�B��B�:B�\B�(B�VB��B�.B�B��B��B�AB�oB�uBtB}�Bw�Bm�BjKBncBf�Bu�BZ�BW�BZ�BR�BQNBR B4nB<�B49B2aB/�B+B($B�B+B�B�B�B1B�B�B
��BoB
��B
�B
�JB
�B
�B
��B
�B
�&B
ںB
�sB
�5B
�B
��B
��B
��B
��B
��B
�B
�CB
��B
��B
�rB
�B
�rB
�AB
m�B
`vB
Z�B
V�B
V�B
Q�B
M6B
^�B
QNB
QB
[#B
[�B
[WB
^�B
\�B
a�B
c�B
]dB
`BB
Z�B
K�B
L�B
P�B
I�B
JXB
A�B
0�B
.�B
I�B
($B
-�B
 \B
CB
B
"hB
�B
~B
�B
�B	�(B	�B	�vB	��B
  B	�B	��B	��B	�B
�B	�^B	�0B	�B	�$B	��B	�.B	�MB	��B	��B	��B	��B	zxB	pB	qAB	m]B	lWB	k�B	q�B	h�B	m]B	e,B	^B	Z�B	ZB	a�B	_;B	R�B	W�B	PB	Y�B	U�B	S�B	O�B	R B	OvB	N�B	N<B	L�B	X�B	HB	FB	T,B	PB	PHB	CaB	CaB	C�B	C�B	B[B	?�B	A�B	A�B	>�B	?�B	A B	?B	>�B	<B	;�B	;dB	C�B	B�B	:�B	:�B	6�B	7B	6B	8RB	0UB	4B	2�B	3�B	33B	6�B	/�B	4nB	-�B	0UB	/OB	.�B	.}B	-�B	.B	-wB	-CB	,�B	,B	-�B	*�B	*�B	+6B	+B	(�B	'�B	&�B	&�B	%zB	!bB	!�B	,�B	>B	(�B	�B	SB	{B	MB	uB	B	�B	 B	4B	B	�B	�B	hB	�B	�B	�B	�B	(B	�B	�B	�B	B	DB	
�B	
	B	B	�B	�B	B	�B	�B	�B	�B��B�(B	�B	
rB��B��B�B��B� B��B�5B� B�)B�B�B�yB�WB��B��B��B�+B�B�yB�B�HB�B�HB�B�B�B�B�/B��B�BB�;B��B��B�B�B�B�B�sB�B�KB�B�B�QB�/B��B��B�"B�B�KB�B�B��B��B�/B�;G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�}�=��<�݆<#�
<J��<�L:<#�
<#�
<#�
<#�
<6s�<#�
<<[�<�� <*�u<#�
<_�<�{;<[�<f�V<BN�<#�
<#�
<#�
<R��<@�<#�
<KL�<#�
<#�
<#�
<9fg<���<� �<Zr�<f�V<Zr�<X��<�=�<�$�<R;�<j��<}\ <�}�<��^<�e�<�݆<�U<#�
<#�
<#�
<#�
<#�
<#�
<��w<#�
<���<z?<��D<+x'<#�
<1Q�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<.c=<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<77<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018061204463020180612044630IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018062207035620180622070356QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018062207035620180622070356QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107550820190521075508IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                