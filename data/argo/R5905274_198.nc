CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-10T12:00:47Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɠ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  р   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � `   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � g    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230610120047  20230610120047  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�/^d��@�/^d��11  @�/^O�	`@�/^O�	`@0�1���
@0�1���
�d�jj&�d�jj&11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AB  AB  ?�  ?��H@:�H@z�H@�p�@��R@�G�A   A  A   A+�A@��A`  A�  A�Q�A���A�Q�A�  A�  A�Q�A�Q�B Q�B�
B�
B  B�
B(  B0  B8  B?�
BH  BP(�BW�
B`(�BhQ�Bp(�BxQ�B�=qB�(�B�{B�  B�  B�{B�=qB�=qB�{B��
B�  B�{B�(�B�{B�{B��B��B�  B��B��B��B�  B��B��
B��
B��B�{B�(�B�{B�  B�  B��B��
C��C  C  C  C

=C
=C��C�C  C
=C
=C
=C
=C  C��C��C!��C$  C&  C'�C)�C,  C.
=C0  C2
=C4  C5��C8
=C:{C<  C=�C@  CA��CC��CE�CG�CI��CK�HCM�CO��CQ��CS��CV
=CX  CY��C[�C]��C`  Cb
=Cd
=Cf  Ch{Cj{Cl
=Cn  Co��Cq��Ct  Cv
=Cx�Cz{C|  C~  C�  C���C���C�  C���C���C���C�  C�C�
=C�  C���C���C���C���C���C�C���C���C�C�C���C�  C���C���C�  C�  C�  C�  C�  C���C�  C�  C�  C���C���C�C�C���C���C�C�  C�  C�C���C�  C�
=C�C�  C�  C���C���C���C���C���C�  C�  C�  C���C��C���C�  C���C���C�C�
=C�  C���C���C�  C�C�
=C���C���C�C�  C���C�  C�\C�
=C�  C���C�C�
=C���C��C�  C�C�  C���C���C���C�  C�C�  C���C�  C���C�  C�
=C�  C�  C�
=C�C�  C���C�  C�  C�  C�C�  C���C�  C�  C�C�\C�  C���C�C�  C���C�  C�
=C�  C���C���C���C�  C���D }qD �qDz�D�qD��D�D��DD��DD�D�D��D�D��D�qDz�D�qD	��D
�D
z�D
�qD�D�qDxRD�qD}qD  D��D�D� D�qD��D�D�D��D� D�D� D�D}qD�D� D�RD� D  D}qD  D� D  D� D  D� D�qD}qD�RDz�D�D�D�qD� D�D��D   D }qD!  D!z�D"�D"��D#D#� D$  D$}qD%  D%��D&  D&� D'�D'�D(D(� D)  D)��D*  D*xRD+  D+� D,  D,�=D-�D-}qD.  D.� D/D/}qD0  D0� D1  D1� D2  D2� D2�qD3� D3��D4}qD5D5��D5�qD6� D7�D7� D7�qD8��D8�qD9��D:D:}qD:��D;��D<�D<}qD<��D=��D=�qD>}qD?  D?��D@D@�DA�DAz�DB  DBz�DCDC��DDDD�DEDE}qDF  DF��DF��DG� DHDH��DI�DI��DJ�DJ}qDK  DK�DL  DL� DL�qDM}qDN  DN}qDN��DO�DP  DP}qDP�qDQz�DQ�qDR}qDSDSz�DS�qDT��DT��DUz�DU�qDV}qDV�qDWz�DW��DXz�DY  DY��DZDZ� DZ�qD[� D[�qD\}qD]�D]�D^  D^z�D^��D_� D`D`��D`�qDaz�Db  Db��Dc�Dc��Dd�Dd��De  De}qDf�Df�DgDg�Dh�Dhz�Dh�qDi}qDj�Dj��Dk  Dk��DlDl}qDl�qDm��Dm�qDn��Do�Do}qDo�qDp� Dp�qDq� DrDr}qDs�Ds}qDs�qDt��DuDu��Du�qDv}qDv�qDw� Dx  Dx� Dy  Dy��Dz  Dz��D{�D{� D|  D|� D}�D}�D~  D~��D  D� D�qD�AHD��HD���D��D�AHD�� D�� D���D�@ D�~�D��HD�  D�@ D�� D�D���D�@ D��HD���D�  D�AHD�� D�� D��D�>�D�|)D�� D�  D�AHD�� D��HD���D�@ D���D��HD�  D�AHD�� D���D�  D�>�D�~�D��qD�  D�>�D��HD�� D�HD�@ D�|)D�� D���D�>�D�~�D�� D�  D�>�D��HD���D�  D�>�D�~�D��HD�HD�>�D�~�D�� D���D�=qD�~�D�� D��qD�=qD�}qD���D�HD�@ D�~�D��HD��D�@ D�~�D�� D���D�>�D�� D�� D���D�>�D�� D��HD�HD�>�D���D��qD�HD�>�D�� D���D���D�B�D�~�D���D��D�AHD��HD��HD���D�>�D��HD���D���D�@ D�� D���D��D�AHD�}qD���D�HD�AHD��HD���D���D�B�D�~�D��qD�  D�>�D�� D��qD�  D�>�D�� D���D��qD�AHD��HD�D�  D�B�D��HD��qD�  D�@ D�� D�� D���D�=qD��HD���D�  D�B�D��HD�� D�HD�AHD��HD�D��qD�=qD��HD�� D��)D�>�D��HD��qD�  D�=qD��HD���D��D�AHD�~�D��HD��D�>�D�� D���D�  D�@ D�~�D���D�HD�AHD�� D�� D���D�>�D�}qD��HD�  D�AHD���D��HD�HD�AHD�~�D��HD��qD�@ D�� D���D�HD�>�D�}qD�� D��qD�@ D�}qD��qD�  D�>�D�}qD�� D�HD�>�D�~�D��HD�  D�>�D�~�D�� D�HD�@ D�� D��HD�HD�AHD�� D��HD�  D�=qD�~�D�� D�  D�@ D�}qD��qD�HD�C�D�� D���D���D�AHD���D���D��qD�>�D�~�D���D�HD�AHD�� D�� D�HD�B�D��HD�� D�  D�>�D�}qD���D���D�>�DHD�� D��qD�=qD�}qDýqD��qD�>�D�}qDľ�D���D�@ Dŀ D�� D�  D�=qD�|)Dƾ�D�HD�>�DǁHD�D�  D�=qD�~�D�� D�  D�@ DɁHD�D��D�B�Dʂ�D���D�HD�@ DˁHD��HD�  D�AHD̀ D�� D�  D�@ D̀ D��HD�HD�B�D΁HD�D�  D�=qD�~�DϾ�D�HD�@ D�|)Dо�D�  D�>�DсHD�� D���D�@ DҀ DҾ�D���D�=qD�~�D�� D�  D�@ D�~�D�� D���D�<)DՀ D�D��D�C�Dւ�D��HD���D�=qD�~�D׾�D��qD�@ D�~�Dؾ�D�  D�AHDق�D��HD�  D�@ DځHD�D�HD�>�Dۀ D۾�D��)D�=qD܀ D��HD��D�AHD�~�DݽqD���D�>�D�}qD޾�D��qD�@ D߂�D߾�D�HD�=qD�� D�D�  D�@ D� D��HD��qD�>�D� D�D��D�B�D�HD�� D�  D�AHD� D�� D���D�@ D�HD�� D�  D�B�D� D��HD�  D�>�D� D羸D���D�>�D� D�D�  D�@ D�HD�� D�  D�AHD�~�D�� D�  D�@ D� D�� D�  D�>�D� D��HD�  D�AHD� D���D�  D�AHDD��HD��qD�<)D�~�DﾸD���D�@ D�� D�D�HD�@ D� D�� D�  D�=qD� D��HD���D�@ D�}qD�D��qD�=qD�}qD���D�  D�>�D�~�D���D�  D�B�D�� D�D��D�@ D�}qD�� D�HD�>�D�}qD���D��D�@ D�� D�D��D�C�D�o\?��?#�
?L��?�  ?���?���?Ǯ?�G�@�\@\)@(�@(��@8Q�@G�@c�
@s33@�G�@�=q@���@�Q�@�G�@�=q@�33@��H@��
@˅@�33@�(�@��@���@�@��RAz�A��A{A�A
=A�A   A$z�A)��A.{A2�\A7
=A<(�A@��AEAI��AN�RAS33AXQ�A\(�AaG�Ac�
Ah��Amp�Aq�AvffA{�A�  A��HA���A�\)A�G�A��
A�{A�Q�A��HA��A�\)A��A�(�A�ffA��HA��A��A���A�(�A��RA�G�A�33A�p�A�\)A�G�A��A�A�  A\A���A�
=A�G�A˅A�{A�  Aҏ\A���A�
=Aٙ�A��
A�{A�Q�A�\A���A�
=A���A�A�A�Q�A��HA��A�\)A���A��
A�{B (�BG�BffB�Bz�B��B�RB�B��B	�B
=B  B�B=qB33BQ�Bp�B�\B�B��BB�HB  B��B{B33BQ�BG�BffB�B ��B!��B"�HB#�B$z�B%p�B&�RB'�B(��B)B+
=B,  B-��B.=qB/\)B0(�B1G�B2ffB3�B4z�B5��B6�RB8(�B9�B:=qB;\)B<Q�B=p�B>ffB?�B@z�BA��BB=qBC�BDQ�BEp�BF�\BG�BHz�BI��BJ�RBK�
BL��BMBN�RBP  BP��BR{BS\)BTz�BUp�BV�\BW�BXz�BYBZffB[\)B\Q�B]G�B^ffB_\)B`z�Bap�Bb�\Bc�
Bd��Be�Bg
=Bh  BiG�Bj=qBk
=Bk�
Bl��Bm�Bn�HBp  Bp��Bq�Bs33Btz�Bu��Bv�RBw�Bx��ByBz�HB|  B|��B}��B~�RB�B�ffB��HB�p�B��B��\B�33B�B�=qB��HB�p�B��
B�=qB���B�33B�B�Q�B��RB�\)B�B�z�B��B���B�(�B��RB��B��B�  B��\B��B���B�=qB���B�p�B�  B�z�B�
=B���B�  B�ffB���B�p�B�  B�z�B��B��
B�Q�B��HB�\)B�  B�z�B��HB�\)B��B�ffB�
=B�\)B�  B��RB�G�B�B�Q�B��HB�\)B��
B�(�B��RB�G�B�B�=qB���B�G�B�  B��\B�
=B���B�{B�ffB��HB�p�B��
B�ffB��HB��B�{B��\B��B��B��
B�ffB���B�G�B��
B�z�B���B�p�B��B�ffB��HB�G�B���B�{B��\B�
=B�B�=qB���B��B���B��B�Q�B���B�G�B��
B�Q�B��HB�33B��B��B�ffB���B��B�  B�ffB���B�\)B��B�{B���B�
=B��B�(�B£�B�33BÙ�B�(�Bď\B��HB�\)B�B�ffB���B�p�B�  B�z�BȸRB�33BɮB�(�B��HB�\)B��B�=qB̸RB��BͮB�Q�B���B�p�Bϙ�B�(�BЏ\B�33B�B�=qBҏ\B���BӅB�  Bԣ�B�33BՅB�{B�ffB�33B�B�Q�Bأ�B��Bٙ�B�Q�B��HB�p�B�B�Q�B���B݅B�(�B�ffB���B�p�B�=qB���B�G�B�B�(�B���B�B�  B�ffB��HB�B�=qB�RB�33B�B�z�B�
=B陚B��B�z�B��B��
B�ffB���B�\)B��B�RB�G�B�B�=qB���B�B�(�B��HB�G�B�B�Q�B��B�B�Q�B��RB�33B�{B��RB�
=B��B�ffB�
=B��B�{B��RB�\)B�{B��\B��B��
C G�C �C ��C33C�\C�HC
=CffCC�Cz�C��C�C\)C�C�C(�Cz�C�HC33CffC�C�Cp�C��C��CQ�C�RC��C	G�C	�C
  C
Q�C
�\C
�HCQ�C�C
=C=qC�\C  C\)C�C�C=qC�RC{CG�C��C{CG�C��C{Cp�C�C
=Cp�C��C{Cz�C�
C(�Cp�C��C33Cz�C��CG�C��C�
C=qC��C�HC=qC�RC��CQ�CC�C\)C�RC(�Cp�CC33C��C��C33C��C�CG�C�RC{CQ�C�RC �C \)C �C!(�C!ffC!C"=qC"z�C"�
C#G�C#�C#�
C$Q�C$��C$�C%ffC%��C%��C&ffC&�C'  C'z�C'�RC({C(�C(C)(�C)�C)C*=qC*�\C*�
C+G�C+�C+�
C,Q�C,�\C,��C-ffC-��C.{C.ffC.��C/{C/\)C/��C0(�C0ffC0�
C1{C1ffC1�
C2{C2ffC2�HC3{C3�C3�HC4{C4�\C4�HC5�C5�\C5�
C6=qC6��C6�C7\)C7��C8{C8z�C8�C9(�C9p�C9�HC:33C:z�C:�C;=qC;�C;��C<33C<��C=
=C==qC=�RC=��C>Q�C>��C?
=C?z�C?C@(�C@��C@�HCA\)CA��CB  CBz�CBCC=qCC�CC��CDffCD��CE(�CEffCE��CF=qCF�CG
=CGG�CGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                              ?�  ?��H@:�H@z�H@�p�@��R@�G�A   A  A   A+�A@��A`  A�  A�Q�A���A�Q�A�  A�  A�Q�A�Q�B Q�B�
B�
B  B�
B(  B0  B8  B?�
BH  BP(�BW�
B`(�BhQ�Bp(�BxQ�B�=qB�(�B�{B�  B�  B�{B�=qB�=qB�{B��
B�  B�{B�(�B�{B�{B��B��B�  B��B��B��B�  B��B��
B��
B��B�{B�(�B�{B�  B�  B��B��
C��C  C  C  C

=C
=C��C�C  C
=C
=C
=C
=C  C��C��C!��C$  C&  C'�C)�C,  C.
=C0  C2
=C4  C5��C8
=C:{C<  C=�C@  CA��CC��CE�CG�CI��CK�HCM�CO��CQ��CS��CV
=CX  CY��C[�C]��C`  Cb
=Cd
=Cf  Ch{Cj{Cl
=Cn  Co��Cq��Ct  Cv
=Cx�Cz{C|  C~  C�  C���C���C�  C���C���C���C�  C�C�
=C�  C���C���C���C���C���C�C���C���C�C�C���C�  C���C���C�  C�  C�  C�  C�  C���C�  C�  C�  C���C���C�C�C���C���C�C�  C�  C�C���C�  C�
=C�C�  C�  C���C���C���C���C���C�  C�  C�  C���C��C���C�  C���C���C�C�
=C�  C���C���C�  C�C�
=C���C���C�C�  C���C�  C�\C�
=C�  C���C�C�
=C���C��C�  C�C�  C���C���C���C�  C�C�  C���C�  C���C�  C�
=C�  C�  C�
=C�C�  C���C�  C�  C�  C�C�  C���C�  C�  C�C�\C�  C���C�C�  C���C�  C�
=C�  C���C���C���C�  C���D }qD �qDz�D�qD��D�D��DD��DD�D�D��D�D��D�qDz�D�qD	��D
�D
z�D
�qD�D�qDxRD�qD}qD  D��D�D� D�qD��D�D�D��D� D�D� D�D}qD�D� D�RD� D  D}qD  D� D  D� D  D� D�qD}qD�RDz�D�D�D�qD� D�D��D   D }qD!  D!z�D"�D"��D#D#� D$  D$}qD%  D%��D&  D&� D'�D'�D(D(� D)  D)��D*  D*xRD+  D+� D,  D,�=D-�D-}qD.  D.� D/D/}qD0  D0� D1  D1� D2  D2� D2�qD3� D3��D4}qD5D5��D5�qD6� D7�D7� D7�qD8��D8�qD9��D:D:}qD:��D;��D<�D<}qD<��D=��D=�qD>}qD?  D?��D@D@�DA�DAz�DB  DBz�DCDC��DDDD�DEDE}qDF  DF��DF��DG� DHDH��DI�DI��DJ�DJ}qDK  DK�DL  DL� DL�qDM}qDN  DN}qDN��DO�DP  DP}qDP�qDQz�DQ�qDR}qDSDSz�DS�qDT��DT��DUz�DU�qDV}qDV�qDWz�DW��DXz�DY  DY��DZDZ� DZ�qD[� D[�qD\}qD]�D]�D^  D^z�D^��D_� D`D`��D`�qDaz�Db  Db��Dc�Dc��Dd�Dd��De  De}qDf�Df�DgDg�Dh�Dhz�Dh�qDi}qDj�Dj��Dk  Dk��DlDl}qDl�qDm��Dm�qDn��Do�Do}qDo�qDp� Dp�qDq� DrDr}qDs�Ds}qDs�qDt��DuDu��Du�qDv}qDv�qDw� Dx  Dx� Dy  Dy��Dz  Dz��D{�D{� D|  D|� D}�D}�D~  D~��D  D� D�qD�AHD��HD���D��D�AHD�� D�� D���D�@ D�~�D��HD�  D�@ D�� D�D���D�@ D��HD���D�  D�AHD�� D�� D��D�>�D�|)D�� D�  D�AHD�� D��HD���D�@ D���D��HD�  D�AHD�� D���D�  D�>�D�~�D��qD�  D�>�D��HD�� D�HD�@ D�|)D�� D���D�>�D�~�D�� D�  D�>�D��HD���D�  D�>�D�~�D��HD�HD�>�D�~�D�� D���D�=qD�~�D�� D��qD�=qD�}qD���D�HD�@ D�~�D��HD��D�@ D�~�D�� D���D�>�D�� D�� D���D�>�D�� D��HD�HD�>�D���D��qD�HD�>�D�� D���D���D�B�D�~�D���D��D�AHD��HD��HD���D�>�D��HD���D���D�@ D�� D���D��D�AHD�}qD���D�HD�AHD��HD���D���D�B�D�~�D��qD�  D�>�D�� D��qD�  D�>�D�� D���D��qD�AHD��HD�D�  D�B�D��HD��qD�  D�@ D�� D�� D���D�=qD��HD���D�  D�B�D��HD�� D�HD�AHD��HD�D��qD�=qD��HD�� D��)D�>�D��HD��qD�  D�=qD��HD���D��D�AHD�~�D��HD��D�>�D�� D���D�  D�@ D�~�D���D�HD�AHD�� D�� D���D�>�D�}qD��HD�  D�AHD���D��HD�HD�AHD�~�D��HD��qD�@ D�� D���D�HD�>�D�}qD�� D��qD�@ D�}qD��qD�  D�>�D�}qD�� D�HD�>�D�~�D��HD�  D�>�D�~�D�� D�HD�@ D�� D��HD�HD�AHD�� D��HD�  D�=qD�~�D�� D�  D�@ D�}qD��qD�HD�C�D�� D���D���D�AHD���D���D��qD�>�D�~�D���D�HD�AHD�� D�� D�HD�B�D��HD�� D�  D�>�D�}qD���D���D�>�DHD�� D��qD�=qD�}qDýqD��qD�>�D�}qDľ�D���D�@ Dŀ D�� D�  D�=qD�|)Dƾ�D�HD�>�DǁHD�D�  D�=qD�~�D�� D�  D�@ DɁHD�D��D�B�Dʂ�D���D�HD�@ DˁHD��HD�  D�AHD̀ D�� D�  D�@ D̀ D��HD�HD�B�D΁HD�D�  D�=qD�~�DϾ�D�HD�@ D�|)Dо�D�  D�>�DсHD�� D���D�@ DҀ DҾ�D���D�=qD�~�D�� D�  D�@ D�~�D�� D���D�<)DՀ D�D��D�C�Dւ�D��HD���D�=qD�~�D׾�D��qD�@ D�~�Dؾ�D�  D�AHDق�D��HD�  D�@ DځHD�D�HD�>�Dۀ D۾�D��)D�=qD܀ D��HD��D�AHD�~�DݽqD���D�>�D�}qD޾�D��qD�@ D߂�D߾�D�HD�=qD�� D�D�  D�@ D� D��HD��qD�>�D� D�D��D�B�D�HD�� D�  D�AHD� D�� D���D�@ D�HD�� D�  D�B�D� D��HD�  D�>�D� D羸D���D�>�D� D�D�  D�@ D�HD�� D�  D�AHD�~�D�� D�  D�@ D� D�� D�  D�>�D� D��HD�  D�AHD� D���D�  D�AHDD��HD��qD�<)D�~�DﾸD���D�@ D�� D�D�HD�@ D� D�� D�  D�=qD� D��HD���D�@ D�}qD�D��qD�=qD�}qD���D�  D�>�D�~�D���D�  D�B�D�� D�D��D�@ D�}qD�� D�HD�>�D�}qD���D��D�@ D�� D�D��D�C�D�o\?��?#�
?L��?�  ?���?���?Ǯ?�G�@�\@\)@(�@(��@8Q�@G�@c�
@s33@�G�@�=q@���@�Q�@�G�@�=q@�33@��H@��
@˅@�33@�(�@��@���@�@��RAz�A��A{A�A
=A�A   A$z�A)��A.{A2�\A7
=A<(�A@��AEAI��AN�RAS33AXQ�A\(�AaG�Ac�
Ah��Amp�Aq�AvffA{�A�  A��HA���A�\)A�G�A��
A�{A�Q�A��HA��A�\)A��A�(�A�ffA��HA��A��A���A�(�A��RA�G�A�33A�p�A�\)A�G�A��A�A�  A\A���A�
=A�G�A˅A�{A�  Aҏ\A���A�
=Aٙ�A��
A�{A�Q�A�\A���A�
=A���A�A�A�Q�A��HA��A�\)A���A��
A�{B (�BG�BffB�Bz�B��B�RB�B��B	�B
=B  B�B=qB33BQ�Bp�B�\B�B��BB�HB  B��B{B33BQ�BG�BffB�B ��B!��B"�HB#�B$z�B%p�B&�RB'�B(��B)B+
=B,  B-��B.=qB/\)B0(�B1G�B2ffB3�B4z�B5��B6�RB8(�B9�B:=qB;\)B<Q�B=p�B>ffB?�B@z�BA��BB=qBC�BDQ�BEp�BF�\BG�BHz�BI��BJ�RBK�
BL��BMBN�RBP  BP��BR{BS\)BTz�BUp�BV�\BW�BXz�BYBZffB[\)B\Q�B]G�B^ffB_\)B`z�Bap�Bb�\Bc�
Bd��Be�Bg
=Bh  BiG�Bj=qBk
=Bk�
Bl��Bm�Bn�HBp  Bp��Bq�Bs33Btz�Bu��Bv�RBw�Bx��ByBz�HB|  B|��B}��B~�RB�B�ffB��HB�p�B��B��\B�33B�B�=qB��HB�p�B��
B�=qB���B�33B�B�Q�B��RB�\)B�B�z�B��B���B�(�B��RB��B��B�  B��\B��B���B�=qB���B�p�B�  B�z�B�
=B���B�  B�ffB���B�p�B�  B�z�B��B��
B�Q�B��HB�\)B�  B�z�B��HB�\)B��B�ffB�
=B�\)B�  B��RB�G�B�B�Q�B��HB�\)B��
B�(�B��RB�G�B�B�=qB���B�G�B�  B��\B�
=B���B�{B�ffB��HB�p�B��
B�ffB��HB��B�{B��\B��B��B��
B�ffB���B�G�B��
B�z�B���B�p�B��B�ffB��HB�G�B���B�{B��\B�
=B�B�=qB���B��B���B��B�Q�B���B�G�B��
B�Q�B��HB�33B��B��B�ffB���B��B�  B�ffB���B�\)B��B�{B���B�
=B��B�(�B£�B�33BÙ�B�(�Bď\B��HB�\)B�B�ffB���B�p�B�  B�z�BȸRB�33BɮB�(�B��HB�\)B��B�=qB̸RB��BͮB�Q�B���B�p�Bϙ�B�(�BЏ\B�33B�B�=qBҏ\B���BӅB�  Bԣ�B�33BՅB�{B�ffB�33B�B�Q�Bأ�B��Bٙ�B�Q�B��HB�p�B�B�Q�B���B݅B�(�B�ffB���B�p�B�=qB���B�G�B�B�(�B���B�B�  B�ffB��HB�B�=qB�RB�33B�B�z�B�
=B陚B��B�z�B��B��
B�ffB���B�\)B��B�RB�G�B�B�=qB���B�B�(�B��HB�G�B�B�Q�B��B�B�Q�B��RB�33B�{B��RB�
=B��B�ffB�
=B��B�{B��RB�\)B�{B��\B��B��
C G�C �C ��C33C�\C�HC
=CffCC�Cz�C��C�C\)C�C�C(�Cz�C�HC33CffC�C�Cp�C��C��CQ�C�RC��C	G�C	�C
  C
Q�C
�\C
�HCQ�C�C
=C=qC�\C  C\)C�C�C=qC�RC{CG�C��C{CG�C��C{Cp�C�C
=Cp�C��C{Cz�C�
C(�Cp�C��C33Cz�C��CG�C��C�
C=qC��C�HC=qC�RC��CQ�CC�C\)C�RC(�Cp�CC33C��C��C33C��C�CG�C�RC{CQ�C�RC �C \)C �C!(�C!ffC!C"=qC"z�C"�
C#G�C#�C#�
C$Q�C$��C$�C%ffC%��C%��C&ffC&�C'  C'z�C'�RC({C(�C(C)(�C)�C)C*=qC*�\C*�
C+G�C+�C+�
C,Q�C,�\C,��C-ffC-��C.{C.ffC.��C/{C/\)C/��C0(�C0ffC0�
C1{C1ffC1�
C2{C2ffC2�HC3{C3�C3�HC4{C4�\C4�HC5�C5�\C5�
C6=qC6��C6�C7\)C7��C8{C8z�C8�C9(�C9p�C9�HC:33C:z�C:�C;=qC;�C;��C<33C<��C=
=C==qC=�RC=��C>Q�C>��C?
=C?z�C?C@(�C@��C@�HCA\)CA��CB  CBz�CBCC=qCC�CC��CDffCD��CE(�CEffCE��CF=qCF�CG
=CGG�CGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�C�A�I�A�Q�A�VA�VA�VA�VA�VA�VA�XA�ZA�ZA�XA�XA�ZA�ZA�\)A�\)A�\)A�^5A�\)A�\)A�VA�S�A�VA�S�A�S�A�S�A�Q�A�K�A�;dA�/A�{A��AѺ^AѰ!Aѩ�Aѥ�Aѝ�Aѧ�AѲ-A�ƨA�ȴAѾwAѰ!AѓuA�t�A�XA�bAЅA�A�G�A�bA�33A�XAʍPA��yA�dZA�ƨA��A��#A���A���A���AǛ�A��TA���AƟ�A�bNA�hsA��`A�-A�C�A�p�A�bA��FA�XA��#A��+A��A�dZA���A�33A��RA�(�A���A�ffA�(�A�^5A��A��A���A��uA�7LA�C�A��A�$�A���A��TA���A�t�A��PA��;A��+A���A��A�+A��A��+A��TA��#A
=Ay��Av-At^5As
=Aqx�Am��AgAf �Ac33Aa�Aa&�A^�`A\ĜAX��AWG�AV��AV1'AT��AT(�AQ|�AN�ALVAJ��AH��ADȴAB��A@ZA<r�A;"�A8ZA6  A49XA3K�A1x�A/��A.A�A-%A,z�A+��A*�\A'�mA&�A%�A%�A$bNA#dZA"z�A!��A"�A"��A"�9A"~�A"=qA"{A!��A v�A��A��AO�A��A�\A�#A33AZA�Av�AK�AjAr�A`BA��AA�#A�AhsAVA�A��AƨA&�A�A	�TA�A��A�A�/A��AA�^A\)A33A�A+A=qA9XA=qAK�A�A��AA�A�;A��A�A"�A ��A ĜA ��A ��A n�@�l�@���@���@��9@�l�@���@�-@��7@�&�@���@�A�@�l�@��H@�v�@�n�@���@��@��T@�p�@�x�@�@�O�@���@�1@��@�V@�r�@�"�@�^5@��@�V@��@�@�"�@�V@��@�M�@�ff@���@�x�@�%@���@�I�@�ƨ@�l�@��@�bN@�Q�@�Q�@�9X@�1@��;@�;d@�-@��#@��@�"�@�I�@��
@�K�@�S�@�"�@�v�@���@ա�@Ԭ@�b@ӕ�@�K�@�"�@�@ҧ�@�M�@�=q@ҧ�@�E�@�@Ѳ-@�x�@��@��`@� �@Ͼw@�S�@�E�@͡�@�7L@�1@��@ʸR@�n�@�{@��T@���@ɑh@�&�@�I�@�1@��m@ǥ�@Ɵ�@�^5@�5?@�-@��@���@ŉ7@�`B@�O�@�V@Ĭ@��;@§�@�E�@��@�7L@��@���@�z�@�  @�K�@�"�@�@�@��y@�^5@��^@���@��@�x�@�x�@�hs@�`B@�O�@�G�@�%@�j@�b@��w@�o@��@�x�@�/@���@�j@� �@��@��F@�S�@�@��+@��7@��`@���@��u@��u@��u@�j@�I�@��@��P@�33@��R@�v�@�V@�-@�J@��#@�X@�&�@��/@���@�j@���@�C�@��y@���@���@�=q@��T@���@�x�@�`B@�7L@�%@�z�@���@�K�@��@��@�n�@�M�@�-@���@���@�x�@�p�@�`B@�?}@�/@��@���@�Q�@��@��
@��F@���@�K�@�"�@��R@�n�@�=q@���@��h@���@��D@�A�@�1@���@���@���@�~�@�5?@��T@�?}@��@�1@�\)@��\@�-@���@���@��@�`B@�&�@��/@��j@��@�A�@� �@��;@��
@��w@�l�@�
=@��@���@�^5@�V@�=q@�-@��@���@���@��`@���@�I�@��P@�K�@�
=@���@�V@��@��#@��7@�O�@�&�@��9@��@�Z@�(�@�ƨ@���@�K�@��H@��\@�v�@�n�@�E�@���@���@�&�@���@��u@�I�@�b@��@�|�@�;d@��y@�ȴ@���@���@�v�@�E�@�{@��@��-@�&�@��/@�Ĝ@��9@�Z@�(�@��P@�S�@���@��@���@��-@�x�@�p�@�?}@�7L@�7L@�V@���@���@�Z@�A�@�1'@�  @��
@���@�t�@�
=@��\@��+@��+@�v�@�^5@�-@��@��T@��-@�p�@�7L@�%@��9@���@��u@�z�@�Q�@�9X@���@��@��m@��
@��F@��P@�|�@�S�@�+@��@�ȴ@���@��+@�^5@�{@��@��#@��^@���@���@��7@�/@���@�r�@�Z@�1'@�1@l�@~v�@}��@}?}@|��@|��@|�@|j@|�@|1@{��@{t�@{"�@{o@z�@z��@z=q@y�7@xĜ@x��@x �@w�w@w;d@v��@u��@uO�@t�@tj@t1@s��@so@rn�@rM�@q�#@qG�@pr�@pb@o�;@o�@o�@n�+@nV@n5?@n@m�-@m�h@mp�@m�@l�j@lz�@l(�@k�
@k�F@k�@k33@k@jn�@j^5@jM�@jM�@ihs@h��@h1'@g�@g�@gl�@gK�@g;d@g+@f�y@fV@e�@e�@d�@c�m@cdZ@c33@c@b��@b�\@a�@`��@`r�@`bN@`Q�@`b@_�;@_|�@^�y@^{@]p�@\�@\�j@\�D@\�@[ƨ@[t�@[33@Y�@Yhs@Y&�@Y%@XĜ@X�@X  @W�P@W�@V��@U��@U�@U`B@UO�@T�@T��@T�@Tj@T�@S�
@SS�@R��@R�!@R�\@Rn�@Q�@Qhs@P�u@O�@Ol�@N��@NV@M�@M��@M�-@M�@L��@K��@K��@J~�@I��@IG�@I7L@IG�@I7L@Hr�@H �@G�@G|�@F�R@E��@E@E��@EO�@E�@D��@D9X@C�
@Ct�@C33@B��@B�\@Bn�@B-@BJ@A��@A�@A��@Ahs@A%@@��@@bN@@b@?�;@?�P@?\)@?;d@?
=@>�+@=�@<z�@<9X@;�
@;��@;C�@:�@:n�@9�@9�#@9�^@9�7@9X@9&�@9%@8��@8A�@8  @7�@7+@6�@6��@65?@5��@5��@5?}@4j@3�@333@2��@2=q@2J@2J@1��@1��@1��@17L@0��@0r�@/�;@/�@/;d@/�@.��@.�y@.v�@.@-��@-�-@-��@-�@-?}@-V@,�@,9X@,�@,1@+�
@+�F@+�@+C�@+@*�H@*��@*n�@)�@)�^@(��@(b@'�w@'�P@'l�@';d@'�@&�@&�R@&E�@&5?@&$�@&@%�T@%�-@%`B@%/@$��@$��@$Z@#ƨ@#S�@"�@"�H@"�H@"��@"�\@"^5@"J@!�@!�^@!hs@!&�@!%@ ��@ ��@ Ĝ@ �@ Q�@�@�@��@\)@�@��@ȴ@�+@$�@��@O�@�@��@�/@��@�@�D@j@I�@�@1@�m@t�@dZ@o@�@��@��@-@��@%@��@�@Q�@A�@1'@ �@�@�P@l�@K�@+@�@
=@�y@�@ȴ@��@��@V@�@�T@��@�@�@?}@V@�/@�D@Z@1@�F@��@t�@dZ@o@�H@-@�@x�@7L@7L@7L@�@%@��@��@��@Ĝ@Ĝ@�u@bN@bN@bN@1'@��@�P@\)@
=@ȴ@��@5?@O�@/@�@�@�j@z�@I�@�m@��@33@
��@
��@
��@
��@
�\@
n�@
=q@
J@	�@	��@	�^@	x�@	X@	G�@	&�@�`@�9@r�@1'@b@�;@|�@l�@\)@\)@\)@\)@�@�@
=A�1'A�1'A�;dA�9XA�5?A�;dA�A�A�;dA�A�A�C�A�C�A�K�A�K�A�K�A�K�A�M�A�Q�A�S�A�Q�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�VA�S�A�S�A�S�A�Q�A�S�A�Q�A�Q�A�S�A�Q�A�S�A�S�A�S�A�VA�VA�VA�XA�VA�VA�VA�XA�VA�XA�XA�ZA�ZA�XA�ZA�ZA�ZA�ZA�XA�XA�XA�VA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�XA�XA�XA�VA�XA�XA�XA�XA�XA�ZA�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�`BA�^5A�^5A�^5A�\)A�\)A�^5A�^5A�`BA�`BA�`BA�^5A�`BA�`BA�`BA�`BA�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�ZA�VA�ZA�ZA�ZA�^5A�^5A�ZA�VA�\)A�ZA�ZA�S�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�Q�A�S�A�S�A�S�A�Q�A�Q�A�Q�A�S�A�S�A�VA�VA�VA�VA�XA�XA�VA�VA�Q�A�VA�VA�VA�VA�XA�XA�VA�S�A�S�A�S�A�Q�A�S�A�Q�A�O�A�Q�A�Q�A�O�A�O�A�M�A�O�A�M�A�O�A�M�A�O�A�O�A�M�A�K�A�I�A�M�A�G�A�G�A�;dA�;dA�5?A�7LA�33A�33A�1'A�1'A�33A�/A�+A�"�A�&�A�+A��A��A��A�oA�VA�JA�1A�A��A���A��mA��mA��A��;A���A�ĜA�A���AѾwAѶFAѶFAѴ9AѴ9AѲ-AѰ!AѰ!AѰ!AѮAѮAѬAѮAѮAѮAѩ�Aѩ�Aѩ�Aѩ�Aѩ�Aѧ�Aѥ�Aѣ�Aѣ�Aѣ�Aѣ�Aѣ�Aџ�Aѝ�Aѝ�Aћ�Aѝ�Aѝ�Aџ�Aѥ�Aѣ�Aѡ�Aѣ�Aѥ�Aѧ�Aѩ�AѬAѰ!AѲ-AѲ-AѲ-AѲ-AѲ-AѶFAѺ^A���A�A�ĜA�ƨA�ȴA�ȴA�ȴA���A���A���A�ȴA�ƨA�ĜA�ĜA�ȴA���A���A�ƨAѾwAѸRAѸRAѶFAѴ9AѲ-AѰ!AѰ!AѰ!AѮAѩ�Aѣ�Aџ�Aѝ�Aћ�Aѕ�AёhAя\Aщ7AуA�~�A�r�A�p�A�n�A�t�A�t�A�r�A�p�A�n�A�jA�`BA�XA�K�A�K�A�I�A�C�A�A�A�A�A�9XA��A�  A��A��`A��#A���A�ĜAа!AЕ�A�n�A�hsA�ZA�G�A�A�A�=qA�1'A� �A�JA��A��HA�ƨAϸRAϩ�Aϙ�A�hsA�O�A�=qA�5?A��A���AήA΁A�?}A��A��A��A���A�A͸RAͮAͧ�A͏\A�x�A�Q�A���A��
A̩�A̡�A̕�Ȁ\Ả7Ȧ+Ȧ+A�~�A��mA�I�A���A��AʾwA�~�A�^5A�G�A�5?A��A���A��A��A��A��A��mA��;A���A���Aɰ!Aɛ�A�~�A�^5A�K�A��A�  A���A��A��TA�ƨA���AȼjAȝ�AȃA�\)A�5?A� �A��A�VA���A��mA��mA��`A��TA��#A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���AǸRAǰ!AǬAǥ�AǛ�A�hsA��A���A��A��;A�ƨA�A���A���A��/A��;A���A�ƨA�ȴA���A���Aư!Aƥ�AƑhAƉ7AƇ+AƃAƃAƃAƃA�jA�5?A��mA�Aš�A�p�A�G�A�+A�$�A�"�A�$�A�$�A��mA�ĜAė�AāA�jA�G�A��A��A��A�VA��/A×�A�bNA�XA�+A¾wA¥�AhAPA+A�bNA�\)A��A���A�ffA�=qA�{A�A��A��`A���A��jA���A��A�r�A�hsA�(�A�&�A�ZA�/A�JA���A���A��HA�ȴA���A�S�A��9A�z�A�XA�Q�A�S�A�+A�bNA��HA�(�A�ȴA���A���A��uA��DA�x�A�(�A���A�(�A�VA��uA��#A��A�9XA��mA�jA��A�n�A�VA�E�A�1A��A�l�A�XA�O�A�?}A�5?A�$�A� �A�1A��HA��^A�A�oA��9A�r�A�"�A��RA�dZA�1A��A�ȴA���A��A�S�A���A�z�A��A�`BA�p�A��A��uA��
A�hsA��A���A�VA���A��A�9XA��yA���A��A���A�ƨA�ȴA���A��wA���A�M�A��;A�l�A��mA�33A��TA��RA���A�|�A�9XA�oA�VA�A��A���A�/A��A��A��!A�E�A�5?A���A���A��9A�p�A��-A�\)A��mA�ȴA���A�ĜA��wA��RA��!A���A�n�A�E�A�bA��#A��FA��-A��A��hA�A�A���A�z�A�A�A���A�v�A�+A�jA�&�A�jA��mA��FA�p�A�?}A�=qA�5?A��A��mA��jA�t�A�1'A�  A���A�n�A��9A�K�A�&�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                              A�9XA�C�A�I�A�Q�A�VA�VA�VA�VA�VA�VA�XA�ZA�ZA�XA�XA�ZA�ZA�\)A�\)A�\)A�^5A�\)A�\)A�VA�S�A�VA�S�A�S�A�S�A�Q�A�K�A�;dA�/A�{A��AѺ^AѰ!Aѩ�Aѥ�Aѝ�Aѧ�AѲ-A�ƨA�ȴAѾwAѰ!AѓuA�t�A�XA�bAЅA�A�G�A�bA�33A�XAʍPA��yA�dZA�ƨA��A��#A���A���A���AǛ�A��TA���AƟ�A�bNA�hsA��`A�-A�C�A�p�A�bA��FA�XA��#A��+A��A�dZA���A�33A��RA�(�A���A�ffA�(�A�^5A��A��A���A��uA�7LA�C�A��A�$�A���A��TA���A�t�A��PA��;A��+A���A��A�+A��A��+A��TA��#A
=Ay��Av-At^5As
=Aqx�Am��AgAf �Ac33Aa�Aa&�A^�`A\ĜAX��AWG�AV��AV1'AT��AT(�AQ|�AN�ALVAJ��AH��ADȴAB��A@ZA<r�A;"�A8ZA6  A49XA3K�A1x�A/��A.A�A-%A,z�A+��A*�\A'�mA&�A%�A%�A$bNA#dZA"z�A!��A"�A"��A"�9A"~�A"=qA"{A!��A v�A��A��AO�A��A�\A�#A33AZA�Av�AK�AjAr�A`BA��AA�#A�AhsAVA�A��AƨA&�A�A	�TA�A��A�A�/A��AA�^A\)A33A�A+A=qA9XA=qAK�A�A��AA�A�;A��A�A"�A ��A ĜA ��A ��A n�@�l�@���@���@��9@�l�@���@�-@��7@�&�@���@�A�@�l�@��H@�v�@�n�@���@��@��T@�p�@�x�@�@�O�@���@�1@��@�V@�r�@�"�@�^5@��@�V@��@�@�"�@�V@��@�M�@�ff@���@�x�@�%@���@�I�@�ƨ@�l�@��@�bN@�Q�@�Q�@�9X@�1@��;@�;d@�-@��#@��@�"�@�I�@��
@�K�@�S�@�"�@�v�@���@ա�@Ԭ@�b@ӕ�@�K�@�"�@�@ҧ�@�M�@�=q@ҧ�@�E�@�@Ѳ-@�x�@��@��`@� �@Ͼw@�S�@�E�@͡�@�7L@�1@��@ʸR@�n�@�{@��T@���@ɑh@�&�@�I�@�1@��m@ǥ�@Ɵ�@�^5@�5?@�-@��@���@ŉ7@�`B@�O�@�V@Ĭ@��;@§�@�E�@��@�7L@��@���@�z�@�  @�K�@�"�@�@�@��y@�^5@��^@���@��@�x�@�x�@�hs@�`B@�O�@�G�@�%@�j@�b@��w@�o@��@�x�@�/@���@�j@� �@��@��F@�S�@�@��+@��7@��`@���@��u@��u@��u@�j@�I�@��@��P@�33@��R@�v�@�V@�-@�J@��#@�X@�&�@��/@���@�j@���@�C�@��y@���@���@�=q@��T@���@�x�@�`B@�7L@�%@�z�@���@�K�@��@��@�n�@�M�@�-@���@���@�x�@�p�@�`B@�?}@�/@��@���@�Q�@��@��
@��F@���@�K�@�"�@��R@�n�@�=q@���@��h@���@��D@�A�@�1@���@���@���@�~�@�5?@��T@�?}@��@�1@�\)@��\@�-@���@���@��@�`B@�&�@��/@��j@��@�A�@� �@��;@��
@��w@�l�@�
=@��@���@�^5@�V@�=q@�-@��@���@���@��`@���@�I�@��P@�K�@�
=@���@�V@��@��#@��7@�O�@�&�@��9@��@�Z@�(�@�ƨ@���@�K�@��H@��\@�v�@�n�@�E�@���@���@�&�@���@��u@�I�@�b@��@�|�@�;d@��y@�ȴ@���@���@�v�@�E�@�{@��@��-@�&�@��/@�Ĝ@��9@�Z@�(�@��P@�S�@���@��@���@��-@�x�@�p�@�?}@�7L@�7L@�V@���@���@�Z@�A�@�1'@�  @��
@���@�t�@�
=@��\@��+@��+@�v�@�^5@�-@��@��T@��-@�p�@�7L@�%@��9@���@��u@�z�@�Q�@�9X@���@��@��m@��
@��F@��P@�|�@�S�@�+@��@�ȴ@���@��+@�^5@�{@��@��#@��^@���@���@��7@�/@���@�r�@�Z@�1'@�1@l�@~v�@}��@}?}@|��@|��@|�@|j@|�@|1@{��@{t�@{"�@{o@z�@z��@z=q@y�7@xĜ@x��@x �@w�w@w;d@v��@u��@uO�@t�@tj@t1@s��@so@rn�@rM�@q�#@qG�@pr�@pb@o�;@o�@o�@n�+@nV@n5?@n@m�-@m�h@mp�@m�@l�j@lz�@l(�@k�
@k�F@k�@k33@k@jn�@j^5@jM�@jM�@ihs@h��@h1'@g�@g�@gl�@gK�@g;d@g+@f�y@fV@e�@e�@d�@c�m@cdZ@c33@c@b��@b�\@a�@`��@`r�@`bN@`Q�@`b@_�;@_|�@^�y@^{@]p�@\�@\�j@\�D@\�@[ƨ@[t�@[33@Y�@Yhs@Y&�@Y%@XĜ@X�@X  @W�P@W�@V��@U��@U�@U`B@UO�@T�@T��@T�@Tj@T�@S�
@SS�@R��@R�!@R�\@Rn�@Q�@Qhs@P�u@O�@Ol�@N��@NV@M�@M��@M�-@M�@L��@K��@K��@J~�@I��@IG�@I7L@IG�@I7L@Hr�@H �@G�@G|�@F�R@E��@E@E��@EO�@E�@D��@D9X@C�
@Ct�@C33@B��@B�\@Bn�@B-@BJ@A��@A�@A��@Ahs@A%@@��@@bN@@b@?�;@?�P@?\)@?;d@?
=@>�+@=�@<z�@<9X@;�
@;��@;C�@:�@:n�@9�@9�#@9�^@9�7@9X@9&�@9%@8��@8A�@8  @7�@7+@6�@6��@65?@5��@5��@5?}@4j@3�@333@2��@2=q@2J@2J@1��@1��@1��@17L@0��@0r�@/�;@/�@/;d@/�@.��@.�y@.v�@.@-��@-�-@-��@-�@-?}@-V@,�@,9X@,�@,1@+�
@+�F@+�@+C�@+@*�H@*��@*n�@)�@)�^@(��@(b@'�w@'�P@'l�@';d@'�@&�@&�R@&E�@&5?@&$�@&@%�T@%�-@%`B@%/@$��@$��@$Z@#ƨ@#S�@"�@"�H@"�H@"��@"�\@"^5@"J@!�@!�^@!hs@!&�@!%@ ��@ ��@ Ĝ@ �@ Q�@�@�@��@\)@�@��@ȴ@�+@$�@��@O�@�@��@�/@��@�@�D@j@I�@�@1@�m@t�@dZ@o@�@��@��@-@��@%@��@�@Q�@A�@1'@ �@�@�P@l�@K�@+@�@
=@�y@�@ȴ@��@��@V@�@�T@��@�@�@?}@V@�/@�D@Z@1@�F@��@t�@dZ@o@�H@-@�@x�@7L@7L@7L@�@%@��@��@��@Ĝ@Ĝ@�u@bN@bN@bN@1'@��@�P@\)@
=@ȴ@��@5?@O�@/@�@�@�j@z�@I�@�m@��@33@
��@
��@
��@
��@
�\@
n�@
=q@
J@	�@	��@	�^@	x�@	X@	G�@	&�@�`@�9@r�@1'@b@�;@|�@l�@\)@\)@\)@\)@�@�@
=A�1'A�1'A�;dA�9XA�5?A�;dA�A�A�;dA�A�A�C�A�C�A�K�A�K�A�K�A�K�A�M�A�Q�A�S�A�Q�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�VA�S�A�S�A�S�A�Q�A�S�A�Q�A�Q�A�S�A�Q�A�S�A�S�A�S�A�VA�VA�VA�XA�VA�VA�VA�XA�VA�XA�XA�ZA�ZA�XA�ZA�ZA�ZA�ZA�XA�XA�XA�VA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�XA�XA�XA�VA�XA�XA�XA�XA�XA�ZA�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�`BA�^5A�^5A�^5A�\)A�\)A�^5A�^5A�`BA�`BA�`BA�^5A�`BA�`BA�`BA�`BA�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�ZA�VA�ZA�ZA�ZA�^5A�^5A�ZA�VA�\)A�ZA�ZA�S�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�Q�A�S�A�S�A�S�A�Q�A�Q�A�Q�A�S�A�S�A�VA�VA�VA�VA�XA�XA�VA�VA�Q�A�VA�VA�VA�VA�XA�XA�VA�S�A�S�A�S�A�Q�A�S�A�Q�A�O�A�Q�A�Q�A�O�A�O�A�M�A�O�A�M�A�O�A�M�A�O�A�O�A�M�A�K�A�I�A�M�A�G�A�G�A�;dA�;dA�5?A�7LA�33A�33A�1'A�1'A�33A�/A�+A�"�A�&�A�+A��A��A��A�oA�VA�JA�1A�A��A���A��mA��mA��A��;A���A�ĜA�A���AѾwAѶFAѶFAѴ9AѴ9AѲ-AѰ!AѰ!AѰ!AѮAѮAѬAѮAѮAѮAѩ�Aѩ�Aѩ�Aѩ�Aѩ�Aѧ�Aѥ�Aѣ�Aѣ�Aѣ�Aѣ�Aѣ�Aџ�Aѝ�Aѝ�Aћ�Aѝ�Aѝ�Aџ�Aѥ�Aѣ�Aѡ�Aѣ�Aѥ�Aѧ�Aѩ�AѬAѰ!AѲ-AѲ-AѲ-AѲ-AѲ-AѶFAѺ^A���A�A�ĜA�ƨA�ȴA�ȴA�ȴA���A���A���A�ȴA�ƨA�ĜA�ĜA�ȴA���A���A�ƨAѾwAѸRAѸRAѶFAѴ9AѲ-AѰ!AѰ!AѰ!AѮAѩ�Aѣ�Aџ�Aѝ�Aћ�Aѕ�AёhAя\Aщ7AуA�~�A�r�A�p�A�n�A�t�A�t�A�r�A�p�A�n�A�jA�`BA�XA�K�A�K�A�I�A�C�A�A�A�A�A�9XA��A�  A��A��`A��#A���A�ĜAа!AЕ�A�n�A�hsA�ZA�G�A�A�A�=qA�1'A� �A�JA��A��HA�ƨAϸRAϩ�Aϙ�A�hsA�O�A�=qA�5?A��A���AήA΁A�?}A��A��A��A���A�A͸RAͮAͧ�A͏\A�x�A�Q�A���A��
A̩�A̡�A̕�Ȁ\Ả7Ȧ+Ȧ+A�~�A��mA�I�A���A��AʾwA�~�A�^5A�G�A�5?A��A���A��A��A��A��A��mA��;A���A���Aɰ!Aɛ�A�~�A�^5A�K�A��A�  A���A��A��TA�ƨA���AȼjAȝ�AȃA�\)A�5?A� �A��A�VA���A��mA��mA��`A��TA��#A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���AǸRAǰ!AǬAǥ�AǛ�A�hsA��A���A��A��;A�ƨA�A���A���A��/A��;A���A�ƨA�ȴA���A���Aư!Aƥ�AƑhAƉ7AƇ+AƃAƃAƃAƃA�jA�5?A��mA�Aš�A�p�A�G�A�+A�$�A�"�A�$�A�$�A��mA�ĜAė�AāA�jA�G�A��A��A��A�VA��/A×�A�bNA�XA�+A¾wA¥�AhAPA+A�bNA�\)A��A���A�ffA�=qA�{A�A��A��`A���A��jA���A��A�r�A�hsA�(�A�&�A�ZA�/A�JA���A���A��HA�ȴA���A�S�A��9A�z�A�XA�Q�A�S�A�+A�bNA��HA�(�A�ȴA���A���A��uA��DA�x�A�(�A���A�(�A�VA��uA��#A��A�9XA��mA�jA��A�n�A�VA�E�A�1A��A�l�A�XA�O�A�?}A�5?A�$�A� �A�1A��HA��^A�A�oA��9A�r�A�"�A��RA�dZA�1A��A�ȴA���A��A�S�A���A�z�A��A�`BA�p�A��A��uA��
A�hsA��A���A�VA���A��A�9XA��yA���A��A���A�ƨA�ȴA���A��wA���A�M�A��;A�l�A��mA�33A��TA��RA���A�|�A�9XA�oA�VA�A��A���A�/A��A��A��!A�E�A�5?A���A���A��9A�p�A��-A�\)A��mA�ȴA���A�ĜA��wA��RA��!A���A�n�A�E�A�bA��#A��FA��-A��A��hA�A�A���A�z�A�A�A���A�v�A�+A�jA�&�A�jA��mA��FA�p�A�?}A�=qA�5?A��A��mA��jA�t�A�1'A�  A���A�n�A��9A�K�A�&�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
�B
��B
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
��B
�PB
�PB
��B
�PB
�PB
�PB
��B
�PB
�PB
�PB
�PB
�PB
�PB
�B
�B
�B
�~B
�JB
�xB
��B
�	B
��B
�lB
�lB
�7B
��B
�oB
�eB
�6B
��B
�B
�B
��B
�dB
�<B
�dB
�RB
֡B
�B
��B\B*�BB�BGzBI�BF�BP}BY�B`Bg�Bx�B��B��B�B�aB��B
	B�B�B%B0�B(�B�]B�B�B,qB@OB	B B�B�rB�B�ABΥB��B��B��BjB��BcBuZBpB_�BWsBJXBC-B:^B,�B
�"B
�"B
�&B
��B
�wB
y>B
u%B
XB
?}B
)�B
"4B
VB	�B	�gB	�XB	�<B	��B	��B	��B	zxB	p;B	j�B	dZB	VmB	L�B	@�B	=B	;0B	6zB	0!B	*0B	VB	FB	B	�B��B�B��B�?BںB��B�BیBںB�HB�ZB�B�B��B�B�
B�yB�KB�B�B	 �B	+B	�B	�B	oB	2�B	A�B	GB	L0B	TaB	X�B	_�B	aHB	kB	o�B	u�B	|�B	�1B	��B	�7B	��B	�B	��B	}"B	�fB	��B	�uB	��B	v`B	m�B	jB	`vB	\�B	h
B	h�B	l"B	d�B	^5B	U2B	RTB	M�B	N<B	O�B	S�B	U�B	XEB	Z�B	d�B	o�B	oiB	tB	v`B	}�B	zxB	�rB	��B	�VB	��B	��B	�(B	��B	��B	��B	��B	�~B	��B	��B	�B	�@B	�hB	��B	��B	�B	��B	�oB	��B	�B	�FB	�B	��B	�YB	��B	�B	�B	�B	��B	��B	��B	�_B	��B	�$B	��B	�B	�uB	��B	��B	��B	�bB	��B	�!B	��B	��B	��B	�XB	�_B	��B	��B	��B	��B	�6B	��B	��B	��B	�:B	�@B	�$B	�*B	�kB	�B	�wB	�B	��B	�$B	��B	��B	��B	�_B	��B	�FB	�RB	�eB	�kB	��B	�B	�B	��B	�?B	�nB	�B	�$B	��B	ÖB	��B	ɆB	�^B	�B	��B	��B	�BB	�pB	�}B	уB	��B	��B	�TB	ҽB	ҽB	�TB	҉B	�TB	ԕB	��B	��B	�9B	�mB	�KB	�WB	��B	�B	�pB	�B	�B	�BB	�;B	�BB	ߤB	�B	�B	��B	��B	��B	�B	�sB	�B	�QB	�B	�WB	�B	�WB	�WB	�/B	�iB	��B	� B	� B	�B	��B	�B	��B	�/B	�5B	�B	�B	��B	�B	�B	�B	�B	�B	�%B	��B	�B	��B	��B	��B	��B	��B	��B	�>B	�>B	�	B	��B	�	B	�	B	�DB	��B	�JB	�PB	�PB	�PB	��B	��B	��B	�(B	�]B	��B	��B	��B
;B
�B
�B
B
�B
MB
�B
�B
�B
B
�B
SB
�B
	B
�B
fB
	B

�B

	B

�B
xB
DB
�B
�B
xB
B
xB
B
�B
~B
�B
PB
�B
B
PB
�B
\B
�B
(B
�B
VB
�B
\B
�B
�B
�B
hB
�B
B
FB
�B
SB
�B
�B
B
�B
�B
�B
�B
�B
B
SB
�B
SB
$B
�B
$B
YB
$B
�B
+B
�B
�B
�B
�B
�B
�B
eB
�B
�B
�B
kB
	B
qB
�B
B
~B
~B
OB
IB
B
�B
�B
�B
�B
B
!B
�B
�B
!B
�B
 \B
 �B
!�B
"�B
#nB
#:B
"hB
!�B
!�B
!�B
"hB
!�B
!�B
"�B
"�B
"�B
#:B
"�B
#:B
#:B
#nB
#nB
#�B
$�B
%FB
%�B
%zB
%B
&�B
%�B
'RB
&�B
)_B
)*B
)_B
)�B
*0B
)�B
*0B
*0B
)�B
*eB
+kB
+kB
+kB
,B
+�B
,B
,qB
,=B
,�B
-CB
.}B
.B
-�B
.IB
.IB
.IB
.�B
.�B
.}B
.�B
/B
/�B
/�B
0UB
/�B
0�B
0�B
1[B
1�B
1[B
1�B
1[B
1�B
1�B
1[B
1�B
1�B
1[B
1[B
1�B
1�B
1[B
2-B
2�B
1�B
1�B
2-B
2-B
2�B
3�B
3�B
3�B
2�B
3hB
2�B
3�B
4nB
4�B
5tB
4�B
5�B
5�B
5�B
6zB
6zB
6zB
6�B
7�B
7LB
7LB
7LB
7�B
7�B
7�B
7�B
9$B
8�B
8�B
9�B
9XB
:�B
:*B
:*B
:�B
:�B
:�B
:�B
:^B
:�B
<B
<jB
<�B
<�B
<�B
>�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@OB
A�B
A�B
A�B
B�B
B�B
CaB
CaB
D3B
C�B
C�B
CaB
E9B
E�B
FB
F?B
F?B
F�B
F�B
F?B
F�B
GB
G�B
HB
HKB
HB
I�B
IRB
I�B
J#B
I�B
J#B
K^B
K�B
K�B
L0B
K�B
L0B
LdB
L�B
MB
NB
NB
N<B
N�B
N<B
OB
M�B
NpB
N<B
P}B
PHB
P}B
PHB
P�B
PB
QNB
QNB
Q�B
R B
S&B
R�B
S[B
R�B
S&B
S�B
S&B
S�B
S[B
S�B
T�B
T�B
T�B
T�B
TaB
U�B
T�B
VmB
V�B
V�B
W�B
WsB
W�B
W�B
W?B
W�B
X�B
XEB
X�B
Z�B
ZQB
Z�B
Z�B
ZB
ZB
[�B
[#B
Z�B
[WB
]dB
]�B
\�B
]/B
^B
]dB
]dB
\�B
]dB
\�B
\�B
]/B
]dB
]�B
^jB
^jB
^jB
^�B
^�B
_B
_�B
`BB
`vB
`�B
`vB
`�B
`�B
`�B
aB
a�B
b�B
c�B
c�B
c�B
c�B
c�B
dZB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
f2B
f2B
f2B
e�B
e`B
e`B
e�B
e�B
e�B
e�B
ffB
e�B
e�B
ffB
f�B
gB
g8B
gmB
g�B
g�B
h>B
h
B
h�B
h�B
h�B
h�B
h�B
iB
iyB
i�B
jB
k�B
k�B
k�B
k�B
l"B
lWB
l�B
l�B
l�B
l�B
m)B
l�B
l�B
m�B
oiB
n�B
o B
oiB
o�B
p;B
pB
p�B
qB
p�B
qB
q�B
qB
qAB
qAB
qAB
qB
p�B
qAB
q�B
q�B
r�B
r|B
rGB
r|B
r�B
r�B
sMB
s�B
s�B
tTB
t�B
t�B
t�B
t�B
t�B
u%B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v`B
v�B
w2B
w�B
w�B
w�B
w�B
w�B
xB
xB
x8B
xlB
x�B
x�B
x�B
y>B
yrB
y>B
y�B
y>B
yrB
z�B
z�B
{�B
{B
|PB
{�B
|PB
|B
|PB
|PB
|�B
|�B
}"B
}"B
}VB
}VB
}�B
}VB
}VB
}�B
}�B
}�B
~]B
~(B
~�B
~�B
~�B
~�B
~�B
.B
�B
�B
� B
�4B
�iB
�iB
�iB
�B
�;B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�GB
�B
�GB
�GB
�{B
�{B
�GB
�GB
��B
�B
��B
�MB
��B
��B
��B
��B
��B
��B
�YB
��B
��B
�+B
�_B
��B
�1B
��B
�B
�7B
�lB
�7B
�lB
��B
�	B
�=B
�=B
�=B
�=B
��B
��B
�B
�DB
�xB
�xB
��B
�~B
�B
�~B
�B
�PB
��B
�B
�PB
��B
��B
��B
�VB
��B
�"B
��B
�VB
�.B
��B
�VB
��B
��B
��B
�"B
�~B
�B
��B
�B
��B
�VB
�VB
��B
�"B
��B
��B
��B
�VB
�VB
�"B
��B
��B
�"B
�"B
�"B
�VB
��B
��B
�"B
�"B
��B
��B
��B
�"B
�VB
�"B
�"B
�VB
�VB
��B
��B
�VB
��B
�VB
�VB
��B
�"B
��B
�"B
��B
�PB
�PB
�B
�PB
�"B
��B
��B
�PB
��B
�B
��B
�B
��B
��B
��B
��B
��B
�"B
�"B
��B
��B
�VB
�VB
�"B
�"B
�VB
��B
�B
�B
�B
�B
��B
��B
��B
��B
�~B
��B
��B
��B
�~B
��B
�~B
�~B
�~B
��B
�~B
��B
�~B
��B
�JB
��B
��B
�B
��B
�"B
��B
�"B
��B
��B
�VB
��B
��B
��B
�"B
��B
��B
�\B
��B
�JB
�B
��B
�\B
�PB
��B
��B
��B
��B
��B
�"B
�"B
��B
��B
�"B
��B
��B
�"B
��B
�"B
�"B
�"B
��B
��B
�B
�B
�JB
��B
�JB
�~B
�B
�B
��B
�~B
��B
�~B
��B
��B
��B
�~B
��B
�~B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�B
��B
��B
�JB
��B
�	B
��B
��B
��B
�B
�DB
�~B
��B
�xB
�xB
��B
��B
�DB
��B
��B
�B
�	B
�xB
��B
�7B
�B
�rB
��B
�lB
�rB
�B
��B
��B
�PB
�fB
�rB
��B
��B
�B
�lB
�1B
��B
�1B
��B
��B
��B
�B
��B
�lB
��B
�=B
�=B
��B
�	B
��B
��B
�fB
�fB
�fB
�1B
�1B
��B
�7B
��B
�	B
�	B
�=B
��B
��B
�B
�=B
�lB
��B
�=B
�=B
�~B
��B
��B
�hB
�B
�MB
��B
�B
�YB
��B
��B
�$B
��B
��B
��B
�-B
�*B
�0B
�B
�wB
�B
�qB
�qB
�qB
�=B
�qB
�B
��B
�B
��B
�UB
�aB
�aB
�3B
��B
�?B
�?B
�tB
�B
�zB
�zB
�LB
��B
��B
�RB
��B
�RB
��B
��B
��B
��B
�RB
�XB
��B
��B
�jB
�0B
�0B
��B
�B
�jB
��B
�<B
�qB
��B
�<B
�qB
��B
��B
�jB
�*B
��B
�$B
�B
��B
�^B
�B
�B
�BB
B
�gB
��B
ʌB
�^B
�jB
�vB
��B
�HB
ҽB
��B
��B
�B
�B
�/B
ܒB
ޞB
�BB
�DB
�B
�TB
�TB
�
B
�KB
�B
�B
�B
�B
�>B �B
��B
�VB
��B iB
��BB	lBVB{B�B 'B�B!�B#B#�B#�B#�B%�BAUBAUB?B<�BE�BD�BE�BA�BA�BB'BC�BE9BEmBGEBGzBI�BI�BJ�BMBJ#BK�BIRBG�BG�BI�BGBDgBC�BEmBGEBD�BF�BJ�BK�BP}BQ�BO�BOBBO�BP�BR�BS�BXBXBY�B[�BZ�B]�B[�B\�B^jB`B`B_Ba|Bd&Bd�Be�BdZBgBh�BiDBk�Bl�Bn�Bp�Bs�Bv�BzDB��B��B�(B�B��B��B�oB�B��B��B��B��B�B�_B��B�tB�tB�B��B��B�IB�UB��B��B��B��B��BרB�QB�#BܒB��B�+B �BBB�B	�B�BPB�B�B�B�B#nB+�B+�B)�B(�B!�B�B�B�BB~B'�BVB�B �B?}B.IB-CB-CB-�B4�B)�B>BB6�B?�BIB	B�BJBuB�B �B��B�`B�ZB�B��B�BhB
	BB
�B�B
�B\B�B(XB,qB(�B+B'�B3�B;0BpBTaB6FB%B�B�B�BeB_B%�BuB-�B"B�B�B iB��B�B��B�B�VB��B�B��B�B�%B��B��B��B�B��B�B�GB�B�B�B��B��B��BרB֡B�B��BB�3B��B��B�6B��B�B��B��B��B��B�YB��B��B{B��B�4BuZBk�Be`Bb�Bb�Bn/Bq�BqABu%B{�B{�B��B��B�AB��By�B�BsBq�Bp�Bz�B��BrGBp;BqABpoBwfB}"Be`BZ�B�B]dB]�BgBY�B\�B_�BwfBS�BO�BL0BJ#BJ�BIBJXBI�BH�BM�BJ�BGBB[BA�B:*B:�B?}B?B>�B9�B-CB%�B7�B%zB?}B($BkB
��B�B�B
�B
��B
�B
�QB
�B
�B
�B
�sB
�B
�B
�,B
��B
��B
ȴB
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                              B
��B
��B
��B
�'B
��B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
��B
�\B
�\B
��B
�\B
�\B
�\B
��B
�\B
�\B
�\B
�\B
�\B
�\B
�'B
�'B
�'B
��B
�VB
��B
��B
�B
��B
�xB
�xB
�CB
��B
�{B
�qB
�BB
��B
�B
�#B
��B
�pB
�HB
�pB
�^B
έB
ݡB
�BhB"�B:�B?�BA�B>�BH�BQ�BXB_�Bp�B��B��B�B�mB� BB�B�BB(�B �B�iB��B�B$}B8[BB	B�(B�~B�B�MBƱB��B��By�Bb"By�BwoBmfBhBW�BOBBdB;9B2jB$�B
�.B
�.B
�2B
��B
��B
qJB
m1B
PB
7�B
!�B
@B
bB	�B	�sB	�dB	�HB	��B	��B	}�B	r�B	hGB	b�B	\fB	NyB	D�B	8�B	5B	3<B	.�B	(-B	"<B	bB	RB	!B��B�BݡB�B�KB��B��B��BӘB��B�TB�fB۔B�B��BݡB�B�B�WB��B�(B��B�7B��B��B	
{B	*�B	9�B	?B	D<B	LmB	P�B	W�B	YTB	c(B	g�B	nB	t�B	�=B	��B	�CB	��B	�!B	|�B	u.B	�rB	��B	z�B	z�B	nlB	e�B	b"B	X�B	T�B	`B	`�B	d.B	\�B	VAB	M>B	J`B	E�B	FHB	G�B	K�B	M�B	PQB	R�B	]B	g�B	guB	l+B	nlB	u�B	r�B	�~B	��B	�bB	��B	��B	�4B	��B	��B	��B	��B	��B	��B	�B	�$B	�LB	�tB	��B	��B	�B	��B	�{B	��B	�$B	�RB	�$B	��B	�eB	��B	�B	�$B	�$B	��B	��B	��B	�kB	��B	�0B	��B	�$B	��B	��B	�B	��B	�nB	��B	�-B	��B	��B	��B	�dB	�kB	�B	��B	��B	��B	�BB	��B	��B	��B	�FB	�LB	�0B	�6B	�wB	� B	��B	�'B	��B	�0B	��B	��B	��B	�kB	��B	�RB	�^B	�qB	�wB	��B	�B	�B	��B	�KB	�zB	�B	�0B	��B	��B	��B	��B	�jB	�B	��B	��B	�NB	�|B	ȉB	ɏB	��B	��B	�`B	��B	��B	�`B	ʕB	�`B	̡B	��B	��B	�EB	�yB	�WB	�cB	�B	�B	�|B	ضB	�B	�NB	�GB	�NB	װB	ڎB	ܛB	�B	��B	��B	�B	�B	�B	�]B	�B	�cB	�B	�cB	�cB	�;B	�uB	��B	�B	�B	�B	��B	�B	��B	�;B	�AB	�B	�B	��B	�B	�B	��B	��B	��B	�1B	��B	�B	��B	��B	�B	��B	�B	�B	�JB	�JB	�B	��B	�B	�B	�PB	�B	�VB	�\B	�\B	�\B	��B	��B	��B	�4B	�iB	��B	�B	�B	�GB	��B	��B	�B	��B	�YB	��B	��B	��B	�+B	��B	�_B	��B
B
 �B
 rB
B
�B
B
�B
�B
PB
�B
�B
�B
!B
�B
!B
�B
�B
�B
\B
�B
'B
\B
�B
hB
�B
4B
�B
bB
�B
hB
B
�B
�B
	tB

�B
B
RB
�B
_B
�B
�B
$B
�B
�B
�B
�B
�B
*B
_B
�B
_B
0B
�B
0B
eB
0B
�B
7B
�B
�B
�B
�B
�B
�B
qB
�B
�B
�B
wB
B
}B
�B
!B
�B
�B
[B
UB
!B
�B
�B
�B
�B
'B
-B
�B
�B
-B
�B
hB
�B
�B
�B
zB
FB
tB
B
B
�B
tB
B
�B
�B
�B
�B
FB
�B
FB
FB
zB
zB
�B
�B
RB
�B
�B
B
�B
�B
^B
�B
!kB
!6B
!kB
!�B
"<B
!�B
"<B
"<B
!�B
"qB
#wB
#wB
#wB
$B
#�B
$B
$}B
$IB
$�B
%OB
&�B
& B
%�B
&UB
&UB
&UB
&�B
&�B
&�B
&�B
''B
'�B
'�B
(aB
'�B
(�B
(�B
)gB
*B
)gB
)�B
)gB
)�B
)�B
)gB
*B
)�B
)gB
)gB
*B
)�B
)gB
*9B
*�B
*B
*B
*9B
*9B
*�B
+�B
+�B
+�B
*�B
+tB
+B
+�B
,zB
,�B
-�B
,�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/�B
/XB
/XB
/XB
/�B
/�B
/�B
/�B
10B
0�B
0�B
1�B
1dB
2�B
26B
26B
2�B
2�B
3B
2�B
2jB
3B
4B
4vB
4�B
4�B
4�B
6�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8[B
9�B
9�B
9�B
;B
:�B
;mB
;mB
<?B
;�B
;�B
;mB
=EB
=�B
>B
>KB
>KB
>�B
>�B
>KB
>�B
?B
?�B
@#B
@WB
@#B
A�B
A^B
A�B
B/B
A�B
B/B
CjB
DB
C�B
D<B
DB
D<B
DpB
D�B
EB
FB
FB
FHB
F�B
FHB
GB
E�B
F|B
FHB
H�B
HTB
H�B
HTB
H�B
H B
IZB
IZB
I�B
J,B
K2B
J�B
KgB
J�B
K2B
K�B
K2B
LB
KgB
LB
L�B
L�B
L�B
M
B
LmB
M�B
L�B
NyB
N�B
N�B
O�B
OB
O�B
O�B
OKB
O�B
P�B
PQB
P�B
R�B
R]B
R�B
R�B
R)B
R)B
TB
S/B
R�B
ScB
UpB
U�B
UB
U;B
VB
UpB
UpB
UB
UpB
UB
T�B
U;B
UpB
U�B
VvB
VvB
VvB
V�B
V�B
WB
W�B
XNB
X�B
X�B
X�B
X�B
X�B
X�B
YB
Y�B
Z�B
[�B
[�B
[�B
[�B
[�B
\fB
]B
]B
\�B
\�B
\�B
]B
]B
]B
]�B
^
B
]�B
^
B
]�B
]�B
]�B
^>B
^>B
^>B
^>B
^
B
]lB
]lB
]�B
^
B
^
B
]�B
^rB
^
B
]�B
^rB
^�B
_B
_DB
_yB
_�B
_�B
`JB
`B
`�B
`�B
`�B
`�B
`�B
aB
a�B
a�B
b�B
c�B
c�B
c�B
c�B
d.B
dcB
d�B
e B
d�B
e B
e5B
e B
d�B
e�B
guB
f�B
gB
guB
g�B
hGB
hB
h�B
iB
h�B
iB
i�B
iB
iMB
iMB
iMB
iB
h�B
iMB
i�B
i�B
j�B
j�B
jSB
j�B
j�B
j�B
kYB
k�B
k�B
l`B
l�B
l�B
l�B
l�B
l�B
m1B
m1B
m�B
m�B
m�B
nB
nB
nB
n7B
nlB
n�B
o>B
o�B
o�B
o�B
o�B
o�B
pB
pB
pDB
pxB
p�B
p�B
p�B
qJB
q~B
qJB
q�B
qJB
q~B
r�B
r�B
s�B
s�B
t\B
s�B
t\B
t(B
t\B
t\B
t�B
t�B
u.B
u.B
ubB
ubB
u�B
ubB
ubB
u�B
u�B
v B
viB
v4B
v�B
v�B
v�B
v�B
wB
w:B
w�B
w�B
xB
x@B
xuB
xuB
xuB
yB
yGB
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{SB
{B
{SB
{SB
{�B
{�B
{SB
{SB
{�B
|%B
{�B
|YB
|�B
|�B
|�B
}�B
~�B
~�B
~eB
~�B
B
7B
kB
�B
�=B
��B
�B
�CB
�xB
�CB
�xB
��B
�B
�IB
�IB
�IB
�IB
��B
��B
�B
�PB
��B
��B
��B
��B
�!B
��B
�'B
�\B
��B
�'B
�\B
��B
��B
��B
�bB
��B
�.B
��B
�bB
�:B
��B
�bB
��B
��B
��B
�.B
��B
�'B
��B
�!B
��B
�bB
�bB
��B
�.B
��B
��B
��B
�bB
�bB
�.B
��B
��B
�.B
�.B
�.B
�bB
��B
��B
�.B
�.B
��B
��B
��B
�.B
�bB
�.B
�.B
�bB
�bB
��B
��B
�bB
��B
�bB
�bB
��B
�.B
��B
�.B
��B
�\B
�\B
�'B
�\B
�.B
��B
��B
�\B
��B
�'B
��B
�'B
��B
��B
��B
��B
��B
�.B
�.B
��B
��B
�bB
�bB
�.B
�.B
�bB
��B
�'B
�'B
�'B
�'B
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
��B
��B
��B
��B
��B
��B
��B
�VB
��B
��B
�'B
��B
�.B
��B
�.B
��B
��B
�bB
��B
��B
��B
�.B
��B
��B
�hB
��B
�VB
�'B
��B
�hB
�\B
��B
��B
��B
��B
��B
�.B
�.B
��B
��B
�.B
��B
��B
�.B
��B
�.B
�.B
�.B
��B
��B
�'B
�'B
�VB
��B
�VB
��B
�!B
�!B
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
�'B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�'B
��B
��B
�!B
��B
��B
�VB
��B
�B
��B
��B
��B
�!B
�PB
��B
��B
��B
��B
��B
��B
�PB
��B
��B
�B
�B
��B
��B
�CB
�B
�~B
��B
�xB
�~B
�B
��B
{�B
�\B
�rB
�~B
��B
��B
�B
�xB
�=B
��B
�=B
��B
��B
��B
�B
��B
�xB
��B
�IB
�IB
��B
�B
��B
��B
�rB
�rB
�rB
�=B
�=B
��B
�CB
��B
�B
�B
�IB
��B
��B
�B
�IB
�xB
��B
�IB
�IB
��B
��B
��B
�tB
�B
�YB
��B
�$B
�eB
��B
��B
�0B
�B
��B
��B
�9B
�6B
�<B
�B
��B
�B
�}B
�}B
�}B
�IB
�}B
�B
��B
� B
��B
�aB
�mB
�mB
�?B
��B
�KB
�KB
��B
�B
��B
��B
�XB
��B
��B
�^B
��B
�^B
��B
��B
��B
��B
�^B
�dB
��B
�B
�vB
�<B
�<B
��B
�B
�vB
��B
�HB
�}B
��B
�HB
�}B
��B
��B
�vB
�6B
�B
�0B
�B
��B
�jB
�B
�B
�NB
��B
�sB
�B
B
�jB
�vB
ǂB
��B
�TB
��B
�B
��B
�#B
�#B
�;B
ԞB
֪B
�NB
�PB
��B
�`B
�`B
�B
�WB
��B
�B
�B
�"B
�JB
��B
� B
�bB
��B
�uB
��B
�BxBbB�B�B3B�B�BB�B�B�B�B9aB9aB7 B4�B=�B<�B=�B9�B9�B:3B;�B=EB=yB?QB?�BA�BA�BCBEBB/BDBA^B?�B?�BA�B?B<sB;�B=yB?QB<�B>�BCBDBH�BI�BG�BGNBG�BH�BJ�BLBPBPBQ�BS�BR�BU�BS�BT�BVvBXBXBWBY�B\2B]B]�B\fB_B`�BaPBc�Bd�Bf�Bh�Bk�Bn�BrPBx�B��B�4B�B��B��B�{B�!B��B��B�B��B�B�kB��B��B��B�B��B��B�UB�aB��B��B��B�B�BϴB�]B�/BԞB��B�7B��B!B�B��B�B�B\B�B�B�B�BzB#�B#�B!�B �B�B�B�B�BB�B�BbB�BB7�B&UB%OB%OB%�B,�B!�B6NB.�B7�BA)BB�BVB��B��B��B�B�lB�fB�B�B�B	tBB!B�B �B�BhB�B dB$}B �B#B�B+�B3<BhBLmB.RBB�B�B�BqBkB�B�B%�B.B �B 	B�uB�B��B��B��B�bB��B��B�B	�B�1B��B��B�B�%B��B�B�SB�B�B��B�B��B�BϴBέB�&B�B��B�?B��B��B�BB��B�B��B��B��B��B�eB��B{�Bs�B��B�@BmfBc�B]lBZ�BZ�Bf;Bi�BiMBm1Bs�Bs�B��B��BzMB��Bq�B�Bk%Bi�Bh�Br�By�BjSBhGBiMBh{BorBu.B]lBR�BzBUpBU�B_BQ�BT�BW�BorBK�BG�BD<BB/BB�BA)BBdBA�B@�BE�BCB?B:gB9�B26B2�B7�B7 B6�B1�B%OB�B/�B�B7�B 0BwB
��B
��B
��B
�B
��B
�B
�]B
�B
�(B
�(B
�B
��B
ضB
�8B
��B
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230610120047                            20230610120047AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023061012004720230610120047  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023061012004720230610120047QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023061012004720230610120047QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               