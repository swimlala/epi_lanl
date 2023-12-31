CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:33:00Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230426223300  20230426223300  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @����h�@����h�11  @��-��@@��-��@@*9]N��@*9]N���c{���F�c{���F11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?�  @   @@  @�G�@�  @�  @�G�AG�A��A\)A+�A?\)A`  A�  A�  A��A�  A���A�Q�A�Q�A�Q�A�\)B�
BQ�B(�B   B(  B/�
B7�
B@  BH  BO�
BW�
B`(�Bh(�Bp  Bx  B�  B�{B�{B�  B��B�  B�{B��B�{B�{B��B�{B�  B�  B�(�B�  B�  B�{B�{B�  B�  B�  B��B��B�  B�{B�{B�{B�{B�  B��B��B��C��C�C�C��C
  C  C  C  C
=C  C  C  C��C
=C
=C   C!�C#�C%��C(
=C*
=C,  C.  C0  C1��C3��C5��C7�C9��C<
=C>  C@  CB  CC��CE��CH
=CJ
=CL  CN
=CP
=CR
=CT  CU��CW��CY��C[�C]�C_��Cb  Cd
=Cf{Ch{Cj  Cl  Cm��Co��Cr  Cs��Cv  Cx  Cz  C|
=C~
=C�C�  C�  C�  C�  C�  C���C���C���C�C�C�  C���C�  C�  C���C���C�C�C�C�C�C�C�  C�  C���C���C�C�C�C�C�C�C�  C�  C���C�  C�C�C�  C���C�  C�C�C�  C�C�  C���C�  C�C���C��C���C���C���C���C���C���C���C���C���C�  C�  C�  C�  C�C�  C���C�  C�  C���C���C�  C�  C�  C�  C���C���C�  C���C�  C�C�  C���C�  C�
=C�C�  C�  C���C���C���C���C�  C�C�C�
=C�  C���C���C�C�
=C�  C���C�  C�
=C�
=C���C���C�  C�
=C�
=C�  C�  C���C���C���C�  C�
=C�  C���C�C�  C���C�C�C���C���D   D � D  D� D  D� D  D� D  D� D�qD��D  D}qD�D��D�D� D��D	}qD
  D
��DD� D�qD� D�D}qD��D}qD�qD��D  Dz�D��Dz�D  D� D  D�D�D��D  D}qD  D��D�D� D  D� D�D��D  D��D�D�DD��D�qD}qD  D� D  D��D   D ��D!�D!� D"�D"� D#  D#� D$  D$� D%  D%}qD&  D&� D'  D'� D'�qD(}qD(�qD)� D*  D*� D+  D+� D,�D,� D,�qD-� D.  D.� D.�qD/� D0�D0� D1  D1��D2  D2}qD2�qD3}qD4  D4��D5  D5� D6  D6��D7D7��D7�qD8z�D9  D9��D:�D:}qD:��D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@��DA�DA� DA�qDB� DC  DC}qDD  DD��DD�qDE� DF  DF� DG�DG� DH  DH� DH�qDI� DJ  DJ}qDK  DK}qDK�qDL}qDL�qDMz�DN  DN}qDO  DO��DP  DP� DQ�DQ��DR�DR� DS  DS��DT�DT��DU  DUz�DU�qDV}qDW  DW� DX  DX��DY�DY��DZ  DZ� DZ�qD[� D\  D\}qD]  D]��D^�D^��D_D_��D`�D`� Da�Da�Db  Db}qDc  Dc}qDc�qDd� Dd�qDez�De�qDf� Dg  Dgz�Dg�qDh� Di  Di}qDi��Dj� Dk�Dk��Dl  Dl��Dm�Dm��Dm�qDn}qDo  Do� Dp  Dp� Dq�Dq� Dr  Dr� Dr�qDs}qDt  Dt� Dt�qDu}qDu�qDvz�Dv��Dw� Dw�qDx}qDy�Dy��Dz  Dz� D{�D{� D{�qD|z�D|�qD}� D~  D~}qD~�qD��D�HD�AHD�� D���D��qD�=qD�~�D�� D�  D�AHD��HD�� D���D�@ D�~�D���D�  D�@ D�� D��HD�HD�AHD�� D���D��qD�@ D��HD�� D���D�>�D�� D�� D�  D�@ D�� D��HD��D�AHD�� D��HD�  D�@ D�� D��qD���D�@ D�~�D�� D�  D�>�D�~�D��HD�  D�@ D�~�D�� D��D�AHD���D��HD�HD�AHD��HD��HD�HD�@ D�}qD���D�HD�AHD�� D��qD��qD�@ D��HD��HD�  D�@ D�~�D���D���D�AHD��HD���D�  D�@ D��HD�D�HD�@ D�~�D��qD���D�@ D�~�D�� D�HD�@ D�� D�� D���D�>�D�� D�D�HD�AHD��HD�D��D�B�D��HD��HD��D�@ D�|)D���D�HD�AHD���D�� D��qD�=qD�~�D���D���D�>�D�~�D�� D�  D�AHD��HD��HD�HD�AHD��HD�� D�  D�>�D�~�D���D��qD�@ D�� D��qD��qD�<)D�~�D��HD�HD�AHD��HD�� D�  D�>�D�~�D��qD��qD�>�D�~�D��qD�  D�B�D���D�D��D�AHD���D��HD�  D�@ D�~�D���D��qD�=qD�|)D��)D���D�AHD�� D���D�  D�AHD�� D�� D���D�=qD�|)D�� D�HD�AHD��HD��HD�HD�AHD�� D�� D���D�>�D�� D�� D�  D�>�D�}qD�� D�HD�>�D�~�D���D�  D�>�D�~�D���D���D�>�D�~�D��qD��qD�@ D���D�D��D�AHD�~�D�� D��D�B�D�� D��qD���D�AHD�� D�� D�  D�>�D�� D��HD��D�B�D���D��HD��D�B�D���D��HD�HD�AHD��HD��HD�  D�>�D�� D��HD�HD�@ D�� D���D���D�>�D�� D���D���D�>�D�~�D���D�  D�>�D D�� D���D�@ DÂ�D��HD�  D�@ D�~�DĽqD���D�@ D�~�Dž�D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�>�DȁHD��HD�  D�@ D�~�Dɾ�D�HD�AHD�~�DʽqD���D�>�D�~�D˾�D���D�@ D̂�D�D�  D�@ D́HD�� D�  D�>�D΁HD��HD�HD�@ Dπ DϾ�D�  D�AHDЁHD��HD�HD�@ DсHD��HD�  D�@ D҂�D�� D�HD�B�DӁHD�� D�  D�>�DԀ D�D�HD�AHDՁHD��HD���D�=qD�~�D־�D�  D�>�D�~�D�� D�HD�B�D؁HD�� D�  D�>�Dـ Dپ�D�  D�@ Dڀ D�� D�  D�@ D�}qD۾�D���D�>�D܀ Dܾ�D�  D�AHD�~�Dݾ�D�  D�AHDށHD�D�HD�>�D߁HD�� D���D�@ D�~�DྸD�  D�@ D�~�DᾸD�  D�AHD�HD��HD��qD�=qD� D�� D���D�@ D�~�D侸D�  D�@ D� D�� D�HD�AHD�HD澸D���D�B�D� D羸D���D�@ D�HD�D�  D�>�D�~�D龸D���D�@ D�HD�� D���D�@ D�HD��HD�  D�@ D�HD�� D���D�AHD�HD�� D�  D�@ D� D�� D���D�>�D� D�� D�HD�AHD��HD��HD�  D�>�D�~�D�� D�  D�>�D� D��HD���D�=qD�~�D�� D���D�>�D� D�D�HD�@ D�� D���D��qD�=qD�|)D���D�  D�>�D�~�D�� D��D�B�D��HD�� D���D�@ D��HD�� D���D�(�D�h�?�?\)?L��?��?���?�p�?�(�@�@\)@#�
@0��@E�@W
=@fff@xQ�@��@�\)@�
=@�G�@���@��@�(�@�ff@�{@�Q�@�  @���@�@��RA33A�Ap�A�\AffA�HA\)A$z�A)��A.{A1�A6ffA<(�A@��AE�AH��AN{AS33AXQ�A\(�A`��Ae�Aj�HAo\)As33Aw�A|(�A���A�33A�p�A�\)A���A�z�A�
=A���A�33A�A�Q�A��\A�z�A�
=A�G�A��
A�A�  A�=qA���A�
=A�G�A�33A�p�A��A�=qA���A�ffA�Q�A\A��A�
=A���A��HA�p�AϮA�=qA�(�A�{Aأ�A�33A�A�  A��A�(�A�RA�G�A��
A�A�  A�\A���A�\)A��A�(�A�{B (�Bp�B�RB  B��B{B33Bz�B	��B
=B  B��B{B\)B��BB�HB  B�BffB�B��B�B
=B  Bp�B�RB�
B ��B"{B#33B$(�B%p�B&�RB(  B)G�B*�\B+�B,��B-�B/
=B0Q�B1B3
=B4(�B5G�B6�\B8  B9G�B:�\B;�B<��B=�B?\)B@��BA�BC\)BDz�BEBF�HBHQ�BI��BJ�HBLQ�BM��BN�RBP  BQp�BR�RBT(�BU��BV�HBX(�BYp�BZ�HB\Q�B]B_33B`z�BaBc33Bd��Bf{Bg�Bh��Bj=qBk�Bm�Bn�\Bp(�Bq��Br�HBtQ�BuBw33Bx��BzffB{�B}�B~�\B�  B��RB��B�=qB���B��B�ffB��B��
B���B�p�B�(�B���B�p�B�(�B���B��B�ffB��B�B�z�B��B��
B��\B�\)B�{B���B�\)B�  B���B�G�B�  B��RB�\)B�{B���B�p�B�{B��RB�p�B�(�B���B��B�z�B�33B�{B��HB��B��\B�\)B�{B��HB��B�z�B�G�B�{B���B�B��\B�p�B�Q�B��B�  B���B��B�z�B�G�B�{B��HB��B�z�B�33B��B��RB�p�B�=qB�
=B��
B���B�p�B�Q�B�
=B��
B��\B�\)B�{B���B��B�(�B���B�p�B�  Bď\B�
=Bř�B�{B�z�B��HB�\)B�B�{B�z�B���B�33BɅB��
B�(�Bʏ\B���B�33B˅B��
B�(�B̏\B���B�p�B�B�(�B�z�B��HB�G�Bϙ�B��B�Q�BУ�B�
=B�G�BѮB�  B�ffB���B��BӅB��B�=qBԣ�B���B�\)BծB�(�B�z�B���B��Bי�B��B�=qB؏\B���B�\)BٮB�  B�Q�Bڣ�B�
=B�\)B�B�(�B܏\B��HB�33Bݙ�B�  B�ffB���B�33Bߙ�B�  B�ffB���B��B�B��
B�=qB��B���B�\)B�B�  B�ffB���B��B�p�B��
B�=qB��B���B�\)B�B�(�B�z�B��HB�33B陚B�  B�ffB�RB��B�p�B��
B�(�B��B�
=B�\)B��
B�=qB��B�
=B�p�B�B�(�B��\B���B�\)B��
B�(�B�\B���B�\)B�B�(�B�z�B��HB�G�B��B�{B�z�B���B�33B���B�  B�z�B���B�G�B��B�{B��\B���B�p�B��
B�=qB��RB�33B��B�(�B���B�
=B�p�B��C =qC p�C �C �C(�CffC�C�HC(�CffC��C�
C{CQ�C��C�
C{CQ�C��C�
C{CQ�C�\C��C{CQ�C��C��C{C\)C��C�
C�C\)C��C�HC	33C	p�C	�RC
  C
G�C
�\C
�
C{C\)C��C�HC(�CffC�RC��C33C�CC  CQ�C�\C�
C�CffC�C��C=qC�C��C�CffC�C��C=qC�C�
C{CffC�C�C33C�C��C
=CQ�C��C�HC(�Cp�CC
=C\)C��C��C=qCz�C��C{CQ�C��C�HC(�Cp�CC{C\)C�C  CG�C�\C�HC(�Cp�C�RC  CQ�C��C�HC=qC�\C�
C (�C p�C C!{C!\)C!��C!�C"=qC"�C"��C#{C#p�C#�RC${C$\)C$�C%  C%G�C%��C%�HC&(�C&z�C&��C'�C'p�C'��C({C(ffC(�RC)  C)Q�C)��C)��C*=qC*�\C*�HC+=qC+�\C+�HC,33C,�C,��C-�C-p�C-C.�C.ffC.C/{C/ffC/�C/��C0Q�C0��C0��C1G�C1��C1�C2G�C2�\C2��C3(�C3p�C3C4�C4z�C4C5{C5ffC5�C6  C6\)C6�RC7  C7Q�C7��C7�C8=qC8�C8�HC9=qC9�\C9�
C:�C:p�C:�RC;{C;ffC;C<
=C<\)C<��C<��C=G�C=��C=��C>G�C>�\C>�HC?(�C?�C?�
C@33C@�C@��CA{CAffCA�RCB{CBffCBCC{CC\)CC�CD  CDQ�CD�CE  CEQ�CE��CE�HCF=qCF�\CF��CG=qCG�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                ?�  @   @@  @�G�@�  @�  @�G�AG�A��A\)A+�A?\)A`  A�  A�  A��A�  A���A�Q�A�Q�A�Q�A�\)B�
BQ�B(�B   B(  B/�
B7�
B@  BH  BO�
BW�
B`(�Bh(�Bp  Bx  B�  B�{B�{B�  B��B�  B�{B��B�{B�{B��B�{B�  B�  B�(�B�  B�  B�{B�{B�  B�  B�  B��B��B�  B�{B�{B�{B�{B�  B��B��B��C��C�C�C��C
  C  C  C  C
=C  C  C  C��C
=C
=C   C!�C#�C%��C(
=C*
=C,  C.  C0  C1��C3��C5��C7�C9��C<
=C>  C@  CB  CC��CE��CH
=CJ
=CL  CN
=CP
=CR
=CT  CU��CW��CY��C[�C]�C_��Cb  Cd
=Cf{Ch{Cj  Cl  Cm��Co��Cr  Cs��Cv  Cx  Cz  C|
=C~
=C�C�  C�  C�  C�  C�  C���C���C���C�C�C�  C���C�  C�  C���C���C�C�C�C�C�C�C�  C�  C���C���C�C�C�C�C�C�C�  C�  C���C�  C�C�C�  C���C�  C�C�C�  C�C�  C���C�  C�C���C��C���C���C���C���C���C���C���C���C���C�  C�  C�  C�  C�C�  C���C�  C�  C���C���C�  C�  C�  C�  C���C���C�  C���C�  C�C�  C���C�  C�
=C�C�  C�  C���C���C���C���C�  C�C�C�
=C�  C���C���C�C�
=C�  C���C�  C�
=C�
=C���C���C�  C�
=C�
=C�  C�  C���C���C���C�  C�
=C�  C���C�C�  C���C�C�C���C���D   D � D  D� D  D� D  D� D  D� D�qD��D  D}qD�D��D�D� D��D	}qD
  D
��DD� D�qD� D�D}qD��D}qD�qD��D  Dz�D��Dz�D  D� D  D�D�D��D  D}qD  D��D�D� D  D� D�D��D  D��D�D�DD��D�qD}qD  D� D  D��D   D ��D!�D!� D"�D"� D#  D#� D$  D$� D%  D%}qD&  D&� D'  D'� D'�qD(}qD(�qD)� D*  D*� D+  D+� D,�D,� D,�qD-� D.  D.� D.�qD/� D0�D0� D1  D1��D2  D2}qD2�qD3}qD4  D4��D5  D5� D6  D6��D7D7��D7�qD8z�D9  D9��D:�D:}qD:��D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@��DA�DA� DA�qDB� DC  DC}qDD  DD��DD�qDE� DF  DF� DG�DG� DH  DH� DH�qDI� DJ  DJ}qDK  DK}qDK�qDL}qDL�qDMz�DN  DN}qDO  DO��DP  DP� DQ�DQ��DR�DR� DS  DS��DT�DT��DU  DUz�DU�qDV}qDW  DW� DX  DX��DY�DY��DZ  DZ� DZ�qD[� D\  D\}qD]  D]��D^�D^��D_D_��D`�D`� Da�Da�Db  Db}qDc  Dc}qDc�qDd� Dd�qDez�De�qDf� Dg  Dgz�Dg�qDh� Di  Di}qDi��Dj� Dk�Dk��Dl  Dl��Dm�Dm��Dm�qDn}qDo  Do� Dp  Dp� Dq�Dq� Dr  Dr� Dr�qDs}qDt  Dt� Dt�qDu}qDu�qDvz�Dv��Dw� Dw�qDx}qDy�Dy��Dz  Dz� D{�D{� D{�qD|z�D|�qD}� D~  D~}qD~�qD��D�HD�AHD�� D���D��qD�=qD�~�D�� D�  D�AHD��HD�� D���D�@ D�~�D���D�  D�@ D�� D��HD�HD�AHD�� D���D��qD�@ D��HD�� D���D�>�D�� D�� D�  D�@ D�� D��HD��D�AHD�� D��HD�  D�@ D�� D��qD���D�@ D�~�D�� D�  D�>�D�~�D��HD�  D�@ D�~�D�� D��D�AHD���D��HD�HD�AHD��HD��HD�HD�@ D�}qD���D�HD�AHD�� D��qD��qD�@ D��HD��HD�  D�@ D�~�D���D���D�AHD��HD���D�  D�@ D��HD�D�HD�@ D�~�D��qD���D�@ D�~�D�� D�HD�@ D�� D�� D���D�>�D�� D�D�HD�AHD��HD�D��D�B�D��HD��HD��D�@ D�|)D���D�HD�AHD���D�� D��qD�=qD�~�D���D���D�>�D�~�D�� D�  D�AHD��HD��HD�HD�AHD��HD�� D�  D�>�D�~�D���D��qD�@ D�� D��qD��qD�<)D�~�D��HD�HD�AHD��HD�� D�  D�>�D�~�D��qD��qD�>�D�~�D��qD�  D�B�D���D�D��D�AHD���D��HD�  D�@ D�~�D���D��qD�=qD�|)D��)D���D�AHD�� D���D�  D�AHD�� D�� D���D�=qD�|)D�� D�HD�AHD��HD��HD�HD�AHD�� D�� D���D�>�D�� D�� D�  D�>�D�}qD�� D�HD�>�D�~�D���D�  D�>�D�~�D���D���D�>�D�~�D��qD��qD�@ D���D�D��D�AHD�~�D�� D��D�B�D�� D��qD���D�AHD�� D�� D�  D�>�D�� D��HD��D�B�D���D��HD��D�B�D���D��HD�HD�AHD��HD��HD�  D�>�D�� D��HD�HD�@ D�� D���D���D�>�D�� D���D���D�>�D�~�D���D�  D�>�D D�� D���D�@ DÂ�D��HD�  D�@ D�~�DĽqD���D�@ D�~�Dž�D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�>�DȁHD��HD�  D�@ D�~�Dɾ�D�HD�AHD�~�DʽqD���D�>�D�~�D˾�D���D�@ D̂�D�D�  D�@ D́HD�� D�  D�>�D΁HD��HD�HD�@ Dπ DϾ�D�  D�AHDЁHD��HD�HD�@ DсHD��HD�  D�@ D҂�D�� D�HD�B�DӁHD�� D�  D�>�DԀ D�D�HD�AHDՁHD��HD���D�=qD�~�D־�D�  D�>�D�~�D�� D�HD�B�D؁HD�� D�  D�>�Dـ Dپ�D�  D�@ Dڀ D�� D�  D�@ D�}qD۾�D���D�>�D܀ Dܾ�D�  D�AHD�~�Dݾ�D�  D�AHDށHD�D�HD�>�D߁HD�� D���D�@ D�~�DྸD�  D�@ D�~�DᾸD�  D�AHD�HD��HD��qD�=qD� D�� D���D�@ D�~�D侸D�  D�@ D� D�� D�HD�AHD�HD澸D���D�B�D� D羸D���D�@ D�HD�D�  D�>�D�~�D龸D���D�@ D�HD�� D���D�@ D�HD��HD�  D�@ D�HD�� D���D�AHD�HD�� D�  D�@ D� D�� D���D�>�D� D�� D�HD�AHD��HD��HD�  D�>�D�~�D�� D�  D�>�D� D��HD���D�=qD�~�D�� D���D�>�D� D�D�HD�@ D�� D���D��qD�=qD�|)D���D�  D�>�D�~�D�� D��D�B�D��HD�� D���D�@ D��HD�� D���D�(�D�h�?�?\)?L��?��?���?�p�?�(�@�@\)@#�
@0��@E�@W
=@fff@xQ�@��@�\)@�
=@�G�@���@��@�(�@�ff@�{@�Q�@�  @���@�@��RA33A�Ap�A�\AffA�HA\)A$z�A)��A.{A1�A6ffA<(�A@��AE�AH��AN{AS33AXQ�A\(�A`��Ae�Aj�HAo\)As33Aw�A|(�A���A�33A�p�A�\)A���A�z�A�
=A���A�33A�A�Q�A��\A�z�A�
=A�G�A��
A�A�  A�=qA���A�
=A�G�A�33A�p�A��A�=qA���A�ffA�Q�A\A��A�
=A���A��HA�p�AϮA�=qA�(�A�{Aأ�A�33A�A�  A��A�(�A�RA�G�A��
A�A�  A�\A���A�\)A��A�(�A�{B (�Bp�B�RB  B��B{B33Bz�B	��B
=B  B��B{B\)B��BB�HB  B�BffB�B��B�B
=B  Bp�B�RB�
B ��B"{B#33B$(�B%p�B&�RB(  B)G�B*�\B+�B,��B-�B/
=B0Q�B1B3
=B4(�B5G�B6�\B8  B9G�B:�\B;�B<��B=�B?\)B@��BA�BC\)BDz�BEBF�HBHQ�BI��BJ�HBLQ�BM��BN�RBP  BQp�BR�RBT(�BU��BV�HBX(�BYp�BZ�HB\Q�B]B_33B`z�BaBc33Bd��Bf{Bg�Bh��Bj=qBk�Bm�Bn�\Bp(�Bq��Br�HBtQ�BuBw33Bx��BzffB{�B}�B~�\B�  B��RB��B�=qB���B��B�ffB��B��
B���B�p�B�(�B���B�p�B�(�B���B��B�ffB��B�B�z�B��B��
B��\B�\)B�{B���B�\)B�  B���B�G�B�  B��RB�\)B�{B���B�p�B�{B��RB�p�B�(�B���B��B�z�B�33B�{B��HB��B��\B�\)B�{B��HB��B�z�B�G�B�{B���B�B��\B�p�B�Q�B��B�  B���B��B�z�B�G�B�{B��HB��B�z�B�33B��B��RB�p�B�=qB�
=B��
B���B�p�B�Q�B�
=B��
B��\B�\)B�{B���B��B�(�B���B�p�B�  Bď\B�
=Bř�B�{B�z�B��HB�\)B�B�{B�z�B���B�33BɅB��
B�(�Bʏ\B���B�33B˅B��
B�(�B̏\B���B�p�B�B�(�B�z�B��HB�G�Bϙ�B��B�Q�BУ�B�
=B�G�BѮB�  B�ffB���B��BӅB��B�=qBԣ�B���B�\)BծB�(�B�z�B���B��Bי�B��B�=qB؏\B���B�\)BٮB�  B�Q�Bڣ�B�
=B�\)B�B�(�B܏\B��HB�33Bݙ�B�  B�ffB���B�33Bߙ�B�  B�ffB���B��B�B��
B�=qB��B���B�\)B�B�  B�ffB���B��B�p�B��
B�=qB��B���B�\)B�B�(�B�z�B��HB�33B陚B�  B�ffB�RB��B�p�B��
B�(�B��B�
=B�\)B��
B�=qB��B�
=B�p�B�B�(�B��\B���B�\)B��
B�(�B�\B���B�\)B�B�(�B�z�B��HB�G�B��B�{B�z�B���B�33B���B�  B�z�B���B�G�B��B�{B��\B���B�p�B��
B�=qB��RB�33B��B�(�B���B�
=B�p�B��C =qC p�C �C �C(�CffC�C�HC(�CffC��C�
C{CQ�C��C�
C{CQ�C��C�
C{CQ�C�\C��C{CQ�C��C��C{C\)C��C�
C�C\)C��C�HC	33C	p�C	�RC
  C
G�C
�\C
�
C{C\)C��C�HC(�CffC�RC��C33C�CC  CQ�C�\C�
C�CffC�C��C=qC�C��C�CffC�C��C=qC�C�
C{CffC�C�C33C�C��C
=CQ�C��C�HC(�Cp�CC
=C\)C��C��C=qCz�C��C{CQ�C��C�HC(�Cp�CC{C\)C�C  CG�C�\C�HC(�Cp�C�RC  CQ�C��C�HC=qC�\C�
C (�C p�C C!{C!\)C!��C!�C"=qC"�C"��C#{C#p�C#�RC${C$\)C$�C%  C%G�C%��C%�HC&(�C&z�C&��C'�C'p�C'��C({C(ffC(�RC)  C)Q�C)��C)��C*=qC*�\C*�HC+=qC+�\C+�HC,33C,�C,��C-�C-p�C-C.�C.ffC.C/{C/ffC/�C/��C0Q�C0��C0��C1G�C1��C1�C2G�C2�\C2��C3(�C3p�C3C4�C4z�C4C5{C5ffC5�C6  C6\)C6�RC7  C7Q�C7��C7�C8=qC8�C8�HC9=qC9�\C9�
C:�C:p�C:�RC;{C;ffC;C<
=C<\)C<��C<��C=G�C=��C=��C>G�C>�\C>�HC?(�C?�C?�
C@33C@�C@��CA{CAffCA�RCB{CBffCBCC{CC\)CC�CD  CDQ�CD�CE  CEQ�CE��CE�HCF=qCF�\CF��CG=qCG�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�ĜAҮAҸRAҾwAҼjAҕ�AҁA�O�A�=qA�$�A�
=A�ƨAуA�XA�1'A�VA��HA��A�jA�VA��A�
=A��AȶFA�z�A�^5A�7LA�/A�9XA�5?A�-A�{A��AǸRAǓuA�ZA�5?A�(�A�  A��
AƲ-A�|�A�^5A�O�A�  A���A��hA�jA��#A�{A�~�A�bNA�I�A��uA���A��RA�p�A���A�bNA���A�O�A�z�A��A�=qA��A�-A�O�A�v�A�A��A��RA�-A�A|5?AzjAx�RAs�#Ao��AmG�Ak��AjI�Ah��AdjAc%Aa�
A_��AZ  AT=qAP��AM?}AK7LAH�AG�PAFQ�AD�ABbNAA�AAAAdZA@-A<�/A7XA5�PA5hsA3K�A1x�A1l�A1K�A1+A1VA0�A0ĜA0ȴA0~�A0��A0E�A0Q�A0 �A/�A/�PA/XA//A/VA/
=A.�9A.jA.M�A-�hA+�hA*^5A)��A(z�A(�A'�mA'VA&�A&�A%�mA%`BA$��A$5?A#�wA#33A"�+A!�A �RA A�An�A1AG�AȴA��A �A�-A�A�A�\AM�A �AbAl�A�HA�9A5?A��AO�A��AVA�A��AK�AoAȴA�uA=qA�A�^At�AK�A��AjA�mA�^A�7A�A�A�A|�AXA"�A��A��A�`A�9A�uAr�AQ�A$�A��A��Ap�A+A��An�A�A��AƨA�A+A
�A
I�A	�A	��A	dZA	G�A	/A�yA�!A�+AE�A�A��AoA{AS�Ar�A�A1AA�A�-Ap�A�A^5A1'A$�AJA�A�hAdZA�A ��A �A I�A 1'A b@���@��P@�;d@��@��\@�-@���@�?}@��D@��@���@�l�@�+@���@���@�v�@�x�@�z�@���@�;d@���@���@���@���@���@�F@��@��y@���@��@�@� �@�@�C�@�
=@�v�@�@���@�7L@�u@���@�|�@�33@�$�@�`B@�&�@�A�@�;d@��@���@�5?@�@�hs@�&�@���@�@�D@���@�K�@◍@��@�`B@�/@��D@�b@��@��@�?}@�/@ܬ@��;@�33@�V@ى7@ج@�b@ם�@ָR@�&�@�ƨ@��@�/@��/@�b@Ͼw@�
=@Η�@��@�O�@̛�@�ƨ@�C�@��y@ʸR@ʗ�@�M�@��T@�?}@�bN@Ǿw@���@�M�@��@���@őh@�x�@�O�@ģ�@���@Å@�33@§�@�V@�-@���@�p�@��@�A�@��w@�C�@��@���@�~�@�ff@���@�X@�%@���@�A�@��;@��@�~�@�M�@�E�@�{@��7@�`B@�X@�G�@��/@��D@�Z@�(�@���@�33@�o@��!@�5?@��-@�X@��@�V@�%@��/@��u@�A�@��@�dZ@�;d@�+@��+@�-@�J@��T@�@�p�@�G�@�7L@�&�@��j@�(�@���@��w@�o@�$�@��^@�X@�%@��`@��u@�1@��F@�\)@���@��+@�E�@��@��-@�%@���@��@��w@��P@�C�@��H@��R@�^5@��#@��7@�G�@�%@��@�r�@�9X@���@���@��!@���@��+@�$�@�p�@�%@���@���@��j@���@�r�@�C�@��y@�^5@��@�G�@�Ĝ@�j@�1'@�ƨ@�K�@�"�@�
=@���@�M�@�$�@�{@��#@�G�@���@��@��;@��
@���@���@�E�@�@�@���@���@��
@��@�dZ@�"�@��!@�v�@��@��-@��@���@���@��@�dZ@�@��@��!@�$�@��@��T@��#@�@��-@��h@��@�O�@���@��9@�Z@�(�@��@��P@��y@�E�@�J@��@���@���@�G�@���@�r�@�9X@�  @���@���@��@�t�@�dZ@�K�@�@��!@�V@�$�@���@���@�hs@�O�@�?}@�/@�V@���@�9X@��@���@��@�t�@�S�@��@�ȴ@���@���@���@�M�@��#@�X@�V@�Ĝ@���@�r�@�Z@�I�@�A�@�9X@�1'@�(�@�b@�@�@�P@|�@;d@~��@}�-@}?}@|�@|(�@{�
@{�@z-@y�^@yG�@x��@x��@x�@xbN@w\)@vv�@v@u`B@uV@t9X@st�@s33@r�@r~�@q��@p��@p��@pbN@pA�@o�P@o
=@nv�@n5?@n{@m��@m�h@mp�@m�@lI�@k�m@kS�@k"�@j�@j��@j^5@jJ@i��@ihs@i%@hQ�@g��@g�w@g��@g+@f��@f$�@e��@e/@dj@d(�@c��@c�F@ct�@cdZ@c"�@b�@b�!@b~�@b�@ax�@aG�@`��@` �@_l�@^��@^�+@^{@]��@]@]�h@\��@\z�@\9X@[�m@[�F@[C�@Z�H@Z~�@ZJ@Y7L@X��@X�9@X��@X�u@X�@X1'@W�P@Vȴ@V��@Vv�@VV@V{@U@U��@U�h@U�@T�j@Tj@T(�@T�@S�m@St�@S33@S33@So@R�@R��@R~�@RJ@P�`@P �@Pb@O�;@O
=@Nff@N$�@M�@M@M�-@M�-@M`B@M?}@M?}@M�@L�D@L(�@Kƨ@Kt�@Ko@J��@JM�@J�@I�@IX@H�@Hb@G�@G�;@G�w@G��@Gl�@GK�@G+@G�@Fȴ@F�+@E�@EO�@D�@C�
@CS�@C@B^5@B-@A�#@Ax�@A7L@@��@@�`@@��@@1'@?�w@?;d@>��@>ȴ@>�R@>�+@=�@=�h@=p�@=`B@=?}@=/@<��@<j@<Z@<(�@<1@<1@;"�@:-@:�@9�7@9%@8�u@8r�@7��@6ȴ@6��@6E�@5�T@5�-@5�h@5O�@5?}@5/@5V@4Z@41@3ƨ@3dZ@2��@2^5@2�@2J@1�@1�#@1��@1��@1hs@1�@0�`@0�@/�;@/��@/�P@/l�@/K�@/�@.E�@-��@-@-�@,�@,�D@,I�@+�@+33@*�@*��@*�\@*^5@*M�@*=q@*�@)��@)�#@)�^@)�7@)�@(Ĝ@(�u@(Q�@( �@'�@'��@'�@'��@'|�@'K�@&ȴ@%�@%�T@%�T@%�T@%��@%?}@$��@$�@$�@$z�@$9X@$�@#�m@#�
@#�
@#t�@"��@"��@"�\@"~�@"M�@"�@!��@!�@!��@!��@!hs@!G�@!%@ ��@ Ĝ@ ��@ �@ b@�@�P@�@
=@�y@�+@$�@@�@��@@�h@p�@`B@�@�@�/@�@I�@�@ƨ@�@33@�@�!@�\@�\@�\@n�@^5@�@�@��@��@hs@&�@%@��@�9@�u@r�@Q�@Q�@Q�@A�@A�@1'@ �@b@�@��@l�@+@��@��@��@v�@E�@@��@@p�@/@�/@�D@9X@(�@�@1@�
@��@t�@C�@�@��@~�@-@�@��@�^@��@x�@X@X@&�@%@�9@Q�@1'@  @�;@��@��@��@��@��@��@�@�P@|�@;d@�@�@�@
=@��@�y@�R@v�@{@��@�h@�h@�h@�@�@p�@`B@?}@V@��@�/@��@�j@�@�D@Z@Z@ZAҺ^AҼjAҮA�ƨA�ȴA���A���A�ȴAҼjAҮAҶFAҮAҮAҮAҲ-AҾwA���AҾwAҾwAҺ^A�A�A�Aң�AғuAҋDAқ�Aҙ�A�t�A�x�A�x�A�XA�K�A�A�A�?}A�=qA�=qA�-A� �A� �A��A��A�
=A��A��A��A��`A��/A��
A���AѲ-AѰ!Aѧ�Aџ�AѓuAч+A�~�A�r�A�hsA�ffA�hsA�ffA�`BA�XA�Q�A�E�A�?}A�9XA�5?A�1'A�-A�(�A� �A��A��A��A�VA�1A�A�  A�A���A���A��A��`A��#A���AоwAУ�AЅAϴ9A�?}A̗�A�ZA��A�ffA�(�A��AʁA��A�Aɰ!Aɡ�AɍPA�S�A�E�A�5?A�33A�-A�&�A��A��A��A��A�oA�VA�
=A�1A�
=A�VA�JA�
=A�A�A�A���A��A��
A���A���A���A���A�AȶFAȧ�AȑhAȃA�~�A�~�A�|�A�x�A�t�A�r�A�r�A�n�A�hsA�dZA�ZA�M�A�E�A�?}A�;dA�7LA�5?A�33A�+A�(�A�+A�/A�1'A�1'A�33A�33A�33A�7LA�9XA�=qA�;dA�9XA�7LA�7LA�7LA�7LA�7LA�5?A�1'A�-A�+A�-A�-A�+A�&�A� �A��A�oA�JA�%A�A���A��A��yA��`A��;A��A���AǺ^AǬAǣ�Aǟ�Aǝ�Aǝ�AǗ�AǏ\AǁA�p�A�jA�ffA�S�A�G�A�=qA�9XA�7LA�5?A�5?A�33A�/A�+A�+A�(�A� �A��A�oA�
=A�  A���A��A��mA��;A��A���A���A�ȴAƾwAƴ9Aư!AƮAƩ�Aơ�AƋDA�z�A�v�A�t�A�p�A�jA�7LA�  AżjA��A� �A���Aô9A�ȴA�`BA��RA��/A��7A�dZA���A�t�A��/A�A�A���A���A�33A��A�S�A���A�C�A�A���A��!A�v�A�O�A�A�A�5?A�(�A�A���A��A�XA���A�\)A��RA�5?A���A���A��A�t�A�l�A�ZA�+A��jA�?}A��yA�bNA��FA�bNA�/A�VA���A��/A�A���A��+A�x�A�p�A�hsA�dZA�`BA�`BA�^5A�`BA�^5A�S�A��A���A��A��FA��DA�hsA��A��/A�A���A��PA�t�A�bNA�I�A�9XA�$�A���A��wA��7A�hsA�ZA�K�A�5?A�$�A� �A��A��A�VA�  A��A��#A�ĜA���A��DA�ffA�;dA�A��#A���A��A���A��PA�v�A�ffA�M�A� �A��A�ȴA��A��A�K�A�+A���A���A���A�n�A�XA�Q�A�$�A���A��TA�ƨA��A��hA�v�A�5?A��A���A���A��jA��^A��9A��A���A���A��DA�z�A�`BA�?}A�"�A�
=A���A��mA��#A���A���A���A�A���A�x�A�7LA��A��7A�oA���A��7A�;dA��A��9A���A��7A�dZA�$�A��`A���A�p�A�1'A��A�~�A� �A���A��A��
A��-A�t�A�=qA��A���A���A��A��`A��;A�ȴA��A��PA�E�A�ȴA�|�A�ƨA�&�A��#A���A�"�A�"�A���A� �A���A��HA��A�`BA�ĜA�S�A���A���A�9XA���A��!A�t�A�?}A���A���A�ZA�;dA��\A��A��#A��!A�ZA���A��A���A��9A���A���A��7A�`BA���A�A�A��HA���A�hsA�;dA��A��`A��^A�~�A�E�A�{AƨA;dA~�A~1'A}S�A|��A|��A| �A{�wA{l�A{7LA{VAz�yAz��Az��Az^5Az�Ay�TAy�Ayt�AyG�AyAx��Ax�uAx �Aw�Au��At��Atv�At1Asx�Ar~�AqƨAqXAp�Ap�DAp(�AodZAn��An^5An�Am�mAm�Aml�AmVAl��Al�+AlM�Al�Ak�Ak�-Ak��AkK�Ak�Aj�9Ajv�AjffAjE�Aj1'Aj�AjJAi�Ai��AiƨAi��Ah�yAg�mAf��Ae�Ad��Ad~�AdE�Ad�Ac�Ac��Ac�^Ac��Ac�AcS�Ab�jAb(�Aa�Aa�Aa�mAa�TAa�#Aa��Aa��Aax�Aa/A`�jA_��A^�A^1A\�9A[�#A[7LAZVAYp�AX��AXE�AVȴAU�
AU+ATv�ASƨARĜAR=qAQ�AQ��AQ/AP�APv�AOdZAN��ANA�AM��AMC�AL�ALĜAK�FAK��AK�AKl�AKG�AKoAJ��AJ1AI�^AIG�AH��AHn�AH�AG�AG��AG�FAG�hAGdZAGK�AG�AF��AF�AFQ�AF1'AF  AE��AEt�ADȴAD(�AC��AC`BAB��AB��AB�\ABn�ABVABA�AB9XAB(�AB�ABbAA��AA�#AA�wAA�wAA��AA��AA�
AA��AA�^AA��AA�PAAx�AAp�AAhsAAXAAK�AAC�AA+A@��A@E�A?��A?t�A>�HA>-A=�^A=�7A<��A<JA;O�A:�A8�+A7dZA6��A6r�A5�mA5��A5�-A5�7A5x�A5l�A5p�A5l�A5p�A5p�A5l�A5dZA5\)A5S�A4��A4ZA4 �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                A���A�ĜAҮAҸRAҾwAҼjAҕ�AҁA�O�A�=qA�$�A�
=A�ƨAуA�XA�1'A�VA��HA��A�jA�VA��A�
=A��AȶFA�z�A�^5A�7LA�/A�9XA�5?A�-A�{A��AǸRAǓuA�ZA�5?A�(�A�  A��
AƲ-A�|�A�^5A�O�A�  A���A��hA�jA��#A�{A�~�A�bNA�I�A��uA���A��RA�p�A���A�bNA���A�O�A�z�A��A�=qA��A�-A�O�A�v�A�A��A��RA�-A�A|5?AzjAx�RAs�#Ao��AmG�Ak��AjI�Ah��AdjAc%Aa�
A_��AZ  AT=qAP��AM?}AK7LAH�AG�PAFQ�AD�ABbNAA�AAAAdZA@-A<�/A7XA5�PA5hsA3K�A1x�A1l�A1K�A1+A1VA0�A0ĜA0ȴA0~�A0��A0E�A0Q�A0 �A/�A/�PA/XA//A/VA/
=A.�9A.jA.M�A-�hA+�hA*^5A)��A(z�A(�A'�mA'VA&�A&�A%�mA%`BA$��A$5?A#�wA#33A"�+A!�A �RA A�An�A1AG�AȴA��A �A�-A�A�A�\AM�A �AbAl�A�HA�9A5?A��AO�A��AVA�A��AK�AoAȴA�uA=qA�A�^At�AK�A��AjA�mA�^A�7A�A�A�A|�AXA"�A��A��A�`A�9A�uAr�AQ�A$�A��A��Ap�A+A��An�A�A��AƨA�A+A
�A
I�A	�A	��A	dZA	G�A	/A�yA�!A�+AE�A�A��AoA{AS�Ar�A�A1AA�A�-Ap�A�A^5A1'A$�AJA�A�hAdZA�A ��A �A I�A 1'A b@���@��P@�;d@��@��\@�-@���@�?}@��D@��@���@�l�@�+@���@���@�v�@�x�@�z�@���@�;d@���@���@���@���@���@�F@��@��y@���@��@�@� �@�@�C�@�
=@�v�@�@���@�7L@�u@���@�|�@�33@�$�@�`B@�&�@�A�@�;d@��@���@�5?@�@�hs@�&�@���@�@�D@���@�K�@◍@��@�`B@�/@��D@�b@��@��@�?}@�/@ܬ@��;@�33@�V@ى7@ج@�b@ם�@ָR@�&�@�ƨ@��@�/@��/@�b@Ͼw@�
=@Η�@��@�O�@̛�@�ƨ@�C�@��y@ʸR@ʗ�@�M�@��T@�?}@�bN@Ǿw@���@�M�@��@���@őh@�x�@�O�@ģ�@���@Å@�33@§�@�V@�-@���@�p�@��@�A�@��w@�C�@��@���@�~�@�ff@���@�X@�%@���@�A�@��;@��@�~�@�M�@�E�@�{@��7@�`B@�X@�G�@��/@��D@�Z@�(�@���@�33@�o@��!@�5?@��-@�X@��@�V@�%@��/@��u@�A�@��@�dZ@�;d@�+@��+@�-@�J@��T@�@�p�@�G�@�7L@�&�@��j@�(�@���@��w@�o@�$�@��^@�X@�%@��`@��u@�1@��F@�\)@���@��+@�E�@��@��-@�%@���@��@��w@��P@�C�@��H@��R@�^5@��#@��7@�G�@�%@��@�r�@�9X@���@���@��!@���@��+@�$�@�p�@�%@���@���@��j@���@�r�@�C�@��y@�^5@��@�G�@�Ĝ@�j@�1'@�ƨ@�K�@�"�@�
=@���@�M�@�$�@�{@��#@�G�@���@��@��;@��
@���@���@�E�@�@�@���@���@��
@��@�dZ@�"�@��!@�v�@��@��-@��@���@���@��@�dZ@�@��@��!@�$�@��@��T@��#@�@��-@��h@��@�O�@���@��9@�Z@�(�@��@��P@��y@�E�@�J@��@���@���@�G�@���@�r�@�9X@�  @���@���@��@�t�@�dZ@�K�@�@��!@�V@�$�@���@���@�hs@�O�@�?}@�/@�V@���@�9X@��@���@��@�t�@�S�@��@�ȴ@���@���@���@�M�@��#@�X@�V@�Ĝ@���@�r�@�Z@�I�@�A�@�9X@�1'@�(�@�b@�@�@�P@|�@;d@~��@}�-@}?}@|�@|(�@{�
@{�@z-@y�^@yG�@x��@x��@x�@xbN@w\)@vv�@v@u`B@uV@t9X@st�@s33@r�@r~�@q��@p��@p��@pbN@pA�@o�P@o
=@nv�@n5?@n{@m��@m�h@mp�@m�@lI�@k�m@kS�@k"�@j�@j��@j^5@jJ@i��@ihs@i%@hQ�@g��@g�w@g��@g+@f��@f$�@e��@e/@dj@d(�@c��@c�F@ct�@cdZ@c"�@b�@b�!@b~�@b�@ax�@aG�@`��@` �@_l�@^��@^�+@^{@]��@]@]�h@\��@\z�@\9X@[�m@[�F@[C�@Z�H@Z~�@ZJ@Y7L@X��@X�9@X��@X�u@X�@X1'@W�P@Vȴ@V��@Vv�@VV@V{@U@U��@U�h@U�@T�j@Tj@T(�@T�@S�m@St�@S33@S33@So@R�@R��@R~�@RJ@P�`@P �@Pb@O�;@O
=@Nff@N$�@M�@M@M�-@M�-@M`B@M?}@M?}@M�@L�D@L(�@Kƨ@Kt�@Ko@J��@JM�@J�@I�@IX@H�@Hb@G�@G�;@G�w@G��@Gl�@GK�@G+@G�@Fȴ@F�+@E�@EO�@D�@C�
@CS�@C@B^5@B-@A�#@Ax�@A7L@@��@@�`@@��@@1'@?�w@?;d@>��@>ȴ@>�R@>�+@=�@=�h@=p�@=`B@=?}@=/@<��@<j@<Z@<(�@<1@<1@;"�@:-@:�@9�7@9%@8�u@8r�@7��@6ȴ@6��@6E�@5�T@5�-@5�h@5O�@5?}@5/@5V@4Z@41@3ƨ@3dZ@2��@2^5@2�@2J@1�@1�#@1��@1��@1hs@1�@0�`@0�@/�;@/��@/�P@/l�@/K�@/�@.E�@-��@-@-�@,�@,�D@,I�@+�@+33@*�@*��@*�\@*^5@*M�@*=q@*�@)��@)�#@)�^@)�7@)�@(Ĝ@(�u@(Q�@( �@'�@'��@'�@'��@'|�@'K�@&ȴ@%�@%�T@%�T@%�T@%��@%?}@$��@$�@$�@$z�@$9X@$�@#�m@#�
@#�
@#t�@"��@"��@"�\@"~�@"M�@"�@!��@!�@!��@!��@!hs@!G�@!%@ ��@ Ĝ@ ��@ �@ b@�@�P@�@
=@�y@�+@$�@@�@��@@�h@p�@`B@�@�@�/@�@I�@�@ƨ@�@33@�@�!@�\@�\@�\@n�@^5@�@�@��@��@hs@&�@%@��@�9@�u@r�@Q�@Q�@Q�@A�@A�@1'@ �@b@�@��@l�@+@��@��@��@v�@E�@@��@@p�@/@�/@�D@9X@(�@�@1@�
@��@t�@C�@�@��@~�@-@�@��@�^@��@x�@X@X@&�@%@�9@Q�@1'@  @�;@��@��@��@��@��@��@�@�P@|�@;d@�@�@�@
=@��@�y@�R@v�@{@��@�h@�h@�h@�@�@p�@`B@?}@V@��@�/@��@�j@�@�D@Z@Z@ZAҺ^AҼjAҮA�ƨA�ȴA���A���A�ȴAҼjAҮAҶFAҮAҮAҮAҲ-AҾwA���AҾwAҾwAҺ^A�A�A�Aң�AғuAҋDAқ�Aҙ�A�t�A�x�A�x�A�XA�K�A�A�A�?}A�=qA�=qA�-A� �A� �A��A��A�
=A��A��A��A��`A��/A��
A���AѲ-AѰ!Aѧ�Aџ�AѓuAч+A�~�A�r�A�hsA�ffA�hsA�ffA�`BA�XA�Q�A�E�A�?}A�9XA�5?A�1'A�-A�(�A� �A��A��A��A�VA�1A�A�  A�A���A���A��A��`A��#A���AоwAУ�AЅAϴ9A�?}A̗�A�ZA��A�ffA�(�A��AʁA��A�Aɰ!Aɡ�AɍPA�S�A�E�A�5?A�33A�-A�&�A��A��A��A��A�oA�VA�
=A�1A�
=A�VA�JA�
=A�A�A�A���A��A��
A���A���A���A���A�AȶFAȧ�AȑhAȃA�~�A�~�A�|�A�x�A�t�A�r�A�r�A�n�A�hsA�dZA�ZA�M�A�E�A�?}A�;dA�7LA�5?A�33A�+A�(�A�+A�/A�1'A�1'A�33A�33A�33A�7LA�9XA�=qA�;dA�9XA�7LA�7LA�7LA�7LA�7LA�5?A�1'A�-A�+A�-A�-A�+A�&�A� �A��A�oA�JA�%A�A���A��A��yA��`A��;A��A���AǺ^AǬAǣ�Aǟ�Aǝ�Aǝ�AǗ�AǏ\AǁA�p�A�jA�ffA�S�A�G�A�=qA�9XA�7LA�5?A�5?A�33A�/A�+A�+A�(�A� �A��A�oA�
=A�  A���A��A��mA��;A��A���A���A�ȴAƾwAƴ9Aư!AƮAƩ�Aơ�AƋDA�z�A�v�A�t�A�p�A�jA�7LA�  AżjA��A� �A���Aô9A�ȴA�`BA��RA��/A��7A�dZA���A�t�A��/A�A�A���A���A�33A��A�S�A���A�C�A�A���A��!A�v�A�O�A�A�A�5?A�(�A�A���A��A�XA���A�\)A��RA�5?A���A���A��A�t�A�l�A�ZA�+A��jA�?}A��yA�bNA��FA�bNA�/A�VA���A��/A�A���A��+A�x�A�p�A�hsA�dZA�`BA�`BA�^5A�`BA�^5A�S�A��A���A��A��FA��DA�hsA��A��/A�A���A��PA�t�A�bNA�I�A�9XA�$�A���A��wA��7A�hsA�ZA�K�A�5?A�$�A� �A��A��A�VA�  A��A��#A�ĜA���A��DA�ffA�;dA�A��#A���A��A���A��PA�v�A�ffA�M�A� �A��A�ȴA��A��A�K�A�+A���A���A���A�n�A�XA�Q�A�$�A���A��TA�ƨA��A��hA�v�A�5?A��A���A���A��jA��^A��9A��A���A���A��DA�z�A�`BA�?}A�"�A�
=A���A��mA��#A���A���A���A�A���A�x�A�7LA��A��7A�oA���A��7A�;dA��A��9A���A��7A�dZA�$�A��`A���A�p�A�1'A��A�~�A� �A���A��A��
A��-A�t�A�=qA��A���A���A��A��`A��;A�ȴA��A��PA�E�A�ȴA�|�A�ƨA�&�A��#A���A�"�A�"�A���A� �A���A��HA��A�`BA�ĜA�S�A���A���A�9XA���A��!A�t�A�?}A���A���A�ZA�;dA��\A��A��#A��!A�ZA���A��A���A��9A���A���A��7A�`BA���A�A�A��HA���A�hsA�;dA��A��`A��^A�~�A�E�A�{AƨA;dA~�A~1'A}S�A|��A|��A| �A{�wA{l�A{7LA{VAz�yAz��Az��Az^5Az�Ay�TAy�Ayt�AyG�AyAx��Ax�uAx �Aw�Au��At��Atv�At1Asx�Ar~�AqƨAqXAp�Ap�DAp(�AodZAn��An^5An�Am�mAm�Aml�AmVAl��Al�+AlM�Al�Ak�Ak�-Ak��AkK�Ak�Aj�9Ajv�AjffAjE�Aj1'Aj�AjJAi�Ai��AiƨAi��Ah�yAg�mAf��Ae�Ad��Ad~�AdE�Ad�Ac�Ac��Ac�^Ac��Ac�AcS�Ab�jAb(�Aa�Aa�Aa�mAa�TAa�#Aa��Aa��Aax�Aa/A`�jA_��A^�A^1A\�9A[�#A[7LAZVAYp�AX��AXE�AVȴAU�
AU+ATv�ASƨARĜAR=qAQ�AQ��AQ/AP�APv�AOdZAN��ANA�AM��AMC�AL�ALĜAK�FAK��AK�AKl�AKG�AKoAJ��AJ1AI�^AIG�AH��AHn�AH�AG�AG��AG�FAG�hAGdZAGK�AG�AF��AF�AFQ�AF1'AF  AE��AEt�ADȴAD(�AC��AC`BAB��AB��AB�\ABn�ABVABA�AB9XAB(�AB�ABbAA��AA�#AA�wAA�wAA��AA��AA�
AA��AA�^AA��AA�PAAx�AAp�AAhsAAXAAK�AAC�AA+A@��A@E�A?��A?t�A>�HA>-A=�^A=�7A<��A<JA;O�A:�A8�+A7dZA6��A6r�A5�mA5��A5�-A5�7A5x�A5l�A5p�A5l�A5p�A5p�A5l�A5dZA5\)A5S�A4��A4ZA4 �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	4�B	6B	5tB	4�B	4�B	5?B	4nB	4�B	5tB	49B	3�B	3�B	1�B	-�B	)�B	&�B	$@B	�B	_B� B��B�0BϫBбBԕB� B�TB��B��B�B�B�B�
B��B�B�B�B�B�,B��B��B��B	  B	8B	�B
��B
��B	B!B&�BG�BL0B[�BA B;�BE�Bc�Bh>Bd�B_pBW�BLdB@�B5?B{B
�B
�dB
��B
jB
%zB
�B	�sB	�B	�3B	�[B	�-B	�B	�B	�B	��B	��B	}�B	}�B	q�B	jB	`�B	e,B	^�B	P}B	E�B	R�B	PHB	S�B	P�B	R�B	]/B	bB	f�B	rGB	��B	�SB	��B	pB	[#B	\)B	hsB	b�B	dZB	h�B	l�B	rGB	w2B	{�B	�oB	��B	�uB	��B	��B	��B	��B	یB	��B	�mB	��B	�B	��B
AB
SB

�B
�B
VB
�B
MB
@B
{B
�B
�B
�B
 B
�B
�B
1B
�B
�B
�B
%�B
*eB
)_B
*0B
&�B
*0B
+�B
,=B
-CB
1�B
33B
3�B
8�B
<6B
<jB
=�B
F?B
L0B
H�B
IB
MB
RTB
U2B
T�B
R�B
V�B
YB
]dB
aB
aB
b�B
b�B
d�B
dZB
f2B
lWB
k�B
lWB
lWB
k�B
k�B
kB
k�B
iB
g�B
h
B
h>B
lWB
m�B
m�B
m)B
l"B
k�B
j�B
j�B
jKB
g�B
g8B
g8B
e�B
d�B
bB
`vB
_�B
_�B
]�B
]�B
[#B
YB
YB
WsB
V�B
V�B
U�B
T�B
T,B
R�B
QB
OvB
LdB
J�B
E9B
EB
A�B
@�B
@�B
@�B
?HB
A�B
B�B
@�B
>BB
>B
>B
=<B
<6B
:�B
:*B
9�B
9$B
8�B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
8�B
9�B
8�B
8�B
7�B
6FB
5�B
5?B
5�B
6FB
6�B
7B
5�B
4nB
2�B
2�B
1�B
2�B
1'B
1�B
.B
,�B
,=B
,B
.B
,qB
*�B
*�B
)_B
(�B
)�B
'�B
'B
'�B
&�B
&�B
$�B
#�B
%B
!�B
 �B
"�B
!bB
�B
�B
 �B
 'B
�B
�B
�B
OB
�B
�B
�B
�B
~B
�B
CB
xB
qB
�B
CB
=B
kB
�B
B
�B
�B
�B
�B
�B
$B
�B
�B
�B
hB
�B
�B
�B
FB
B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
MB
�B
�B
MB
MB
�B
MB
MB
SB
SB
SB
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
+B
�B
_B
_B
�B
eB
B
7B
�B
�B
CB
xB
CB
B
CB
�B
xB
CB
B
B
�B
�B
�B
~B
IB
B
�B
�B
B
B
�B
�B
�B
~B
B
�B
�B
!B
�B
�B
 \B
�B
 'B
�B
�B
 'B
�B
�B
VB
 \B
�B
�B
VB
VB
�B
�B
�B
IB
B
~B
IB
�B
�B
�B
!B
VB
!B
�B
 'B
 \B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"4B
"4B
#B
#B
#nB
$�B
$�B
$tB
$B
$@B
$B
%B
%zB
%B
$�B
%B
$�B
$�B
$�B
&�B
&�B
'�B
'�B
(�B
)�B
+kB
,qB
-B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
/�B
/B
/�B
/�B
/OB
/�B
0�B
/�B
/�B
/�B
.�B
1'B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1[B
1�B
2�B
2�B
3�B
49B
49B
4nB
5�B
5tB
5�B
5�B
5�B
5�B
5�B
5�B
6B
6zB
6zB
7B
7B
7B
7�B
8�B
9�B
9�B
9XB
9�B
9XB
:*B
:�B
;0B
;0B
;�B
;�B
;�B
;�B
;�B
;�B
;�B
<6B
<jB
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
?B
>�B
?}B
?�B
?�B
?}B
?�B
@B
@�B
@�B
@�B
@B
A B
A�B
B�B
B�B
CaB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D3B
D3B
DgB
D�B
DgB
DgB
D�B
E9B
F?B
FB
F?B
GB
F�B
F�B
H�B
HKB
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J#B
JXB
I�B
K^B
K^B
K^B
K)B
K�B
LdB
L�B
L�B
MB
MB
M�B
M�B
MjB
M�B
MjB
NB
NB
M�B
N<B
N�B
OBB
OvB
OvB
OvB
O�B
O�B
PB
P�B
PHB
P�B
Q�B
Q�B
Q�B
Q�B
RTB
S&B
R�B
S�B
S�B
TaB
TaB
TaB
T�B
U2B
T�B
V9B
VB
U�B
U�B
V9B
V�B
V�B
W?B
W?B
W?B
WsB
W
B
W�B
WsB
WsB
WsB
X�B
X�B
YKB
YKB
YKB
Y�B
Y�B
Y�B
ZQB
[#B
[#B
Z�B
Z�B
Z�B
Z�B
[#B
[�B
\]B
\)B
\)B
\)B
\]B
\�B
\]B
\]B
\�B
]/B
]dB
]dB
]dB
]�B
^5B
^5B
^B
^5B
^B
^jB
^B
^�B
`vB
_�B
_�B
`B
aB
aB
aB
a�B
a�B
a|B
a|B
a�B
a�B
a|B
a�B
b�B
bB
bB
bNB
bNB
bNB
b�B
b�B
b�B
c�B
c�B
d&B
c�B
d&B
c�B
d&B
d&B
dZB
c�B
d&B
dZB
d&B
d�B
d�B
e�B
f2B
f2B
ffB
f�B
ffB
gmB
gB
g8B
g8B
gB
gmB
h
B
h>B
h�B
h�B
iB
iB
iDB
jB
jB
jB
jB
jKB
jB
jB
j�B
j�B
j�B
j�B
jKB
kQB
k�B
k�B
l�B
m)B
m�B
m]B
m�B
m�B
m�B
n/B
ncB
ncB
n�B
n�B
n�B
oiB
pB
qAB
qAB
qvB
qvB
q�B
rB
rB
rB
rGB
rGB
rGB
r|B
r|B
r�B
r|B
r�B
r|B
rGB
sB
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
uZB
uZB
uZB
u%B
uZB
uZB
u�B
v`B
v�B
v�B
v�B
w2B
wfB
w�B
x8B
x8B
x8B
x8B
xB
xB
xB
xB
w�B
x8B
x8B
yrB
y�B
y�B
y�B
y�B
zB
z�B
{JB
{JB
{JB
{B
{B
{B
{�B
{�B
{B
|B
|�B
|�B
|�B
|�B
|�B
}VB
}"B
}VB
}VB
}�B
}�B
}�B
}�B
}�B
~(B
~(B
~(B
~]B
~]B
~�B
.B
~�B
~�B
�B
� B
�B
�B
� B
� B
�4B
�iB
�4B
��B
��B
��B
�B
�oB
��B
�B
�AB
��B
��B
�B
�GB
�{B
�B
��B
��B
�MB
��B
��B
�B
�MB
�B
��B
��B
�SB
�B
�SB
��B
��B
�SB
��B
��B
��B
��B
��B
��B
�%B
��B
��B
�+B
��B
��B
��B
��B
�fB
��B
�fB
��B
�7B
�7B
�	B
�	B
�=B
�=B
�=B
�rB
��B
��B
�B
�DB
�xB
��B
�JB
�~B
�~B
�~B
��B
��B
�B
��B
��B
�PB
��B
��B
�"B
�"B
�VB
��B
��B
��B
�VB
�VB
��B
��B
��B
��B
�(B
�(B
��B
��B
�(B
�(B
�(B
�\B
��B
�.B
��B
��B
��B
��B
��B
��B
��B
� B
�4B
�hB
�hB
��B
��B
��B
��B
�:B
�oB
�oB
��B	1[B	1[B	7LB	5B	6�B	5B	4B	5�B	8�B	7�B	49B	1�B	5?B	4nB	6zB	2aB	4nB	5�B	6FB	4nB	2�B	3hB	49B	9�B	9XB	4B	1�B	6FB	4�B	33B	6B	7�B	4B	33B	5B	3�B	2�B	1�B	33B	4B	3�B	33B	1'B	;�B	2-B	2-B	1�B	2-B	2�B	5�B	0!B	-�B	0�B	0�B	2-B	,�B	,�B	,�B	,qB	)�B	(�B	(�B	)�B	*eB	)_B	&�B	&�B	(XB	(XB	'�B	%zB	&B	&LB	&B	$tB	#:B	$B	%FB	$�B	#nB	!�B	!bB	 �B	 �B	�B	=B	�B	1B	B	hB	@B	"�B��B�;B�B�BԕB��B�B�mB�KB�RBƨBɆB�<B��B��B�B�zB�B�^B˒B̘B�B�<B�vBбB�}B�vBΥB�BB��B�B�}B�}B�B�}B� BбBѷB��B��B�aB��B�
B�?B��B҉B�NB�NB��B�TBҽB� BѷB�NB��B� B�TB��B�,BԕB��B�aB��B�mBרBیBچBںBܒBݘB�;B�|B�vB�vB��B�B�B�B�&B�B�B�TB��B��B��B�B��B�mB��B�>B�
B�B�>B��B�B��B��B��B��B��B��B�B�&B�ZB��B��BޞBݘB��BݘB�jB��B�B��B�)BޞB�jBߤB�B�HB�B�BB�vB�|B�TB�B�B�>B�mB�mB�mB�B�B�KB�KB��B��B�cB�B�oB�B�B��B��B�>B�B��B	uB	�B	oB	 �B	 �B	VB	B	#B	T�B	t�B	|�B	z�B	��B	�DB
EmB
qAB
�B
��B
� B
��B
��B
��B
�B
�B
��B�BD3B�B�BBB�B&LB 'B 'B �BVB%�B)_B,qB+B:�BL0BQBVBT�BRTBK�BJ�BI�BJ�BS&B]�B_pB]�BV�B^5B;0B=B5?B7B;dB:^B>wB>B=qB<jB=<B<�B<6B:�B:�B9�B;0B<jBK�BS�BV�B\�B\�Bb�Bf�Bf2Bc�Be�Bc�Be�Bc�Bc�Bb�BcTBgmBm]Bo BiBg�Bh
Bi�Bf�BffBf2Bf2Be�Bf�Be�Bd�BdZBc�Bc�BbNBb�Ba|B`vB]dB\)B\)BZQB\�B^BdZBe�Bc BbB]dB_;Bc�BW
B[#BYBV9BV�BO�BNBS�BPBOBBMBM�BL�BLdBS[BN�BH�BFtBC-BB�BB�BB�BB�BAUB@�B@�BC�BA B?�B>�B;�B;0B7B6zB49B4B2�B3�B2-B33B-�B.�B(�B�B�BoB�B
�B;B;BBMB
��B
��B
��B
�B
��B
��B
�B
�QB
֡B
�
B
�
B
�)B
ѷB
�)B
ǮB
�9B
�gB
�aB
�'B
�B
��B
�[B
�}B
�XB
�nB
�nB
��B
��B
�=B
�7B
�=B
}�B
qAB
XEB
y	B
U�B
E�B
B�B
<�B
,�B
+6B
*�B
!bB
�B
�B
MB
B

�B

rB
.�B
�B
�B	�+B	�2B	�B	�B	�B	�B	�
B	� B	�B	� B	�B	�VB	�;B	��B
+B	ԕB	ϫB	��B	уB	�B	��B	��B	�B	ÖB	ŢB	��B	��B	�<B	��B	��B	�$B	�qB	�0B	�B	��B	�tB	�bB	��B	�-B	�'B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�tB	�CB	�oB	��B	��B	��B	�IB	�B	��B	�.B	�B	�CB	�:B	��B	��B	�7B	�rB	��B	��B	��B	�+B	�YB	�oB	�B	��B	��B	�1B	�iB	�%B	cB	}VB	�B	~�B	{�B	y>B	zB	x�B	u�B	{�B	��B	��B	�oB	�B	}�B	tTB	lWB	m�B	lWB	jB	g�B	gB	gB	iDB	v�B	ncB	d�B	c B	b�B	`�B	_�B	_�B	^�B	^jB	^B	b�B	jB	c�B	g�B	n�B	b�B	`BB	c�B	^�B	S[B	_;B	c�B	S�B	M6B	M6B	O�B	R B	FtB	B�B	F�B	E�B	AUB	<6B	YB	B�B	UgB	IB	U2B	P�B	R�B	\�B	PHB	NB	N�B	M�B	PHB	VmB	T�B	Q�B	V�B	Y�B	PHB	Q�B	PB	NpB	PHB	QB	P�B	QNB	S�B	U2B	T�B	RTB	QB	T,B	PHB	W�B	]�B	\�B	Z�B	aHB	bB	a|B	_B	bNB	b�B	c�B	c�B	c�B	b�B	d�B	e�B	h�B	iyB	k�B	l�B	o5B	n/B	s�B	u�B	y>B	z�B	}�B	��B	��B	�lB	��B	�JB	��B	��B	�qB	�eB	�B	�B	�1B	�xB	��B	�FB	�B	��B	��B	��B	xB	b�B	`�B	_�B	X�B	ZB	Y�B	[�B	]�B	\�B	\)B	Z�B	\)B	\]B	\�B	\�B	\)B	f2B	^jB	aG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                B	.�B	/�B	/&B	.TB	.�B	.�B	. B	.�B	/&B	-�B	-NB	-NB	+BB	'^B	#EB	 gB	�B	�B	B��BB��B�]B�cB�GB��B�B�|BէB�cB�AB�SB�B�{B��B׳B׳B�\B��B�B�B�@B��B	1�B	��B
��B
�B�B�B 3BA`BE�BU>B:�B5B?TB]�Ba�B^�BY"BQZBFB:�B.�B-B
�JB
�B
��B
d1B
,B
FB	�%B	ٿB	��B	�B	��B	��B	��B	��B	�LB	}�B	wqB	w=B	k\B	c�B	Z\B	^�B	XPB	J/B	?TB	L�B	I�B	MAB	JcB	L�B	V�B	[�B	`�B	k�B	nB	�B	�kB	i�B	T�B	U�B	b%B	\4B	^B	b�B	frB	k�B	p�B	ueB	{!B	~hB	�'B	�EB	��B	�mB	��B	�>B	ܝB	�B	�rB	�bB	��B	��B	�B
XB

IB
B
�B
�B
�B
-B
aB
OB
OB

�B
aB
nB
�B
XB
dB
dB
aB
$B
#B
#�B
 3B
#�B
%�B
%�B
&�B
+BB
,�B
-�B
28B
5�B
6B
7�B
?�B
E�B
BfB
B�B
F�B
LB
N�B
N|B
LoB
P�B
S1B
WB
Z�B
Z�B
\4B
\iB
^AB
^B
_�B
f	B
e�B
f	B
f	B
elB
elB
d�B
e�B
b�B
aSB
a�B
a�B
f	B
gCB
gxB
f�B
e�B
e7B
deB
deB
c�B
a�B
`�B
`�B
_{B
^uB
[�B
Z(B
YVB
Y�B
W~B
WJB
T�B
S1B
R�B
Q%B
P�B
PSB
O�B
N|B
M�B
LoB
J�B
I(B
FB
D�B
>�B
>�B
;;B
:�B
:5B
:jB
8�B
;;B
<vB
:jB
7�B
7�B
7�B
6�B
5�B
4EB
3�B
3�B
2�B
28B
28B
2mB
28B
28B
2mB
3>B
3>B
2�B
3>B
2mB
2�B
12B
/�B
/ZB
.�B
/ZB
/�B
0�B
0�B
/ZB
. B
,�B
,|B
+�B
,HB
*�B
+�B
'�B
&WB
%�B
%�B
'�B
&#B
$�B
$KB
#B
"sB
#�B
!�B
 �B
!9B
 gB
 gB
�B
�B
�B
�B
wB
�B
B
pB
<B
BB
�B
<B
<B
�B
B
�B
�B
dB
�B
0B
^B
�B
*B
#B
�B
�B
�B
B
RB
�B
LB
�B
�B
�B
tB
�B
nB
gB
[B
B
OB
�B
aB
�B
�B
aB
�B
aB
aB
�B
�B
�B
�B
�B
aB
aB
�B
3B
�B
3B
gB
�B
�B
3B
�B
�B
B
B
B
nB
�B
�B
�B
9B
tB
?B
tB
�B
�B
�B
EB
B
B
LB
B
�B
�B
RB
�B
�B
*B
�B
�B
�B
�B
*B
�B
�B
�B
�B
^B
�B
0B
�B
�B
dB
^B
�B
�B
�B
�B
dB
0B
�B
�B
jB
�B
6B
jB
B
�B
�B
�B
pB
�B
pB
<B
B
B
�B
<B
B
B
jB
dB
dB
�B
�B
0B
�B
dB
�B
�B
�B
B
�B
pB
�B
B
�B
BB
BB
BB
�B
�B
}B
}B
�B
�B
�B
�B
 B
�B
�B
&B
�B
�B
�B
�B
,B
�B
[B
�B
�B
[B
�B
 gB
 �B
!mB
!mB
"�B
#�B
%B
&#B
&�B
'�B
'�B
'�B
(dB
(dB
(dB
(dB
(�B
)jB
(�B
)5B
)jB
)B
)5B
*pB
)�B
)�B
)5B
(dB
*�B
)�B
*<B
*<B
*pB
*<B
*<B
*pB
*�B
+B
+�B
,|B
,HB
-NB
-�B
-�B
. B
/�B
/&B
/ZB
/ZB
/�B
/ZB
/�B
/ZB
/�B
0,B
0,B
0�B
0�B
0�B
12B
2�B
3>B
3>B
3
B
3>B
3
B
3�B
4yB
4�B
4�B
5KB
5KB
5B
5KB
5KB
5KB
5KB
5�B
6B
6�B
6QB
6�B
7WB
7�B
7WB
7WB
7WB
7WB
8�B
8�B
9/B
9cB
9cB
9/B
9�B
9�B
:�B
:jB
:5B
9�B
:�B
;pB
<AB
<vB
=B
=HB
=|B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>B
>NB
>B
>B
>NB
>�B
?�B
?�B
?�B
@�B
@ZB
@�B
BfB
A�B
B�B
B�B
BfB
BfB
B2B
C�B
C�B
C�B
D
B
C�B
EB
EB
EB
D�B
EDB
FB
FJB
FB
F�B
F�B
GQB
G�B
GB
G�B
GB
G�B
G�B
GQB
G�B
H�B
H�B
I(B
I(B
I(B
I�B
I�B
I�B
JcB
I�B
JcB
K�B
K�B
KiB
K5B
LB
L�B
L�B
MAB
MuB
NB
NB
NB
N�B
N�B
N�B
O�B
O�B
O�B
OMB
O�B
PSB
PSB
P�B
P�B
P�B
Q%B
P�B
QZB
Q%B
Q%B
Q%B
R`B
R`B
R�B
R�B
R�B
SfB
SfB
SfB
TB
T�B
T�B
T�B
T�B
TlB
TlB
T�B
UrB
VB
U�B
U�B
U�B
VB
V�B
VB
VB
V�B
V�B
WB
WB
WB
WJB
W�B
W�B
W�B
W�B
W�B
XB
W�B
X�B
Z(B
Y�B
YVB
Y�B
Z�B
Z�B
Z�B
[cB
[cB
[.B
[.B
[cB
[cB
[.B
[cB
\4B
[�B
[�B
\ B
\ B
\ B
\�B
\iB
\4B
]:B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^B
]�B
]�B
^B
]�B
^�B
^uB
_{B
_�B
_�B
`B
`�B
`B
aB
`�B
`�B
`�B
`�B
aB
a�B
a�B
bYB
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
c�B
d1B
d�B
d�B
d�B
deB
c�B
eB
elB
e7B
f=B
f�B
gCB
gB
gxB
gxB
g�B
g�B
hB
hB
hJB
h~B
h~B
iB
i�B
j�B
j�B
k(B
k(B
k\B
k�B
k�B
k�B
k�B
k�B
k�B
l.B
l.B
lbB
l.B
lbB
l.B
k�B
l�B
m�B
mhB
mhB
nnB
n�B
nnB
n�B
n�B
n�B
oB
oB
oB
n�B
oB
oB
o�B
pB
pFB
p�B
p�B
p�B
qB
qLB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
s$B
s�B
sYB
sYB
sYB
s�B
t_B
t�B
t�B
t�B
u1B
u1B
u1B
ueB
ueB
u1B
u�B
vkB
v7B
vkB
v�B
v�B
wB
v�B
wB
wB
w=B
w=B
wqB
w�B
w�B
w�B
w�B
w�B
xB
xB
xwB
x�B
x�B
x�B
yIB
y�B
y~B
y~B
y�B
y�B
y�B
zB
y�B
zOB
zOB
zOB
z�B
{!B
{UB
{�B
{�B
|\B
|�B
|�B
|�B
}-B
|�B
}bB
}�B
}�B
}�B
~3B
}�B
}�B
~�B
~hB
~�B
B
~�B
B
nB
:B
B
nB
nB
nB
nB
nB
�B
�B
�@B
��B
��B
�FB
�FB
�zB
��B
�B
�LB
�B
��B
��B
��B
��B
��B
��B
��B
��B
�$B
�XB
�XB
��B
��B
�*B
��B
��B
�0B
�0B
�0B
�eB
�eB
��B
��B
�6B
�B
�kB
�kB
��B
��B
�B
�<B
�<B
�<B
�B
�B
�<B
�<B
�<B
�<B
��B
��B
��B
��B
��B
��B
��B
�B
�wB
��B
�}B
�}B
�}B
�IB
�}B
�}B
�IB
��B
��B
�B
�B
�OB
�OB
��B
��B
��B
�!B
�!B
�OB	+B	+B	0�B	.�B	0`B	.�B	-�B	/ZB	28B	12B	-�B	+�B	.�B	. B	0,B	,B	. B	/ZB	/�B	. B	,|B	-B	-�B	3>B	3
B	-�B	+vB	/�B	.TB	,�B	/�B	1gB	-�B	,�B	.�B	-�B	,|B	+vB	,�B	-�B	-�B	,�B	*�B	5B	+�B	+�B	+vB	+�B	,HB	/ZB	)�B	'�B	*�B	*�B	+�B	&WB	&WB	&WB	&#B	#�B	"�B	"sB	#yB	$B	#B	 �B	 �B	"
B	"
B	!mB	,B	�B	�B	�B	&B	�B	�B	�B	�B	 B	HB	B	�B	�B	�B	�B	LB	�B	�B	B	9�B	NB߰B��B�iB�1B�GB�uB׳B�B��B�B�ZB�8B��BBB��B�,B��B�B�DB�JBƳB��B�(B�cB�/B�(B�WB��BɑB��B�/B�/B��B�/B��B�cB�iBʗBʗB�BςBмB��B�uB�;B� B� B˞B�B�oB��B�iB� BʗB��B�B̤B��B�GBΰB�BΰB�B�ZB�>B�8B�lB�DB�JB��B�.B�(B�(BڑB�4B�:B�AB��B�oB�oB�B��B��B�B�YB�B�B��B��B�B�_B��B�B�MB�{B�{B�{B��B�{BީB�:B��B�BۗBڑB�PB�JB֭B�JB�B؅B׳BէB��B�PB�B�VB��B��B�\B��B�(B�.B�B�MB�B��B�B�B�B��B��B��B��B�B�B�B�PB�!B��B�hB�uB�SB��B��B�CB�'B�\B�!B��B�OB	B	�B	�B	NGB	nnB	v�B	t�B	�`B	��B
?B
j�B
y~B
�RB
��B
ЈB
ԠB
�{B
��B
�:B
�CB�B=�B�B�B�B�BzB�B�B�BBBBaB#B&#B$�B4EBE�BJ�BO�BN|BLBE�BD�BClBDsBL�BW~BY"BWJBPSBW�B4�B6�B.�B0�B5B4B8)B7�B7#B6B6�B6QB5�B4yB4EB3�B4�B6BEyBMuBP�BV�BV�B\4B`�B_�B]:B_�B]oB_{B]�B]�B\iB]BaBgBh�Bb�Ba�Ba�Bc�B`�B`B_�B_�B_{B`MB_{B^uB^B]:B]�B\ B\�B[.BZ(BWBU�BU�BTBV�BW�B^B_�B\�B[�BWBX�B]oBP�BT�BR�BO�BPSBI]BG�BMuBI�BH�BF�BG�BFJBFBMBHWBBfB@&B<�B<�B<�B<AB<AB;B:jB:�B=HB:�B9cB8�B5B4�B0�B0,B-�B-�B,|B-NB+�B,�B'�B(�B"?BLBRB!B
}BXB
��B
��B
��B
��B
�IB
�uB
�B
�VB
�B
�_B
�cB
�B
�SB
мB
мB
��B
�iB
��B
�`B
��B
�B
�B
��B
��B
�KB
�B
�/B
�
B
� B
� B
�}B
�wB
��B
��B
��B
w=B
j�B
Q�B
r�B
OMB
?TB
<vB
6QB
&WB
$�B
$�B
B
�B
�B
�B
�B
XB
$B
(dB	��B	�bB	��B	��B	�B	�:B	�SB	��B	�B	��B	�iB	��B	�cB	�B	��B	�uB
 �B	�GB	�]B	ȋB	�5B	��B	ħB	��B	��B	�HB	�TB	�KB	B	��B	�ZB	�5B	��B	�#B	��B	��B	�UB	�&B	�B	�<B	��B	��B	�dB	��B	��B	��B	��B	�zB	�EB	�^B	��B	�&B	��B	�!B	�^B	�XB	�UB	��B	��B	�aB	��B	��B	��B	��B	�wB	��B	��B	�$B	:B	��B	|\B	��B	�B	{!B	|�B	nB	{UB	��B	zB	�B	yB	wB	yIB	xCB	u�B	r�B	s�B	r�B	o@B	ueB	�^B	|\B	{!B	z�B	w=B	nB	f	B	gCB	f	B	c�B	aSB	`�B	`�B	b�B	p{B	hB	^�B	\�B	\iB	Z\B	Y�B	YVB	X�B	XB	W�B	\4B	d1B	]oB	a�B	h~B	\iB	Y�B	]�B	XPB	MB	X�B	]�B	M�B	F�B	F�B	I]B	K�B	@&B	<AB	@�B	?TB	;B	5�B	R�B	<AB	OB	B�B	N�B	JcB	L;B	VxB	I�B	G�B	HWB	G�B	I�B	PB	NGB	K5B	P�B	SfB	I�B	K5B	I�B	H"B	I�B	J�B	J�B	K B	MAB	N�B	NGB	LB	J�B	M�B	I�B	QZB	WJB	VxB	T�B	Z�B	[�B	[.B	X�B	\ B	\4B	]:B	]�B	]:B	\iB	^uB	_GB	b�B	c+B	e7B	frB	h�B	g�B	mhB	o�B	r�B	t_B	wqB	nB	��B	�B	�^B	��B	��B	�9B	�#B	�B	��B	��B	��B	�*B	��B	��B	z�B	�LB	~3B	�IB	q�B	\iB	Z\B	Y�B	R�B	S�B	SfB	UrB	W~B	VxB	U�B	TlB	U�B	VB	V�B	VxB	U�B	_�B	XB	Z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223300                            20230426223300AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622330020230426223300  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622330020230426223300QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622330020230426223300QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               