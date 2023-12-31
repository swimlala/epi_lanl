CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:59Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230426223259  20230426223259  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�E�ۚ@�E�ۚ11  @�E�}*�@�E�}*�@)�M��a@)�M��a�cw�`k{�cw�`k{11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?u?��H@=p�@�  @�  @�  @޸RA   A\)A�RA+�A@��AaG�A���A�Q�A�  A�Q�A���A���A�  A�A��B�
B�
B  B (�B(  B0  B8(�B?�
BH  BP  BW�
B_�
Bg�
Bp  Bx  B�  B�  B�  B�{B�  B�  B�  B��B�  B�(�B�=qB�  B��B�{B��B��B�  B�{B�  B�(�B�{B�  B�  B��B�  B�{B��B��B�  B�{B�{B�  C   C
=C
=C  C  C
  C  C  C
=C
=C
=C
=C��C  C{C  C   C"{C$
=C%��C'��C*  C+��C.  C0  C2  C3��C6  C8
=C9��C;�C>  C@
=CB
=CC��CE�CH  CJ  CL  CM��CO��CR  CT  CV
=CX
=CZ
=C\{C^  C_��Cb
=Cd  Ce�Cg��Cj  Cl{Cn
=Cp  Cr
=Ct  Cv  Cx  Cy�C{��C~  C�C�  C���C���C���C��C���C�  C�  C�C���C���C���C�  C�  C���C���C�  C�  C���C���C���C�  C�C�  C�  C�C���C���C�  C�C���C�  C���C�  C�C�  C�C�C�  C�C�C�  C���C���C�  C���C���C�C�  C�  C�  C�  C�  C�C�
=C���C�  C�
=C�  C���C�  C�C�  C�  C�C�  C�C�
=C�C�  C���C�C�
=C�C�  C���C���C���C�  C�  C���C���C�  C�
=C�
=C�
=C�
=C�C�  C���C���C���C���C���C���C���C���C���C���C�  C�C�C�
=C�
=C�C�  C�C�C�
=C�\C�  C���C���C�  C�  C�  C�C�C�C�C�  C��C���C���C�  C�  C�  D   D � D�D��D�D�D�qD� D�D� D�qD��D  D� D�qD}qD�qDz�D	�D	� D
  D
��D�D� D  D}qD�D� D�qD� D��D}qD�qD� D  D}qD  D��D��D}qD�D� D  D� D  D}qD�qD� D  D}qD�D��D�qD� D  D� DD��D  D�D  D� D�D��D   D }qD!�D!� D!�qD"��D#  D#� D$  D$� D%  D%� D&�D&��D'  D'� D(  D(}qD(�qD)� D*�D*� D+  D+��D,  D,� D-  D-� D-�qD.}qD.�qD/}qD0  D0� D1�D1��D2�D2}qD3  D3� D3�qD4� D5  D5}qD6  D6��D7  D7}qD7��D8z�D8�qD9}qD:�D:��D;�D;� D<  D<� D<�qD=z�D>�D>��D?  D?}qD?�qD@��DA�DA}qDA�qDB��DC�DC��DD  DD��DE�DE��DF�DF��DG�DG� DH  DH��DI  DI� DI�qDJ}qDJ�qDK� DL�DL� DM  DM� DM�qDNz�DN�qDO}qDO�qDP� DQ  DQ� DR  DR� DS  DS� DT  DT}qDU  DU��DV  DV� DW  DW� DX  DX� DY  DY� DY�qDZ}qD[  D[��D\  D\� D\�qD]� D^�D^��D_�D_� D_�qD`� Da  Da� Db  Db� Dc  Dc� Dd�Dd� De  De� De�qDfz�Df�qDg}qDg�qDh��Di�Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl�qDm� Dm�qDn��Do  Doz�Dp  Dp� Dp�qDq� Dr  Dr��DsDs� Ds�qDt}qDu  Du��Dv  Dvz�Dw  Dw��Dx  Dx� Dx�qDy� Dz  Dz}qDz�qD{� D|�D|�D}�D}��D~�D~}qD~�qD}qD�  D�AHD�� D�� D�  D�AHD��HD���D���D�AHD��HD�� D���D�>�D�� D��HD�  D�>�D�}qD��qD�  D�>�D�~�D��HD��D�@ D�}qD��qD���D�>�D�� D���D�  D�AHD�� D�� D���D�AHD�� D���D�HD�@ D�� D�� D�  D�>�D�� D�� D��qD�=qD�~�D��HD�HD�@ D�~�D���D�  D�@ D��HD�D�HD�AHD�� D�� D���D�=qD�� D��HD�HD�@ D�~�D�� D�HD�>�D�}qD���D�  D�>�D�}qD�� D�HD�=qD�~�D�� D�  D�@ D�� D��HD�HD�@ D�}qD���D�  D�AHD��HD��HD�  D�=qD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�� D��HD�HD�@ D�~�D�� D�  D�>�D��HD�� D���D�@ D�� D�� D�HD�AHD���D�D�HD�@ D�~�D��qD�  D�AHD��HD�� D�HD�AHD��HD�� D���D�@ D�� D�D��D�>�D�~�D��HD�  D�@ D�~�D���D���D�>�D�~�D���D�  D�AHD�� D�� D�  D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD���D�D�  D�AHD��HD��HD�HD�>�D�� D�� D���D�@ D��HD�� D�HD�B�D���D�� D���D�@ D��HD�� D�HD�B�D���D�D�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�>�D�~�D���D�HD�B�D���D�D�  D�=qD�}qD���D���D�@ D��HD��HD�  D�>�D�� D�� D���D�@ D��HD�� D�  D�>�D�}qD�� D�HD�@ D�~�D��HD�HD�@ D���D�D�HD�@ D�� D�D�HD�>�D�~�D��HD��D�B�D��HD���D���D�AHD��HD�� D���D�>�D�� D��HD�  D�>�D�~�D��qD�  D�AHD�~�D�� D�HD�@ D D¾�D���D�@ DÁHD�� D���D�@ DĀ DĽqD���D�@ Dŀ D�� D�  D�>�Dƀ D�� D�  D�B�Dǂ�D�� D�  D�AHDȁHD��HD�HD�>�D�}qDɾ�D�  D�@ Dʀ Dʾ�D�HD�AHDˁHD��HD�  D�@ D̀ D̾�D���D�=qD�~�D;�D���D�AHD΁HD��HD�HD�@ Dπ D�� D�  D�@ D�~�D�� D�HD�@ Dр DѾ�D�HD�@ D�~�D��HD�HD�@ DӀ D�� D���D�>�D�~�DԾ�D�  D�@ DՀ D�� D���D�=qDր D��HD�HD�@ D�~�D׾�D�  D�AHD؁HD�D�HD�@ Dـ Dپ�D���D�AHDڀ D�� D�HD�AHDہHD��HD�HD�@ D܀ D�� D���D�=qD�~�D�� D���D�>�Dހ D޾�D�  D�@ D�~�D�� D�  D�@ D���DྸD���D�B�DႏD��HD���D�@ D� D�� D�  D�@ D�HD��HD�HD�AHD�HD��HD�HD�AHD�HD�� D�  D�@ D� D澸D���D�>�D�~�D羸D�  D�AHD�HD�D�HD�>�D�~�D�� D�HD�B�D�HD�� D�HD�B�D낏D뾸D���D�@ D�HD��HD�  D�AHD�HD�� D�  D�@ D� D�� D�HD�>�D� D��HD�  D�@ D��HD�� D�  D�@ D�~�D�D��qD�>�D�HD�� D���D�=qD�~�D�� D�  D�>�D�~�D��qD�  D�@ D�}qD��qD���D�AHD��HD���D��qD�>�D�~�D���D���D�>�D�}qD���D���D�@ D�� D���D���D�<)D���?�?W
=?�=q?�33?��?�@\)@#�
@333@G�@W
=@k�@�G�@�=q@�z�@�(�@�ff@��@��H@��
@���@�
=@�\@�@�z�A   A�A
=qA�RAz�A��A�RA$z�A(��A-p�A333A8��A>�RAB�\AG�AN{AS�
AXQ�A\��Ab�\AhQ�Amp�Aq�AvffA|��A�G�A��A�A�  A�33A�A�Q�A��\A�p�A�Q�A��HA��A��A��\A��A��A�=qA���A��A��\A��A��A�=qA��A�  A��HA�p�AǮAʏ\A�p�A�Q�A�33A�A�  Aڏ\A�p�A�Q�A��HA��A�  A��HA�A�Q�A�\A�p�A���A�33A�B   Bp�B�HBQ�B��B�RB(�B	��B
=BQ�BG�B�HB(�B��B�HB  BG�B�RB  B��B�RB�
B�B�\B   B!�B"=qB#�B$��B&ffB'�B(��B*{B+�B,��B.=qB/�B0��B1�B3\)B4��B5�B7
=B8(�B9��B;
=B<z�B=��B>�RB@(�BA��BC
=BD  BEG�BF�RBH  BIp�BJ�\BK�BL��BNffBP  BQ�BR=qBS�BT��BV=qBW�BX��BY�B[33B\z�B]�B_33B`z�Ba��Bb�HBdQ�BeBg
=Bh(�BiG�Bj�RBl(�Bm��Bn�RBo�
Bq�Br�\Bt  Bu�Bv=qBw�Bx��BzffB{�B|��B~{B\)B�ffB�
=B��B�(�B��HB���B�=qB��HB�p�B�  B��RB�p�B�{B��RB�G�B��B��\B�G�B�  B��\B�33B�B�z�B�33B��
B��\B�33B�B�ffB��B��B��\B�33B�B�ffB�
=B��
B��\B�33B�B�Q�B�
=B�B�z�B��B��B�Q�B���B��B�ffB�
=B��B�=qB���B��B�=qB���B�p�B�  B���B�G�B��B��\B�33B��B�Q�B���B�\)B�  B���B�G�B�B�Q�B���B��B�=qB��HB��B�ffB�
=B��B�Q�B�
=B��
B��\B�G�B�  B��RB�\)B�{B���B��B�=qB���B�B�z�B�G�B�{B���B��B�ffB�33B�  Bģ�B�p�B�(�B���BǙ�B�Q�B�
=BɮB�ffB�
=B˙�B�(�Ḅ�B��B͙�B��B�Q�BθRB��B�p�B��
B�=qBУ�B�
=B�\)B�B�(�Bҏ\B���B�G�BӮB�  B�ffBԸRB��B�p�B��
B�(�B֣�B���B�p�B��
B�=qBأ�B���B�G�BٮB�  B�ffB���B�33Bۙ�B�  B�ffB��HB�33BݮB�{B�z�B��HB�G�B߮B�{B�z�B��HB�33BᙚB�  B�ffB���B�G�B㙚B�{B�z�B��HB�\)B�B�(�B��B�
=B�p�B��B�Q�B���B�33B陚B�  B�z�B��HB�G�B�B�(�B�\B�
=B�B��
B�=qB�RB��B�B�  B�Q�B���B�33B�B�  B�z�B��HB�G�B�B�(�B�\B���B�\)B��
B�=qB���B��B��B��B�Q�B��RB��B���B��B�Q�B���B�33B���B�  B�ffB���B�G�B��B�{B�z�B��HB�33B��C 
=C =qC p�C ��C �
C
=C=qCp�C��C�
C  C=qCffC��C��C  C33CffC�\C��C��C(�C\)C�C�RC�C�CQ�C�C�RC�C�CQ�C�C�RC�HC{CG�Cp�C��C�
C
=C33CffC��C��C	  C	33C	ffC	��C	�
C

=C
=qC
z�C
�C
�HC{C=qCp�C��C��C  C=qCp�C��C�
C{CG�C�C�RC�C(�C\)C�\C��C��C33CffC��C��C
=CG�C�CC��C=qCz�C�RC�C(�C\)C��C�
C{C\)C��C�
C
=CG�Cz�C�RC��C33Cz�C�RC  C=qCp�C�C�C(�CffC�C��C(�CffC��C�HC�C\)C��C�
C(�CffC��C�HC�C\)C�\C�
C�C\)C��C�C33Cp�C�C�C(�Cp�C�RC  CG�C�\C��C {C G�C �\C �HC!(�C!p�C!�C!��C"33C"z�C"C#  C#Q�C#��C#�HC$(�C$\)C$��C$�C%33C%z�C%C&{C&Q�C&��C&�
C'{C'ffC'�C(  C(=qC(z�C(C)
=C)Q�C)��C)�HC*(�C*p�C*�RC*��C+33C+z�C+C,{C,\)C,��C,�
C-{C-\)C-�C-��C.33C.p�C.�C.��C/G�C/�\C/�
C0{C0Q�C0��C0�HC133C1z�C1�RC1��C2=qC2z�C2��C3�C3ffC3��C3�HC4(�C4p�C4C5
=C5Q�C5�C5��C6(�C6p�C6�RC6��C7=qC7�C7��C8�C8ffC8�RC9  C9G�C9�\C9��C:{C:ffC:�RC;
=C;Q�C;��C;�
C<�C<ffC<�RC=
=C=\)C=��C=�C>33C>p�C>��C?�C?p�C?�C?��C@=qC@�\C@�HCA(�CAp�CA�CA��CB33CB�\CB�
CC{CC\)CC�CD  CDQ�CD�\CD�HCE�CEffCE�RCF
=CFQ�CF��CF�
CG(�CGz�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                         ?u?��H@=p�@�  @�  @�  @޸RA   A\)A�RA+�A@��AaG�A���A�Q�A�  A�Q�A���A���A�  A�A��B�
B�
B  B (�B(  B0  B8(�B?�
BH  BP  BW�
B_�
Bg�
Bp  Bx  B�  B�  B�  B�{B�  B�  B�  B��B�  B�(�B�=qB�  B��B�{B��B��B�  B�{B�  B�(�B�{B�  B�  B��B�  B�{B��B��B�  B�{B�{B�  C   C
=C
=C  C  C
  C  C  C
=C
=C
=C
=C��C  C{C  C   C"{C$
=C%��C'��C*  C+��C.  C0  C2  C3��C6  C8
=C9��C;�C>  C@
=CB
=CC��CE�CH  CJ  CL  CM��CO��CR  CT  CV
=CX
=CZ
=C\{C^  C_��Cb
=Cd  Ce�Cg��Cj  Cl{Cn
=Cp  Cr
=Ct  Cv  Cx  Cy�C{��C~  C�C�  C���C���C���C��C���C�  C�  C�C���C���C���C�  C�  C���C���C�  C�  C���C���C���C�  C�C�  C�  C�C���C���C�  C�C���C�  C���C�  C�C�  C�C�C�  C�C�C�  C���C���C�  C���C���C�C�  C�  C�  C�  C�  C�C�
=C���C�  C�
=C�  C���C�  C�C�  C�  C�C�  C�C�
=C�C�  C���C�C�
=C�C�  C���C���C���C�  C�  C���C���C�  C�
=C�
=C�
=C�
=C�C�  C���C���C���C���C���C���C���C���C���C���C�  C�C�C�
=C�
=C�C�  C�C�C�
=C�\C�  C���C���C�  C�  C�  C�C�C�C�C�  C��C���C���C�  C�  C�  D   D � D�D��D�D�D�qD� D�D� D�qD��D  D� D�qD}qD�qDz�D	�D	� D
  D
��D�D� D  D}qD�D� D�qD� D��D}qD�qD� D  D}qD  D��D��D}qD�D� D  D� D  D}qD�qD� D  D}qD�D��D�qD� D  D� DD��D  D�D  D� D�D��D   D }qD!�D!� D!�qD"��D#  D#� D$  D$� D%  D%� D&�D&��D'  D'� D(  D(}qD(�qD)� D*�D*� D+  D+��D,  D,� D-  D-� D-�qD.}qD.�qD/}qD0  D0� D1�D1��D2�D2}qD3  D3� D3�qD4� D5  D5}qD6  D6��D7  D7}qD7��D8z�D8�qD9}qD:�D:��D;�D;� D<  D<� D<�qD=z�D>�D>��D?  D?}qD?�qD@��DA�DA}qDA�qDB��DC�DC��DD  DD��DE�DE��DF�DF��DG�DG� DH  DH��DI  DI� DI�qDJ}qDJ�qDK� DL�DL� DM  DM� DM�qDNz�DN�qDO}qDO�qDP� DQ  DQ� DR  DR� DS  DS� DT  DT}qDU  DU��DV  DV� DW  DW� DX  DX� DY  DY� DY�qDZ}qD[  D[��D\  D\� D\�qD]� D^�D^��D_�D_� D_�qD`� Da  Da� Db  Db� Dc  Dc� Dd�Dd� De  De� De�qDfz�Df�qDg}qDg�qDh��Di�Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl�qDm� Dm�qDn��Do  Doz�Dp  Dp� Dp�qDq� Dr  Dr��DsDs� Ds�qDt}qDu  Du��Dv  Dvz�Dw  Dw��Dx  Dx� Dx�qDy� Dz  Dz}qDz�qD{� D|�D|�D}�D}��D~�D~}qD~�qD}qD�  D�AHD�� D�� D�  D�AHD��HD���D���D�AHD��HD�� D���D�>�D�� D��HD�  D�>�D�}qD��qD�  D�>�D�~�D��HD��D�@ D�}qD��qD���D�>�D�� D���D�  D�AHD�� D�� D���D�AHD�� D���D�HD�@ D�� D�� D�  D�>�D�� D�� D��qD�=qD�~�D��HD�HD�@ D�~�D���D�  D�@ D��HD�D�HD�AHD�� D�� D���D�=qD�� D��HD�HD�@ D�~�D�� D�HD�>�D�}qD���D�  D�>�D�}qD�� D�HD�=qD�~�D�� D�  D�@ D�� D��HD�HD�@ D�}qD���D�  D�AHD��HD��HD�  D�=qD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�� D��HD�HD�@ D�~�D�� D�  D�>�D��HD�� D���D�@ D�� D�� D�HD�AHD���D�D�HD�@ D�~�D��qD�  D�AHD��HD�� D�HD�AHD��HD�� D���D�@ D�� D�D��D�>�D�~�D��HD�  D�@ D�~�D���D���D�>�D�~�D���D�  D�AHD�� D�� D�  D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD���D�D�  D�AHD��HD��HD�HD�>�D�� D�� D���D�@ D��HD�� D�HD�B�D���D�� D���D�@ D��HD�� D�HD�B�D���D�D�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�>�D�~�D���D�HD�B�D���D�D�  D�=qD�}qD���D���D�@ D��HD��HD�  D�>�D�� D�� D���D�@ D��HD�� D�  D�>�D�}qD�� D�HD�@ D�~�D��HD�HD�@ D���D�D�HD�@ D�� D�D�HD�>�D�~�D��HD��D�B�D��HD���D���D�AHD��HD�� D���D�>�D�� D��HD�  D�>�D�~�D��qD�  D�AHD�~�D�� D�HD�@ D D¾�D���D�@ DÁHD�� D���D�@ DĀ DĽqD���D�@ Dŀ D�� D�  D�>�Dƀ D�� D�  D�B�Dǂ�D�� D�  D�AHDȁHD��HD�HD�>�D�}qDɾ�D�  D�@ Dʀ Dʾ�D�HD�AHDˁHD��HD�  D�@ D̀ D̾�D���D�=qD�~�D;�D���D�AHD΁HD��HD�HD�@ Dπ D�� D�  D�@ D�~�D�� D�HD�@ Dр DѾ�D�HD�@ D�~�D��HD�HD�@ DӀ D�� D���D�>�D�~�DԾ�D�  D�@ DՀ D�� D���D�=qDր D��HD�HD�@ D�~�D׾�D�  D�AHD؁HD�D�HD�@ Dـ Dپ�D���D�AHDڀ D�� D�HD�AHDہHD��HD�HD�@ D܀ D�� D���D�=qD�~�D�� D���D�>�Dހ D޾�D�  D�@ D�~�D�� D�  D�@ D���DྸD���D�B�DႏD��HD���D�@ D� D�� D�  D�@ D�HD��HD�HD�AHD�HD��HD�HD�AHD�HD�� D�  D�@ D� D澸D���D�>�D�~�D羸D�  D�AHD�HD�D�HD�>�D�~�D�� D�HD�B�D�HD�� D�HD�B�D낏D뾸D���D�@ D�HD��HD�  D�AHD�HD�� D�  D�@ D� D�� D�HD�>�D� D��HD�  D�@ D��HD�� D�  D�@ D�~�D�D��qD�>�D�HD�� D���D�=qD�~�D�� D�  D�>�D�~�D��qD�  D�@ D�}qD��qD���D�AHD��HD���D��qD�>�D�~�D���D���D�>�D�}qD���D���D�@ D�� D���D���D�<)D���?�?W
=?�=q?�33?��?�@\)@#�
@333@G�@W
=@k�@�G�@�=q@�z�@�(�@�ff@��@��H@��
@���@�
=@�\@�@�z�A   A�A
=qA�RAz�A��A�RA$z�A(��A-p�A333A8��A>�RAB�\AG�AN{AS�
AXQ�A\��Ab�\AhQ�Amp�Aq�AvffA|��A�G�A��A�A�  A�33A�A�Q�A��\A�p�A�Q�A��HA��A��A��\A��A��A�=qA���A��A��\A��A��A�=qA��A�  A��HA�p�AǮAʏ\A�p�A�Q�A�33A�A�  Aڏ\A�p�A�Q�A��HA��A�  A��HA�A�Q�A�\A�p�A���A�33A�B   Bp�B�HBQ�B��B�RB(�B	��B
=BQ�BG�B�HB(�B��B�HB  BG�B�RB  B��B�RB�
B�B�\B   B!�B"=qB#�B$��B&ffB'�B(��B*{B+�B,��B.=qB/�B0��B1�B3\)B4��B5�B7
=B8(�B9��B;
=B<z�B=��B>�RB@(�BA��BC
=BD  BEG�BF�RBH  BIp�BJ�\BK�BL��BNffBP  BQ�BR=qBS�BT��BV=qBW�BX��BY�B[33B\z�B]�B_33B`z�Ba��Bb�HBdQ�BeBg
=Bh(�BiG�Bj�RBl(�Bm��Bn�RBo�
Bq�Br�\Bt  Bu�Bv=qBw�Bx��BzffB{�B|��B~{B\)B�ffB�
=B��B�(�B��HB���B�=qB��HB�p�B�  B��RB�p�B�{B��RB�G�B��B��\B�G�B�  B��\B�33B�B�z�B�33B��
B��\B�33B�B�ffB��B��B��\B�33B�B�ffB�
=B��
B��\B�33B�B�Q�B�
=B�B�z�B��B��B�Q�B���B��B�ffB�
=B��B�=qB���B��B�=qB���B�p�B�  B���B�G�B��B��\B�33B��B�Q�B���B�\)B�  B���B�G�B�B�Q�B���B��B�=qB��HB��B�ffB�
=B��B�Q�B�
=B��
B��\B�G�B�  B��RB�\)B�{B���B��B�=qB���B�B�z�B�G�B�{B���B��B�ffB�33B�  Bģ�B�p�B�(�B���BǙ�B�Q�B�
=BɮB�ffB�
=B˙�B�(�Ḅ�B��B͙�B��B�Q�BθRB��B�p�B��
B�=qBУ�B�
=B�\)B�B�(�Bҏ\B���B�G�BӮB�  B�ffBԸRB��B�p�B��
B�(�B֣�B���B�p�B��
B�=qBأ�B���B�G�BٮB�  B�ffB���B�33Bۙ�B�  B�ffB��HB�33BݮB�{B�z�B��HB�G�B߮B�{B�z�B��HB�33BᙚB�  B�ffB���B�G�B㙚B�{B�z�B��HB�\)B�B�(�B��B�
=B�p�B��B�Q�B���B�33B陚B�  B�z�B��HB�G�B�B�(�B�\B�
=B�B��
B�=qB�RB��B�B�  B�Q�B���B�33B�B�  B�z�B��HB�G�B�B�(�B�\B���B�\)B��
B�=qB���B��B��B��B�Q�B��RB��B���B��B�Q�B���B�33B���B�  B�ffB���B�G�B��B�{B�z�B��HB�33B��C 
=C =qC p�C ��C �
C
=C=qCp�C��C�
C  C=qCffC��C��C  C33CffC�\C��C��C(�C\)C�C�RC�C�CQ�C�C�RC�C�CQ�C�C�RC�HC{CG�Cp�C��C�
C
=C33CffC��C��C	  C	33C	ffC	��C	�
C

=C
=qC
z�C
�C
�HC{C=qCp�C��C��C  C=qCp�C��C�
C{CG�C�C�RC�C(�C\)C�\C��C��C33CffC��C��C
=CG�C�CC��C=qCz�C�RC�C(�C\)C��C�
C{C\)C��C�
C
=CG�Cz�C�RC��C33Cz�C�RC  C=qCp�C�C�C(�CffC�C��C(�CffC��C�HC�C\)C��C�
C(�CffC��C�HC�C\)C�\C�
C�C\)C��C�C33Cp�C�C�C(�Cp�C�RC  CG�C�\C��C {C G�C �\C �HC!(�C!p�C!�C!��C"33C"z�C"C#  C#Q�C#��C#�HC$(�C$\)C$��C$�C%33C%z�C%C&{C&Q�C&��C&�
C'{C'ffC'�C(  C(=qC(z�C(C)
=C)Q�C)��C)�HC*(�C*p�C*�RC*��C+33C+z�C+C,{C,\)C,��C,�
C-{C-\)C-�C-��C.33C.p�C.�C.��C/G�C/�\C/�
C0{C0Q�C0��C0�HC133C1z�C1�RC1��C2=qC2z�C2��C3�C3ffC3��C3�HC4(�C4p�C4C5
=C5Q�C5�C5��C6(�C6p�C6�RC6��C7=qC7�C7��C8�C8ffC8�RC9  C9G�C9�\C9��C:{C:ffC:�RC;
=C;Q�C;��C;�
C<�C<ffC<�RC=
=C=\)C=��C=�C>33C>p�C>��C?�C?p�C?�C?��C@=qC@�\C@�HCA(�CAp�CA�CA��CB33CB�\CB�
CC{CC\)CC�CD  CDQ�CD�\CD�HCE�CEffCE�RCF
=CFQ�CF��CF�
CG(�CGz�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                         G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AͰ!AͰ!AͬAͬA͙�A͙�A͓uA͇+AͅÁA�hsA�ZA�O�A�K�A�M�A�K�A�I�A�9XA�33A�/A�/A�-A�$�A��A��A��A�"�A��A��A�{A�bA�VA�JA�JA�
=A�
=A�%A�A�  A���A���A���A���A���A��A��A��`A���A̟�Aș�A�K�A���A���A�r�A�dZA��-A�x�A���A�ZA��
A���A�JA��PA�hsA�VA��A�r�A��hA�v�A��A�=qA��`A���A���A�1'A� �A�&�A�I�A�  A��;A���A��+A���Ax�Au
=Ao�#Aj�HAi"�Ae�A^ffAX��AT��AMƨAJ�\AH5?AB��A@�!A?dZA>=qA<�/A:  A77LA6-A6VA4ZA2�/A2��A2�A1p�A1S�A1t�A1x�A1XA1oA0r�A/33A.�yA.9XA,��A+�A*�A(�A(r�A'�;A&�yA%�A%O�A$�`A$r�A#��A#�A"VA!��A!%A ȴA ��A M�A�PAO�A7LA��AĜAM�A�FA�yA��AffAAC�A~�A�A�PAO�A+A��Ar�A�AAz�A�A�hA/AA�RAffA�hA
=A��Az�A �A�^A��AG�A+A�A�A�+A �A�TA��AO�A7LA�yA�AQ�A=qA9XAE�A1AƨA��A�^A��A�hA�A|�AdZA33A��A�+A(�A�#AAAA��At�AS�AG�A/A%A
�yA
�jA
�A
n�A
jA
VA
1'A
�A
�A
�A
�A
�A	�
A	+A�A��A=qA1A�wA�AG�A�\A=qA�A�
A��At�AdZAO�A"�A��AVA�wAl�AG�AoA��A��AbNA(�A�TAt�A�A �A �9A �+A Q�A �@��P@�\)@��y@�@���@�Q�@���@��@�o@�`B@��@�z�@�bN@�(�@�|�@��!@�-@���@��h@��@��u@�+@�~�@�5?@�%@��
@�^5@�V@�u@�
=@�!@�-@���@��@��;@��@��@�"�@⟾@��@��T@�X@���@��u@߅@ް!@���@܃@�I�@��@�;d@�V@�%@�Z@�ƨ@׍P@�33@��@�$�@ՙ�@���@ӥ�@�C�@ҟ�@��#@�z�@�K�@�"�@���@�ȴ@�5?@�G�@�b@�o@��y@�ȴ@ʰ!@�v�@�hs@ȋD@�1'@��;@ǥ�@�^5@š�@�G�@���@ċD@� �@�ƨ@Å@��@�@�^5@�E�@���@��h@�O�@��9@��@���@�ƨ@��F@���@�C�@���@�~�@�M�@�@��#@��^@�7L@�r�@���@��F@�|�@�C�@�ȴ@�$�@��T@�x�@��@��@��D@�9X@���@�dZ@�"�@��@�$�@�G�@��`@���@���@��j@���@��D@��@�r�@�b@��@�K�@��@��H@�ff@��@���@��^@�7L@���@�A�@���@�dZ@�o@���@��#@��@�/@�r�@��@�l�@�;d@���@��!@�n�@�5?@��#@��^@��7@�X@�/@��@���@��9@�9X@���@�l�@�l�@�;d@���@�J@���@��7@�O�@��@��@�r�@���@�l�@���@���@���@�E�@��@���@�?}@�V@���@��m@���@�K�@��@�ȴ@�ff@�{@��-@���@�`B@��9@�I�@� �@�1@��;@���@�C�@��H@���@�J@��-@���@�`B@��@��9@�I�@�b@���@�ȴ@��+@�n�@��@�I�@�1'@�b@��w@�t�@�"�@���@��!@�-@��@���@��7@�X@��@���@��@�r�@�I�@�1'@�b@��m@�\)@�33@���@���@���@���@��7@�7L@��@�r�@��@�t�@��\@�E�@�{@���@�p�@�X@�7L@�V@���@��j@��@���@�Z@�ƨ@��@�dZ@�K�@�33@��@�@�ȴ@�v�@��@��-@�G�@�V@���@�r�@�(�@��
@�t�@�dZ@��@��H@��!@�v�@�5?@�J@��#@���@�x�@�G�@�V@��@��`@��/@��j@���@�j@�Q�@�9X@�b@�P@�@~E�@}�-@}O�@|�/@|I�@|�@{�F@{dZ@z�@z�!@z�\@z~�@z-@y�#@y�^@yX@x��@w|�@w+@v��@v5?@t��@s�m@sC�@so@r�H@r��@r-@q�@p1'@o�@n��@n��@nV@n@m��@m�-@m�@l�@l�D@l1@k�@j��@jJ@i��@iX@i�@h  @g�;@gK�@e�@e@e��@e?}@d�@d(�@c��@ct�@ct�@cS�@c"�@b��@bJ@a&�@`�`@`�@_��@_K�@^ȴ@^{@]p�@]�@\��@\�/@\�j@\��@\�D@\j@\Z@\(�@[�m@[dZ@["�@[o@Z�H@Z�!@Z~�@Z^5@ZM�@Z-@Y��@Y�#@Y��@Y��@Y7L@X�`@XQ�@Xb@X  @W�@W+@V�y@V{@U�-@U�@Up�@U`B@U?}@T��@Tj@T1@SdZ@R�@R��@R�@Q�^@Q&�@P��@P�9@P �@O�@O\)@N��@N�R@Nv�@N$�@M�T@M�h@L�/@Lz�@L(�@Kƨ@K"�@I�#@I��@I�7@I�7@Ihs@I7L@H�u@HQ�@H �@H  @G�w@G|�@G�@F�@FV@F{@E/@D�@Dz�@DZ@DZ@DI�@D�@C�m@C�F@CdZ@C"�@B�@B�!@B~�@B^5@B�@A��@Ax�@@��@@�9@@1'@@  @?�;@?�P@>�y@>�R@>��@>E�@>$�@=�-@=/@<�/@<�@<j@;dZ@;@:�@:��@:��@:~�@:=q@:�@9�@9��@9�^@9��@9x�@9�@8�9@8�@81'@8b@7�;@7\)@6�y@6$�@5��@5?}@4�/@4��@4��@49X@41@3��@3dZ@2�H@2-@1�@1hs@0��@0  @/��@/l�@.ȴ@.��@.v�@.E�@-�h@-/@,z�@,�@,1@,1@+ƨ@+dZ@+dZ@+S�@+S�@+C�@*��@*~�@*n�@*�@*J@)�#@)�7@)&�@(�`@(Q�@'��@'\)@'�@'
=@&�y@&�y@&�y@&�@&�R@&ff@%p�@%O�@%/@%V@$��@$��@$�@$�/@$��@$�@$z�@#��@#dZ@#@"�\@"M�@"-@"�@!�#@!X@!&�@ �`@ �@ bN@ A�@�;@��@�P@l�@\)@
=@��@��@�y@ȴ@v�@ff@��@��@�@/@�j@��@Z@ƨ@t�@C�@o@�@��@n�@�@��@X@�@��@�u@bN@bN@Q�@1'@�;@�;@�@�P@
=@ff@E�@5?@5?@5?@$�@�@�-@�h@�h@�h@p�@/@�/@�D@�D@z�@9X@ƨ@��@t�@dZ@S�@33@"�@�H@��@��@M�@-@�@J@�@��@hs@�@��@�9@�u@r�@Q�@ �@b@�@�@�@�P@|�@l�@;d@+@�@��@�@��@ff@{@��@��@��@��@��@�@p�@O�@?}@/@/@/@�@V@V@��@�@�@��@I�@(�@�@��@ƨ@��@�@t�@S�@C�@33@
�@
��@
��@
n�@
=q@
-@
-@
�@
-@
-@
-@
-@
=q@
=q@
=q@	�^@	hs@	G�@	�@	%@��AͮAͲ-Aʹ9AͲ-AͮAͰ!AͰ!AͰ!AͬAͩ�Aͧ�AͮAͰ!AͬA͗�A͗�A͝�A͝�A͗�A͕�A͛�A͝�A͏\A͇+AͅA͋DA͇+AͅA̓ÁAͅA͇+A�|�A�n�A�`BA�ffA�hsA�ZA�M�A�Q�A�VA�S�A�S�A�K�A�K�A�M�A�I�A�K�A�K�A�M�A�K�A�G�A�G�A�K�A�M�A�M�A�M�A�O�A�O�A�O�A�K�A�M�A�I�A�K�A�K�A�G�A�I�A�K�A�O�A�M�A�C�A�;dA�;dA�;dA�;dA�=qA�9XA�1'A�5?A�7LA�9XA�5?A�-A�-A�-A�5?A�1'A�/A�-A�/A�33A�1'A�-A�+A�-A�33A�/A�-A�+A�+A�-A�(�A�"�A�"�A�$�A�&�A�"�A��A��A� �A�"�A��A��A��A��A� �A� �A��A��A��A��A� �A��A��A��A� �A�"�A�"�A� �A� �A�"�A�$�A�$�A� �A��A��A��A� �A��A��A��A��A��A��A��A�oA�oA�{A�{A�oA�VA�bA�oA�oA�oA�VA�VA�oA�oA�bA�VA�
=A�
=A�VA�VA�JA�1A�1A�
=A�VA�VA�VA�
=A�1A�
=A�JA�JA�
=A�1A�
=A�JA�JA�
=A�%A�%A�
=A�JA�
=A�%A�%A�1A�1A�1A�A�A�  A�A�A�A�  A�  A�A�A�A�  A���A���A�  A�  A���A���A���A���A�  A�A���A���A���A���A�  A�A�  A���A���A���A���A�  A�  A���A���A���A���A���A���A���A��A��A��A���A���A��A��A��A��A��A��A��A��yA��mA��yA��A��A��mA��TA��/A��/A��#A��/A��/A��
A���A̼jA̲-A̮A̧�Ḁ�A̡�Ȧ+A�~�A�C�A��;A�  A��AƑhA��A��`A��\A��yA�A���A���A�XA��yA�`BA�%A�M�A��A�bA�I�A�Q�A��A��TA��
A�(�A�\)A�I�A���A�I�A���A�=qA��A��yA���A�A�ƨA��A�?}A�JA�ĜA���A��A�ffA�{A���A�n�A�E�A��A��A��A��A��;A���A��jA��A���A���A��hA�~�A�v�A�p�A�ffA�Q�A�I�A�A�A�9XA�1'A�+A��A�{A�1A���A��A��;A�ĜA���A��7A�x�A�hsA�Q�A�9XA� �A��A�VA��A���A���A�hsA�VA�A�A�+A��A�VA�A���A��A��A��yA��`A��
A�ƨA���A��RA���A��DA�dZA�?}A�$�A���A��#A��wA���A��hA�n�A�(�A���A��FA�z�A�E�A�(�A��A�A���A��A��yA��HA��
A�ȴA��9A���A��PA�z�A�l�A�\)A�Q�A�A�A�&�A�A��HA��FA���A�r�A�E�A�$�A���A��jA��PA�\)A�bA��wA��A�S�A�/A�oA�A���A��A��mA���A��^A��uA�hsA�5?A���A��RA�r�A�C�A���A��wA�~�A�XA�G�A�/A�
=A��-A�\)A�%A���A��hA�M�A�bA��A��/A���A��7A�dZA�M�A�33A���A�-A���A�v�A�=qA��TA��!A���A�~�A�l�A�O�A�+A��A��-A��PA�l�A�33A�  A��^A�dZA��A�z�A��A��RA�hsA�"�A���A���A�p�A�7LA��#A��hA�I�A���A�ĜA��\A�dZA�bNA�dZA�S�A�I�A�E�A�=qA�5?A�"�A�%A���A��^A��A�t�A� �A�ƨA���A�Q�A�ffA��A���A��DA�bA�A��FA���A��uA�\)A��`A�I�A�`BA���A�`BA�1'A�1A��A���A��PA�bNA�?}A� �A�
=A��A��/A��
A��jA��DA��A��A�z�A�K�A��A��`A���A�|�A�{A���A���A��!A��DA�x�A�VA�(�A�A��A��RA���A�G�A��;A��PA�A}�-A|�A|  Az$�Ax��Ax$�Awp�Av�/Av�DAvE�AvbAu�AtĜAt�\AtjAt=qAs�^ArVAq+Ap��Ap  An�Am7LAl�Aln�Al �Ak�wAj��AjI�Aj  Ai�mAi�^Ai�PAil�AiG�Ai+Ah�Ah��Ahz�Ah�Ag�-Af��Ad�yAc�7AbJA`��A`{A_\)A^�`A^��A^A�A]��A\I�AZ�!AY�AXI�AX5?AX1'AX-AX �AW�AWC�AV~�AU��AT��ATI�ARr�AP��AO�^AN�`ANbAMhsAM�AM%AL��AL(�AK�AKt�AJ�AJE�AI�;AI�AI`BAI+AH�`AH�AH^5AH  AG�AF��AE�
ACC�ABȴABQ�AB�AB �ABAA��AA\)AAoA@�/A@�+A@=qA@JA?�mA?��A?��A?t�A?K�A?7LA?�A?VA>�A>��A>M�A>�A=��A=�A=�A=XA=/A<��A<��A<�DA<-A;��A;dZA:ĜA:JA9l�A8�yA8�uA85?A7�;A7�7A7C�A6�A6�A6�+A6ffA6A�A6-A6(�A6�A6$�A6 �A6bA6(�A6jA6�!A6��A6VA5��A5&�A4�A4�`A4�jA3�TA3�A2�A2�`A2�/A2�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                         AͰ!AͰ!AͬAͬA͙�A͙�A͓uA͇+AͅÁA�hsA�ZA�O�A�K�A�M�A�K�A�I�A�9XA�33A�/A�/A�-A�$�A��A��A��A�"�A��A��A�{A�bA�VA�JA�JA�
=A�
=A�%A�A�  A���A���A���A���A���A��A��A��`A���A̟�Aș�A�K�A���A���A�r�A�dZA��-A�x�A���A�ZA��
A���A�JA��PA�hsA�VA��A�r�A��hA�v�A��A�=qA��`A���A���A�1'A� �A�&�A�I�A�  A��;A���A��+A���Ax�Au
=Ao�#Aj�HAi"�Ae�A^ffAX��AT��AMƨAJ�\AH5?AB��A@�!A?dZA>=qA<�/A:  A77LA6-A6VA4ZA2�/A2��A2�A1p�A1S�A1t�A1x�A1XA1oA0r�A/33A.�yA.9XA,��A+�A*�A(�A(r�A'�;A&�yA%�A%O�A$�`A$r�A#��A#�A"VA!��A!%A ȴA ��A M�A�PAO�A7LA��AĜAM�A�FA�yA��AffAAC�A~�A�A�PAO�A+A��Ar�A�AAz�A�A�hA/AA�RAffA�hA
=A��Az�A �A�^A��AG�A+A�A�A�+A �A�TA��AO�A7LA�yA�AQ�A=qA9XAE�A1AƨA��A�^A��A�hA�A|�AdZA33A��A�+A(�A�#AAAA��At�AS�AG�A/A%A
�yA
�jA
�A
n�A
jA
VA
1'A
�A
�A
�A
�A
�A	�
A	+A�A��A=qA1A�wA�AG�A�\A=qA�A�
A��At�AdZAO�A"�A��AVA�wAl�AG�AoA��A��AbNA(�A�TAt�A�A �A �9A �+A Q�A �@��P@�\)@��y@�@���@�Q�@���@��@�o@�`B@��@�z�@�bN@�(�@�|�@��!@�-@���@��h@��@��u@�+@�~�@�5?@�%@��
@�^5@�V@�u@�
=@�!@�-@���@��@��;@��@��@�"�@⟾@��@��T@�X@���@��u@߅@ް!@���@܃@�I�@��@�;d@�V@�%@�Z@�ƨ@׍P@�33@��@�$�@ՙ�@���@ӥ�@�C�@ҟ�@��#@�z�@�K�@�"�@���@�ȴ@�5?@�G�@�b@�o@��y@�ȴ@ʰ!@�v�@�hs@ȋD@�1'@��;@ǥ�@�^5@š�@�G�@���@ċD@� �@�ƨ@Å@��@�@�^5@�E�@���@��h@�O�@��9@��@���@�ƨ@��F@���@�C�@���@�~�@�M�@�@��#@��^@�7L@�r�@���@��F@�|�@�C�@�ȴ@�$�@��T@�x�@��@��@��D@�9X@���@�dZ@�"�@��@�$�@�G�@��`@���@���@��j@���@��D@��@�r�@�b@��@�K�@��@��H@�ff@��@���@��^@�7L@���@�A�@���@�dZ@�o@���@��#@��@�/@�r�@��@�l�@�;d@���@��!@�n�@�5?@��#@��^@��7@�X@�/@��@���@��9@�9X@���@�l�@�l�@�;d@���@�J@���@��7@�O�@��@��@�r�@���@�l�@���@���@���@�E�@��@���@�?}@�V@���@��m@���@�K�@��@�ȴ@�ff@�{@��-@���@�`B@��9@�I�@� �@�1@��;@���@�C�@��H@���@�J@��-@���@�`B@��@��9@�I�@�b@���@�ȴ@��+@�n�@��@�I�@�1'@�b@��w@�t�@�"�@���@��!@�-@��@���@��7@�X@��@���@��@�r�@�I�@�1'@�b@��m@�\)@�33@���@���@���@���@��7@�7L@��@�r�@��@�t�@��\@�E�@�{@���@�p�@�X@�7L@�V@���@��j@��@���@�Z@�ƨ@��@�dZ@�K�@�33@��@�@�ȴ@�v�@��@��-@�G�@�V@���@�r�@�(�@��
@�t�@�dZ@��@��H@��!@�v�@�5?@�J@��#@���@�x�@�G�@�V@��@��`@��/@��j@���@�j@�Q�@�9X@�b@�P@�@~E�@}�-@}O�@|�/@|I�@|�@{�F@{dZ@z�@z�!@z�\@z~�@z-@y�#@y�^@yX@x��@w|�@w+@v��@v5?@t��@s�m@sC�@so@r�H@r��@r-@q�@p1'@o�@n��@n��@nV@n@m��@m�-@m�@l�@l�D@l1@k�@j��@jJ@i��@iX@i�@h  @g�;@gK�@e�@e@e��@e?}@d�@d(�@c��@ct�@ct�@cS�@c"�@b��@bJ@a&�@`�`@`�@_��@_K�@^ȴ@^{@]p�@]�@\��@\�/@\�j@\��@\�D@\j@\Z@\(�@[�m@[dZ@["�@[o@Z�H@Z�!@Z~�@Z^5@ZM�@Z-@Y��@Y�#@Y��@Y��@Y7L@X�`@XQ�@Xb@X  @W�@W+@V�y@V{@U�-@U�@Up�@U`B@U?}@T��@Tj@T1@SdZ@R�@R��@R�@Q�^@Q&�@P��@P�9@P �@O�@O\)@N��@N�R@Nv�@N$�@M�T@M�h@L�/@Lz�@L(�@Kƨ@K"�@I�#@I��@I�7@I�7@Ihs@I7L@H�u@HQ�@H �@H  @G�w@G|�@G�@F�@FV@F{@E/@D�@Dz�@DZ@DZ@DI�@D�@C�m@C�F@CdZ@C"�@B�@B�!@B~�@B^5@B�@A��@Ax�@@��@@�9@@1'@@  @?�;@?�P@>�y@>�R@>��@>E�@>$�@=�-@=/@<�/@<�@<j@;dZ@;@:�@:��@:��@:~�@:=q@:�@9�@9��@9�^@9��@9x�@9�@8�9@8�@81'@8b@7�;@7\)@6�y@6$�@5��@5?}@4�/@4��@4��@49X@41@3��@3dZ@2�H@2-@1�@1hs@0��@0  @/��@/l�@.ȴ@.��@.v�@.E�@-�h@-/@,z�@,�@,1@,1@+ƨ@+dZ@+dZ@+S�@+S�@+C�@*��@*~�@*n�@*�@*J@)�#@)�7@)&�@(�`@(Q�@'��@'\)@'�@'
=@&�y@&�y@&�y@&�@&�R@&ff@%p�@%O�@%/@%V@$��@$��@$�@$�/@$��@$�@$z�@#��@#dZ@#@"�\@"M�@"-@"�@!�#@!X@!&�@ �`@ �@ bN@ A�@�;@��@�P@l�@\)@
=@��@��@�y@ȴ@v�@ff@��@��@�@/@�j@��@Z@ƨ@t�@C�@o@�@��@n�@�@��@X@�@��@�u@bN@bN@Q�@1'@�;@�;@�@�P@
=@ff@E�@5?@5?@5?@$�@�@�-@�h@�h@�h@p�@/@�/@�D@�D@z�@9X@ƨ@��@t�@dZ@S�@33@"�@�H@��@��@M�@-@�@J@�@��@hs@�@��@�9@�u@r�@Q�@ �@b@�@�@�@�P@|�@l�@;d@+@�@��@�@��@ff@{@��@��@��@��@��@�@p�@O�@?}@/@/@/@�@V@V@��@�@�@��@I�@(�@�@��@ƨ@��@�@t�@S�@C�@33@
�@
��@
��@
n�@
=q@
-@
-@
�@
-@
-@
-@
-@
=q@
=q@
=q@	�^@	hs@	G�@	�@	%@��AͮAͲ-Aʹ9AͲ-AͮAͰ!AͰ!AͰ!AͬAͩ�Aͧ�AͮAͰ!AͬA͗�A͗�A͝�A͝�A͗�A͕�A͛�A͝�A͏\A͇+AͅA͋DA͇+AͅA̓ÁAͅA͇+A�|�A�n�A�`BA�ffA�hsA�ZA�M�A�Q�A�VA�S�A�S�A�K�A�K�A�M�A�I�A�K�A�K�A�M�A�K�A�G�A�G�A�K�A�M�A�M�A�M�A�O�A�O�A�O�A�K�A�M�A�I�A�K�A�K�A�G�A�I�A�K�A�O�A�M�A�C�A�;dA�;dA�;dA�;dA�=qA�9XA�1'A�5?A�7LA�9XA�5?A�-A�-A�-A�5?A�1'A�/A�-A�/A�33A�1'A�-A�+A�-A�33A�/A�-A�+A�+A�-A�(�A�"�A�"�A�$�A�&�A�"�A��A��A� �A�"�A��A��A��A��A� �A� �A��A��A��A��A� �A��A��A��A� �A�"�A�"�A� �A� �A�"�A�$�A�$�A� �A��A��A��A� �A��A��A��A��A��A��A��A�oA�oA�{A�{A�oA�VA�bA�oA�oA�oA�VA�VA�oA�oA�bA�VA�
=A�
=A�VA�VA�JA�1A�1A�
=A�VA�VA�VA�
=A�1A�
=A�JA�JA�
=A�1A�
=A�JA�JA�
=A�%A�%A�
=A�JA�
=A�%A�%A�1A�1A�1A�A�A�  A�A�A�A�  A�  A�A�A�A�  A���A���A�  A�  A���A���A���A���A�  A�A���A���A���A���A�  A�A�  A���A���A���A���A�  A�  A���A���A���A���A���A���A���A��A��A��A���A���A��A��A��A��A��A��A��A��yA��mA��yA��A��A��mA��TA��/A��/A��#A��/A��/A��
A���A̼jA̲-A̮A̧�Ḁ�A̡�Ȧ+A�~�A�C�A��;A�  A��AƑhA��A��`A��\A��yA�A���A���A�XA��yA�`BA�%A�M�A��A�bA�I�A�Q�A��A��TA��
A�(�A�\)A�I�A���A�I�A���A�=qA��A��yA���A�A�ƨA��A�?}A�JA�ĜA���A��A�ffA�{A���A�n�A�E�A��A��A��A��A��;A���A��jA��A���A���A��hA�~�A�v�A�p�A�ffA�Q�A�I�A�A�A�9XA�1'A�+A��A�{A�1A���A��A��;A�ĜA���A��7A�x�A�hsA�Q�A�9XA� �A��A�VA��A���A���A�hsA�VA�A�A�+A��A�VA�A���A��A��A��yA��`A��
A�ƨA���A��RA���A��DA�dZA�?}A�$�A���A��#A��wA���A��hA�n�A�(�A���A��FA�z�A�E�A�(�A��A�A���A��A��yA��HA��
A�ȴA��9A���A��PA�z�A�l�A�\)A�Q�A�A�A�&�A�A��HA��FA���A�r�A�E�A�$�A���A��jA��PA�\)A�bA��wA��A�S�A�/A�oA�A���A��A��mA���A��^A��uA�hsA�5?A���A��RA�r�A�C�A���A��wA�~�A�XA�G�A�/A�
=A��-A�\)A�%A���A��hA�M�A�bA��A��/A���A��7A�dZA�M�A�33A���A�-A���A�v�A�=qA��TA��!A���A�~�A�l�A�O�A�+A��A��-A��PA�l�A�33A�  A��^A�dZA��A�z�A��A��RA�hsA�"�A���A���A�p�A�7LA��#A��hA�I�A���A�ĜA��\A�dZA�bNA�dZA�S�A�I�A�E�A�=qA�5?A�"�A�%A���A��^A��A�t�A� �A�ƨA���A�Q�A�ffA��A���A��DA�bA�A��FA���A��uA�\)A��`A�I�A�`BA���A�`BA�1'A�1A��A���A��PA�bNA�?}A� �A�
=A��A��/A��
A��jA��DA��A��A�z�A�K�A��A��`A���A�|�A�{A���A���A��!A��DA�x�A�VA�(�A�A��A��RA���A�G�A��;A��PA�A}�-A|�A|  Az$�Ax��Ax$�Awp�Av�/Av�DAvE�AvbAu�AtĜAt�\AtjAt=qAs�^ArVAq+Ap��Ap  An�Am7LAl�Aln�Al �Ak�wAj��AjI�Aj  Ai�mAi�^Ai�PAil�AiG�Ai+Ah�Ah��Ahz�Ah�Ag�-Af��Ad�yAc�7AbJA`��A`{A_\)A^�`A^��A^A�A]��A\I�AZ�!AY�AXI�AX5?AX1'AX-AX �AW�AWC�AV~�AU��AT��ATI�ARr�AP��AO�^AN�`ANbAMhsAM�AM%AL��AL(�AK�AKt�AJ�AJE�AI�;AI�AI`BAI+AH�`AH�AH^5AH  AG�AF��AE�
ACC�ABȴABQ�AB�AB �ABAA��AA\)AAoA@�/A@�+A@=qA@JA?�mA?��A?��A?t�A?K�A?7LA?�A?VA>�A>��A>M�A>�A=��A=�A=�A=XA=/A<��A<��A<�DA<-A;��A;dZA:ĜA:JA9l�A8�yA8�uA85?A7�;A7�7A7C�A6�A6�A6�+A6ffA6A�A6-A6(�A6�A6$�A6 �A6bA6(�A6jA6�!A6��A6VA5��A5&�A4�A4�`A4�jA3�TA3�A2�A2�`A2�/A2�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                         G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	u�B	v+B	v+B	v�B	u�B	u�B	v`B	uZB	uZB	u%B	v�B	u�B	u�B	v+B	v�B	w�B	w�B	w�B	wfB	w2B	x8B	{JB	|�B	}�B	}�B	~(B	~�B	�B	�B	.B	cB	.B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	~]B	~(B	}�B	}�B	|�B	{�B	zB	v�B	q�B	hsB	��B
}�B
�gB
��B�B+�BC-BOBZQB[WB^jBiBoiBm]Bh�B_�BYBMB8�B$�B
=B
�)B
�)B
��B
�B
TaB
>�B
8�B
>B
�B
�B	�(B	�B	�pB	�B	�-B	��B	�B	t�B	sB	W�B	AUB	<jB	 �B	eB	�B	VB	-wB	2-B	7�B	7�B	-�B	B	YB	0�B	M�B	U�B	]�B	iDB	cB	�B	��B	�CB	�B	چB	�B	�B	��B
 �B
 �B
;B
AB
%B
SB
�B
B
\B
�B
�B
�B
_B
�B
!�B
&�B
'�B
)*B
*0B
.�B
3hB
2-B
2-B
33B
3�B
5?B
8�B
8B
7�B
7�B
9�B
;�B
?B
?�B
B[B
A�B
A B
B�B
B�B
EB
H�B
G�B
G�B
H�B
H�B
H�B
IRB
I�B
MB
L�B
MjB
L�B
OB
NB
N�B
O�B
OBB
OBB
PHB
P�B
Q�B
Q�B
R�B
S[B
U2B
U�B
W
B
XyB
[�B
`vB
c�B
e�B
b�B
c�B
e`B
d�B
dZB
d�B
e,B
f�B
f�B
f�B
h
B
g8B
f2B
e�B
f2B
gB
g�B
g8B
f2B
e�B
g�B
f2B
e�B
e�B
e`B
c�B
c�B
c�B
c�B
b�B
b�B
b�B
bNB
a|B
b�B
_�B
^B
\�B
[WB
YKB
X�B
VmB
V�B
S�B
S&B
R�B
Q�B
RTB
R�B
Q�B
Q�B
QNB
O�B
QNB
M6B
K)B
M6B
M�B
NB
N�B
MjB
M�B
K�B
LdB
K^B
I�B
IB
IRB
H�B
HB
GEB
EmB
FB
FB
CaB
B�B
C-B
A�B
@�B
B�B
?HB
=�B
=<B
=<B
=<B
<6B
:�B
:^B
9�B
9XB
9$B
9XB
6B
5�B
4nB
2�B
/OB
*0B
/�B
0�B
.�B
-wB
,B
*�B
'�B
"�B
B
�B
�B
�B
eB
B
eB
kB
7B
�B
kB
_B
�B
_B
�B
�B
�B
YB
�B
SB
�B
�B
B
B
�B
�B
B
�B
B
�B
�B
�B
�B
{B
FB
�B
B
�B
FB
{B
FB
�B
MB
�B
�B
�B
B
�B
MB
�B
�B
�B
B
B
SB
YB
�B
�B
�B
�B
YB
$B
_B
+B
�B
�B
�B
�B
_B
�B
_B
�B
�B
�B
�B
1B
�B
eB
�B
1B
�B
�B
B
�B
kB
B
7B
B
7B
7B
�B
kB
�B
kB
	B
7B
7B
kB
7B
B
7B
kB
=B
�B
CB
�B
B
�B
�B
!B
!B
!B
�B
�B
IB
�B
IB
B
B
B
�B
�B
�B
qB
�B
�B
B
CB
B
B
�B
�B
�B
VB
!B
VB
VB
�B
 �B
!bB
!-B
 �B
!bB
"4B
"�B
#nB
$�B
$�B
%B
$B
#nB
"�B
$B
#nB
#�B
#�B
$tB
$tB
%�B
%�B
&�B
(XB
'�B
'�B
'B
'B
'�B
($B
(�B
(�B
(�B
)�B
+kB
+�B
,=B
,=B
,�B
-B
-�B
-�B
-�B
/B
.�B
.}B
.�B
.�B
.B
.�B
-�B
/�B
.�B
.}B
-�B
1'B
/B
/B
.�B
/�B
/�B
/�B
/�B
0!B
0�B
0�B
1[B
1[B
1�B
1�B
1�B
1�B
2-B
2�B
2aB
2aB
2�B
3�B
3�B
3�B
4�B
5�B
5tB
5?B
5�B
6�B
6FB
6�B
8B
9XB
8�B
9$B
9�B
9�B
9�B
:*B
:*B
:^B
:�B
:^B
:^B
:�B
<B
;�B
<B
;�B
;�B
;�B
;�B
<6B
<jB
<�B
=<B
=�B
=qB
>BB
>wB
>�B
?}B
?�B
?HB
@�B
@OB
@�B
@�B
A�B
A�B
A�B
B[B
B[B
B�B
B�B
C-B
B�B
B�B
CaB
CaB
C�B
C�B
C�B
C�B
DgB
D�B
E�B
E�B
E�B
F�B
F�B
F�B
GEB
GEB
G�B
G�B
G�B
G�B
HB
HKB
HB
HB
HKB
I�B
IRB
H�B
J#B
J�B
J�B
K)B
J�B
J�B
J�B
K^B
LdB
L�B
L�B
MjB
MjB
MjB
M�B
M�B
MjB
M�B
NpB
N�B
OB
N<B
O�B
OBB
O�B
OvB
O�B
QB
PHB
Q�B
Q�B
Q�B
Q�B
RTB
RTB
R�B
S[B
S&B
R�B
R�B
S&B
S&B
S�B
T�B
T�B
U2B
VB
U�B
W
B
W�B
XEB
XB
XEB
XEB
XyB
XyB
XyB
XyB
XyB
X�B
YB
Y�B
YKB
YKB
Y�B
Y�B
ZQB
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
\)B
\]B
]/B
\�B
\�B
]�B
]�B
^5B
^�B
^�B
^�B
^�B
^�B
^jB
^�B
^�B
_B
_B
_B
_pB
_�B
_�B
`BB
`BB
`BB
a�B
a�B
bB
bNB
b�B
b�B
b�B
c B
c B
c�B
c�B
c�B
c�B
dZB
e�B
d�B
d�B
d�B
d�B
d�B
e�B
e`B
e,B
e`B
e�B
e�B
e�B
e�B
ffB
f�B
g�B
gmB
h>B
h
B
g�B
h
B
hsB
h>B
h�B
iB
iB
iDB
iyB
iDB
iyB
i�B
jB
jB
jB
j�B
kB
j�B
j�B
k�B
k�B
k�B
k�B
l"B
k�B
l�B
m)B
l�B
l�B
m)B
o B
n�B
n�B
n�B
n�B
n�B
o B
o5B
oiB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
pB
p;B
p�B
p�B
qAB
q�B
rGB
rB
rB
rGB
r|B
r|B
r�B
r�B
s�B
s�B
s�B
s�B
r�B
sB
sB
s�B
s�B
s�B
tB
tB
tTB
s�B
t�B
uZB
uZB
u�B
v�B
v�B
v�B
v�B
v�B
v�B
wfB
wfB
wfB
w�B
w�B
w�B
w�B
x8B
xB
x�B
y>B
y�B
zB
zDB
zDB
zxB
zxB
zDB
zDB
z�B
{B
{JB
{B
{B
{B
{B
{B
{�B
{B
{�B
{JB
|B
|�B
|�B
}VB
}VB
}VB
}�B
}�B
~]B
~]B
~�B
~�B
~�B
~�B
cB
cB
cB
�B
�B
� B
� B
�B
�B
� B
�4B
�4B
�B
��B
��B
�oB
��B
�B
�AB
��B
��B
��B
�B
�B
�{B
�{B
�B
�MB
��B
��B
��B
�SB
�SB
�SB
�SB
��B
��B
��B
��B
��B
��B
�_B
�_B
�_B
�_B
�+B
�+B
��B
��B
��B
��B
��B
�1B
�1B
��B
�B
��B
��B
�7B
��B
�	B
�	B
�	B
�=B
�=B
�rB
��B
��B
��B
�DB
�DB
�xB
�DB
�xB
�B
�B
�~B
��B
�B
��B
��B
��B
�"B
�\B
�\B
�\B
��B
��B
�\B
�\B
��B
��B
�.B
�bB
�bB
��B
��B
� B
� B
��B
� B
��B
� B
� B
��B
�4B
�4B
�4B
�4B
�4B
�4B
�hB
�hB
�hB
��B
�hB
�B
�:B
�oB
�oB
��B
��B
��B
�B
��B
�B
��B
�B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
��B
�B
��B
��B
��B
��B
�B
�B
��B
��B
��B	w2B	v+B	u%B	u�B	w�B	u�B	u�B	u%B	v`B	w2B	v�B	u�B	t�B	v�B	w�B	xB	sB	t�B	v�B	v`B	t�B	v`B	w�B	w�B	u�B	s�B	t�B	u�B	v`B	v�B	s�B	s�B	wfB	v�B	wfB	t�B	r�B	cB	u�B	t�B	u%B	t�B	v`B	v�B	u%B	uZB	v+B	u�B	v�B	u�B	u�B	v�B	w2B	v+B	u�B	uZB	yrB	v+B	w2B	v�B	x�B	wfB	yrB	u�B	v`B	xB	w2B	x8B	v�B	w2B	xB	w�B	w�B	wfB	v�B	w�B	y	B	x�B	x�B	v�B	v`B	wfB	x�B	wfB	w�B	tTB	u�B	xlB	x�B	v�B	v�B	xB	xlB	y	B	y	B	zB	z�B	{�B	|B	{B	zDB	|PB	}"B	}VB	{�B	{B	|�B	~]B	~(B	|�B	|�B	}�B	~(B	~�B	~(B	|�B	}"B	}"B	~�B	~�B	}VB	}"B	~]B	~�B	.B	~�B	~(B	}�B	~�B	�iB	��B	~�B	~�B	� B	��B	�iB	cB	~(B	cB	�iB	��B	� B	.B	~�B	cB	�4B	�iB	.B	~�B	cB	��B	�B	.B	~�B	~�B	�4B	�B	~�B	~(B	~�B	�B	�4B	�B	}�B	}�B	~�B	�B	�B	.B	}�B	}�B	~(B	cB	�B	~�B	~(B	~(B	.B	�B	~�B	~(B	}�B	~�B	cB	cB	~�B	}�B	~]B	.B	~�B	~(B	}�B	}�B	~�B	cB	cB	}�B	}�B	~(B	cB	~�B	~(B	}�B	}�B	~�B	�B	~�B	}�B	}�B	}�B	~�B	cB	~�B	}�B	}"B	}�B	.B	~�B	~]B	}�B	|�B	}�B	}�B	~�B	~�B	}VB	|�B	|�B	}�B	~]B	~(B	}�B	|�B	{�B	|B	}"B	}"B	}VB	{�B	{JB	z�B	{�B	{�B	|PB	zB	y�B	y	B	zB	zB	x�B	w�B	v`B	w2B	v�B	v�B	uZB	t�B	rB	qB	p�B	qB	qB	oiB	lWB	j�B	h
B	e,B	e�B	`�B	g�B	dZB	��B	��B	�>B
QB
K^B
^jB
i�B
��B
��B
��B
�B
�B
�$B
��B
��B
�ZB
��B
�(BGB
��B
��B
�2BJB7LB!�B4�B�B1�B2�B'�B1[B9�BIRBEBEBA�BDgBFBCaBYBF?BK^B_pBU2BW?B_B[�BXEBW�BZ�B[WB[�BX�BYBX�BY�B[�BZBZ�BZ�B]�B[�B\)B[�B[�B[�B\]B[#B[WB[WBZ�B\�B^�B`vBd�Bb�Bc�Bd�Bg8Bg8Be`Bf�Bh�BkBn�Bo Bm]BoiBq�BqBp�Bp�BpoBn�Bn�Bn�Bm�BoiBm]Bl"BlWBlWBncBo Bj�BiBj�Bg�BffBc�Be�BiBncBf�Bm�BgBf�B`�B_�B_pB\]B\�B]�B^�B^5B^jB^B]/B[�BZ�BX�BWsBU�BT�BU2BQNBQNBP�BMBI�BH�BGBG�BHKB?}B@�BB[BA B6�B6�B33B.�B-�B,�B*�B(�B-�B(�B&�B&LB!�B�B~B+BB+B.B�B�B
��B�BoB%B
��B
�(B
�|B
�TB
��B
�B
�
B
�B
�fB
�B
�BB
�BB
��B
�cB
�QB
�B
��B
�sB
�KB
��B
�tB
�9B
��B
ÖB
�3B
��B
�B
��B
�9B
��B
�zB
��B
��B
�OB
�	B
~]B
{B
tTB
k�B
e�B
j�B
c�B
d�B
_�B
]�B
W�B
Q�B
MB
PB
B�B
>�B
?}B
B'B
?�B
?HB
>�B
>BB
?}B
B[B
>�B
8RB
9�B
<B
I�B
3�B
7�B
@�B
P}B
+6B
'�B
(�B
R B
=B
8B
7�B
5?B
;0B
EB
9�B
C�B
1�B
#nB
�B
�B
�B
$B
 B
�B
B
�B
DB
	7B
�B
MB

=B
	lB
 iB	��B	��B
GB	�cB	��B	��B	��B	�.B	�5B	�oB	�/B	�KB	��B	�8B	�
B	��B	�B	�#B	�#B	�TB	ޞB	��B	�"B	ܒB	�UB	�B	�B	�3B	�B	�*B	�-B	��B	�B	��B	��B	��B	��B	�kB	��B	��B	��B	�B	��B	�B	��B	��B	��B	�;B	��B	�4B	��B	�B	zDB	x�B	y>B	x�B	u�B	u�B	q�B	sB	p�B	s�B	qvB	l�B	|�B	|�B	u%B	p;B	e`B	b�B	[�B	T,B	M�B	P}B	R�B	^�B	]�B	S&B	E9B	9XB	8B	7B	6B	9�B	>BB	;�B	?�B	2aB	4�B	B�B	5�B	)*B	*eB	'�B	VB	+B	�B	_B	�B	B	qB	�B	qB	=B	�B	�B	+B	�B	�B	�B	B	CB	=B	7LB	�B	 �B	eB	@B	B	�B	-�B	($B	,B	/B	-B	/OB	1�B	/B	.�B	4nB	4�B	2aB	2�B	0�B	0�B	2�B	7�B	7�B	9$B	8B	9�B	:�B	8RB	9�B	7�B	5tB	5?B	6zB	2�B	2aB	5�B	.B	-�B	)�B	$tB	#�B	 'B	�B	�B	OB	kB	�B	�B	SB	uB	�B	$B	$B	B	kB	�B	"�B	'�B	C-B	D3B	K�B	M�B	F�B	A B	FtB	\]B	]/B	T�B	U�B	U�B	T�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                         B	o�B	o�B	o�B	pFB	o�B	o@B	pB	oB	oB	n�B	p{B	o@B	o@B	o�B	p�B	qLB	qLB	q�B	qB	p�B	q�B	t�B	v7B	wqB	wqB	w�B	x�B	y~B	yIB	x�B	yB	x�B	xwB	xwB	x�B	xCB	xwB	xCB	xCB	xB	w�B	w�B	w=B	v�B	ueB	s�B	p�B	k�B	b%B	�ZB
wqB
�B
�B�B%QB<�BH�BTBU	BXBb�BiBgBbYBYVBS1BF�B28B[B�B
��B
��B
��B
z�B
NB
8�B
28B
7�B
XB
�B	��B	�eB	�"B	��B	��B	��B	y~B	nnB	l�B	QZB	;B	6B	�B	B	�B	B	')B	+�B	1gB	1�B	'�B	�B	B	*pB	GQB	O�B	WJB	b�B	yB	��B	��B	��B	��B	�8B	��B	��B	�7B	��B	�OB	��B	��B	��B	�B
 tB
�B
	B
�B
�B
9B
B
dB
}B
 3B
!9B
"�B
#�B
(dB
-B
+�B
+�B
,�B
-NB
.�B
2mB
1�B
12B
1�B
3�B
5B
8�B
9�B
<B
;;B
:�B
<vB
<vB
>�B
BfB
A`B
A�B
BfB
B2B
B2B
CB
ClB
F�B
FB
GB
FB
H�B
G�B
H�B
I�B
H�B
H�B
I�B
J�B
KiB
KiB
L�B
MB
N�B
O�B
P�B
R+B
U�B
Z(B
]:B
_{B
\�B
]oB
_B
^�B
^B
^uB
^�B
`�B
`MB
`MB
a�B
`�B
_�B
_�B
_�B
`�B
aSB
`�B
_�B
_�B
aSB
_�B
_�B
_�B
_B
]�B
]oB
]oB
]�B
\�B
\�B
\iB
\ B
[.B
\iB
YVB
W�B
VDB
U	B
R�B
R`B
PB
PSB
MuB
L�B
LoB
K�B
LB
L;B
K�B
KiB
K B
I]B
K B
F�B
D�B
F�B
GQB
G�B
H�B
GB
GQB
EyB
FB
EB
C�B
B�B
CB
BfB
A�B
@�B
?B
?�B
?�B
=B
<vB
<�B
;�B
:�B
<AB
8�B
7�B
6�B
6�B
6�B
5�B
4�B
4B
3sB
3
B
2�B
3
B
/�B
/ZB
. B
,|B
)B
#�B
)jB
*<B
(dB
')B
%�B
$�B
!mB
�B
�B
LB
zB
tB
B
�B
B
B
�B
�B
B
B
�B
B
�B
�B
?B
B
9B
B
3B
3B
�B
�B
9B
�B
�B
aB
�B
gB
�B
�B
aB
-B
�B
�B
�B
aB
�B
-B
�B
�B
�B
�B
aB
aB
�B
�B
�B
3B
gB
gB
�B
�B
B
B
?B
tB
?B
tB
B
�B
B
�B
�B
tB
�B
?B
B
zB
B
EB
�B
EB
EB
�B
LB
B
LB
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
RB
B
RB
B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
�B
^B
�B
jB
pB
�B
�B
�B
pB
jB
�B
dB
�B
�B
�B
�B
RB
RB
RB
#B
XB
XB
�B
�B
�B
�B
jB
6B
jB
B
�B
B
B
�B
BB
B
�B
�B
B
�B
NB
 B
�B
�B
�B
�B
 B
�B
�B
 B
UB
�B
&B
&B
aB
aB
 3B
"
B
!�B
!mB
 �B
 �B
!�B
!�B
"�B
"�B
"sB
#EB
%B
%�B
%�B
%�B
&�B
&�B
'�B
'^B
'�B
(�B
(dB
(/B
(�B
(dB
'�B
(dB
'�B
)5B
(�B
(/B
'�B
*�B
(�B
(�B
(�B
)5B
)5B
)�B
)�B
)�B
*�B
*�B
+B
+B
+BB
+BB
+vB
+�B
+�B
,HB
,B
,B
,|B
-�B
-�B
-NB
.�B
/�B
/&B
.�B
/�B
0�B
/�B
0�B
1�B
3
B
28B
2�B
3sB
3�B
3�B
3�B
3�B
4B
4yB
4B
4B
4yB
5�B
5KB
5�B
5B
5B
5B
5B
5�B
6B
6QB
6�B
7WB
7#B
7�B
8)B
8]B
9/B
9�B
8�B
:5B
:B
:�B
:�B
;pB
;pB
;�B
<B
<B
<�B
<�B
<�B
<�B
<�B
=B
=B
=|B
=HB
=|B
=�B
>B
>�B
?�B
?�B
?�B
@ZB
@ZB
@ZB
@�B
@�B
A�B
A`B
A`B
A`B
A�B
A�B
A�B
A�B
A�B
C�B
CB
B�B
C�B
D�B
DsB
D�B
D�B
D�B
DsB
EB
FB
FB
FB
GB
GB
GB
GQB
GQB
GB
G�B
H"B
HWB
H�B
G�B
I]B
H�B
I]B
I(B
I]B
J�B
I�B
KiB
K�B
KiB
KiB
LB
LB
LoB
MB
L�B
L�B
L�B
L�B
L�B
M�B
NGB
NGB
N�B
O�B
O�B
P�B
QZB
Q�B
Q�B
Q�B
Q�B
R+B
R+B
R+B
R+B
R+B
R`B
R�B
SfB
R�B
R�B
SfB
S�B
TB
T8B
T�B
T�B
U>B
U>B
U>B
UrB
U�B
VB
V�B
V�B
V�B
WJB
W~B
W�B
X�B
X�B
XPB
XPB
XPB
XB
X�B
X�B
X�B
X�B
X�B
Y"B
YVB
Y�B
Y�B
Y�B
Y�B
[cB
[�B
[�B
\ B
\4B
\iB
\iB
\�B
\�B
]�B
]:B
]�B
]oB
^B
_GB
^uB
^�B
^uB
^uB
^uB
_GB
_B
^�B
_B
_GB
_GB
_�B
_GB
`B
`�B
a�B
aB
a�B
a�B
a�B
a�B
b%B
a�B
b�B
b�B
b�B
b�B
c+B
b�B
c+B
c_B
c�B
c�B
d1B
deB
d�B
d�B
d�B
e7B
elB
elB
elB
e�B
e�B
frB
f�B
f�B
frB
f�B
h�B
h~B
hJB
hJB
hJB
h~B
h�B
h�B
iB
iB
iPB
iPB
iPB
i�B
iPB
i�B
i�B
i�B
i�B
jVB
jVB
j�B
k\B
k�B
k�B
k�B
k�B
l.B
l.B
lbB
l�B
m4B
m�B
m4B
m4B
l�B
l�B
l�B
m4B
m�B
m�B
m�B
m�B
nB
m�B
n�B
oB
oB
o�B
pFB
p�B
p�B
p�B
p{B
p{B
qB
qB
qB
qLB
qLB
qLB
q�B
q�B
q�B
r�B
r�B
sYB
s�B
s�B
s�B
t*B
t*B
s�B
s�B
t_B
u1B
t�B
u1B
u1B
u1B
u1B
u1B
ueB
u1B
ueB
t�B
u�B
v7B
vkB
wB
wB
wB
w=B
wqB
xB
xB
xCB
x�B
xwB
xwB
yB
yB
yB
yIB
yIB
y�B
y�B
y~B
y~B
y�B
y�B
y�B
z�B
z�B
z�B
{!B
{�B
{�B
{�B
|\B
|�B
|�B
|�B
|�B
}-B
}-B
}�B
}�B
~3B
~�B
~hB
B
B
B
B
:B
�B
:B
nB
�B
�@B
�B
�B
�B
�B
��B
��B
�zB
�zB
��B
��B
�zB
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
�$B
�XB
�XB
��B
��B
��B
�*B
��B
�*B
��B
��B
�0B
�eB
��B
�6B
�kB
��B
��B
�B
�B
�B
�CB
�CB
�B
�B
��B
�wB
��B
�B
�B
�}B
�IB
��B
��B
�}B
��B
�}B
��B
��B
�}B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�OB
�B
��B
��B
�!B
�!B
�UB
��B
��B
��B
��B
��B
��B
��B
�[B
��B
�[B
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
�3B
�gB
�3B	p�B	o�B	n�B	o�B	qLB	ouB	ouB	n�B	pB	p�B	p�B	ouB	n�B	p�B	q�B	q�B	l�B	n:B	p{B	pB	n�B	pB	qLB	qLB	o�B	m�B	n:B	ouB	pB	pFB	m�B	mhB	qB	p{B	qB	n:B	l�B	yB	o�B	n�B	n�B	n:B	pB	p�B	n�B	oB	o�B	o�B	p�B	o@B	ouB	p�B	p�B	o�B	ouB	oB	s$B	o�B	p�B	p{B	rSB	qB	s$B	o@B	pB	q�B	p�B	q�B	p�B	p�B	q�B	q�B	q�B	qB	p�B	qLB	r�B	r�B	r�B	pFB	pB	qB	r�B	qB	qLB	nB	o@B	rB	r�B	p�B	p{B	q�B	rB	r�B	r�B	s�B	t�B	u�B	u�B	t�B	s�B	vB	v�B	wB	u�B	u1B	v�B	xB	w�B	v�B	vkB	wqB	w�B	xwB	w�B	v�B	v�B	v�B	xwB	xCB	wB	v�B	xB	xCB	x�B	xwB	w�B	w�B	xwB	zB	zOB	x�B	x�B	y�B	zOB	zB	yB	w�B	yB	zB	zOB	y�B	x�B	xCB	yB	y�B	zB	x�B	xCB	yB	zOB	yIB	x�B	xCB	x�B	y�B	yIB	xwB	w�B	xCB	yIB	y�B	yIB	w�B	w�B	x�B	y~B	yIB	x�B	w�B	w�B	w�B	yB	y~B	xwB	w�B	w�B	x�B	yIB	x�B	w�B	wqB	xCB	yB	yB	xCB	w=B	xB	x�B	xwB	w�B	wqB	w�B	xCB	yB	yB	wqB	w=B	w�B	yB	x�B	w�B	w=B	w�B	xCB	yIB	x�B	wqB	w=B	w�B	x�B	yB	xCB	w=B	v�B	wqB	x�B	x�B	xB	w=B	v�B	w=B	w�B	xwB	xwB	wB	vkB	vkB	w=B	xB	w�B	wqB	v�B	ueB	u�B	v�B	v�B	wB	ueB	t�B	t�B	u�B	u�B	vB	s�B	sYB	r�B	s�B	s�B	r�B	qLB	pB	p�B	p{B	p{B	oB	n:B	k�B	j�B	j�B	j�B	j�B	iB	f	B	d�B	a�B	^�B	_{B	Z�B	aSB	^B	�zB	�jB	��B
J�B
EB
XB
c�B
�eB
��B
�NB
��B
��B
��B
�IB
�|B
�B
�B
��B
��B
�B
��B
��B�B0�B�B.TB�B+BB,�B!mB+B3>BCB>�B>�B;�B>B?�B=BS1B?�BEBY"BN�BP�BX�BUrBQ�BQ�BT8BU	BU>BR�BS1BR`BSfBU>BS�BTlBT�BWJBU>BU�BU�BUrBU�BVBT�BU	BU	BTlBV�BX�BZ(B^uB\iB]:B^AB`�B`�B_B`�BbYBd�Bh~Bh�BgBiBk�Bj�Bj�BjVBj!Bh~Bh~BhJBg�BiBgBe�Bf	Bf	BhBh�BdeBb�Bd�BaSB`B]oB_�Bb�BhB`�BgxB`�B`�BZ�BY�BY"BVBV�BW~BXPBW�BXBW�BV�BUrBT8BR`BQ%BOMBN|BN�BK BK BJcBF�BC�BB2B@�BA`BA�B9/B:jB<B:�B0�B0`B,�B(�B'�B&WB$KB"?B'�B"sB gB�B}B�B0B�B�B�B	�BeB �B
�IB
��B
�!B
��B
��B
��B
�.B
�B
�B
��B
�B
�\B
�B
�_B
��B
��B
ܝB
�B
�B
��B
էB
�%B
��B
�B
�&B
��B
�|B
�HB
��B
�|B
��B
��B
��B
�jB
�,B
�sB
�}B
�B
��B
xB
t�B
nB
elB
_GB
d�B
]:B
^uB
YVB
WJB
QZB
K�B
F�B
I�B
<�B
8�B
9/B
;�B
9cB
8�B
8]B
7�B
9/B
<B
8]B
2B
3�B
5�B
ClB
-�B
1gB
:5B
J/B
$�B
!mB
"�B
K�B
6�B
1�B
12B
.�B
4�B
>�B
3>B
=HB
+�B
 B
XB
LB
?B
�B

�B
�B
�B
kB
�B
�B	�hB	��B
�B
B	�B	�CB	�CB	��B	�B	�qB	�SB	�uB	��B	��B	�!B	��B	��B	�B	��B	�B	ܝB	�cB	��B	��B	�B	�PB	ЈB	��B	�DB	�B	ǹB	׳B	��B	��B	��B	��B	�gB	��B	��B	��B	�aB	��B	�B	�tB	�NB	��B	��B	��B	��B	�aB	�RB	{�B	z�B	|\B	y�B	��B	y~B	s�B	r�B	r�B	rSB	o�B	o�B	k�B	l�B	j�B	m�B	k(B	f�B	v�B	v7B	n�B	i�B	_B	\iB	U>B	M�B	GQB	J/B	LoB	XPB	WJB	L�B	>�B	3
B	1�B	0�B	/�B	3sB	7�B	5B	9cB	,B	.TB	<vB	/�B	"�B	$B	!9B	B	�B	3B	B	jB	�B	#B	jB	#B	�B	?B	gB	�B	zB	nB	�B	�B	�B	�B	0�B	�B	�B	B	�B	�B	dB	'^B	!�B	%�B	(�B	&�B	)B	+vB	(�B	(dB	. B	.TB	,B	,HB	*�B	*pB	,HB	1gB	1gB	2�B	1�B	3sB	4�B	2B	3>B	1gB	/&B	.�B	0,B	,HB	,B	/�B	'�B	'^B	#EB	&B	UB	�B	�B	^B	B	B	�B	9B	B	'B	3B	�B	�B	�B	B	�B	NB	!mB	<�B	=�B	EyB	G�B	@�B	:�B	@&B	VB	V�B	N|B	OMB	OMB	N�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                         G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223259                            20230426223259AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622325920230426223259  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622325920230426223259QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622325920230426223259QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               