CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:54Z creation      
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
_FillValue                 �  [h   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  cL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ѭ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � gx   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230426223254  20230426223254  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @��3�TH@��3�TH11  @��q�&�@��q�&�@)��m�@)��m��c��L/�{�c��L/�{11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AB  AB  ?��@   @=p�@�  @��R@�  @�\A   A\)A   A,(�A@  A`  A�  A��A�\)A�  A�Q�AϮA�  A�  B   B  B�
B  B   B(  B0(�B8  B@  BH(�BO�
BW�
B_�
Bh(�BpQ�BxQ�B�{B�{B��B�  B�{B�{B�  B�{B�{B�{B�z�B��
B��B�  B�  B�  B�  B�  B�  B�  B�  B��B��B��B�  B�  B��B�  B�  B��B��B�  C 
=C
=C  C  C
=C

=C
=C  C  C  C��C�C��C  C  C  C   C"  C$
=C&
=C(
=C*
=C,  C.  C0
=C2
=C4  C6  C8  C9��C;��C=��C@  CB
=CD
=CF  CH  CJ  CL  CN
=CP  CQ�CS��CV
=CX�CZ{C\
=C]��C_��Cb  Cd
=Ce��Ch  Cj
=Ck��Cm��Cp  Cr  Ct
=Cv  Cw�Cy��C|  C~  C��C���C���C���C�  C�  C���C���C�C�C���C���C���C���C���C�C�C�  C�  C���C���C���C���C���C�  C�
=C�C�C���C���C�C�C�C�C�C�C�  C�
=C�
=C�C�C�  C�  C���C���C���C�C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C���C�  C�  C�C�C���C���C���C�  C���C�  C�C�  C���C�  C�  C���C���C���C���C���C���C���C���C�  C���C���C�
=C�\C�  C���C���C���C���C���C���C�  C�C���C���C���C���C�  C�  C�  C�C�C�C�
=C�C�  C�C�  C���C���C���C�  C�C�  C���C�
=C�
=C���C���C�  C�  C���C���D �D �D  D��DD��D  D� D�qD��D�Dz�D��Dz�D  D� D  D� D	  D	��D	��D
}qD  D}qD��Dz�D�qD� D�D}qD��D}qD�D� D�qD}qD�qD}qD  D� D�qD}qD  D� D�D��D�D��D  Dz�D  D��D  Dz�D�qD��D�D��D  D� D�qD}qD��Dz�D�RD }qD!D!��D"  D"� D#  D#� D$  D$� D%  D%��D&�D&��D'D'� D'�qD(}qD)  D)� D*�D*}qD*�RD+z�D+�qD,� D-�D-�D.D.��D/�D/}qD0  D0��D1�D1�D2D2��D3  D3}qD3�qD4� D5  D5� D6  D6}qD6�qD7� D7�qD8}qD8�qD9z�D9��D:� D;  D;}qD;�qD<� D=  D=� D>�D>}qD>�qD?� D@�D@��D@�qDA� DB  DB� DC  DC� DD  DD� DE�DE� DE��DF}qDF�qDG}qDH  DH� DH�qDI}qDJ�DJ� DK  DK��DL  DL� DM�DM� DM�qDNz�DO  DO��DO�qDP}qDQ  DQ� DR  DR� DS  DS��DT  DT� DU  DU� DV  DV��DW  DW� DX  DX� DY  DY� DZ�DZ��D[  D[��D\  D\� D\�qD]}qD^  D^� D^�qD_� D`  D`� Da  Da� Db�Db� Dc  Dc� Dd  Dd� Dd�qDe}qDf�Df��Df�qDg� Dh  Dh��Di�Di� Dj�Dj� Dj�qDk� Dl  Dl��Dm  Dm}qDn  Dn� Do  Do��Dp  Dp}qDp��Dq� Dr�Dr�DsDs�DtDt�Du�Du� Du�qDv}qDw  Dw��Dx�Dx}qDy  Dy��Dy�qDz� D{�D{��D|�D|��D}D}��D~�D~� D~�qD}qD��D�@ D��HD�� D���D�@ D���D���D���D�AHD�� D�� D���D�>�D�� D�� D�  D�>�D��HD�� D���D�AHD�~�D���D�HD�AHD���D�� D���D�@ D�� D���D�  D�@ D�~�D��HD�  D�>�D�� D���D��qD�>�D�� D�� D�HD�@ D�� D�� D�  D�B�D��HD�� D���D�=qD�~�D��HD�HD�AHD��HD���D�  D�@ D�� D�� D�  D�AHD�� D�� D�HD�@ D�~�D�� D�HD�AHD�� D���D���D�@ D���D�� D���D�>�D�� D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D��HD�D��D�>�D�|)D�� D�  D�=qD�~�D���D���D�>�D�~�D�� D�  D�AHD��HD�D�  D�=qD�~�D�� D�HD�B�D���D��HD�  D�>�D�� D���D���D�B�D��HD��qD��qD�@ D���D�D�  D�@ D�� D���D���D�@ D��HD��HD�  D�AHD�� D���D��D�B�D�~�D���D��D�B�D�� D�� D�HD�=qD�~�D���D�  D�AHD�~�D�� D�  D�=qD�~�D���D�  D�@ D��HD�D�  D�>�D�}qD���D�  D�@ D�� D��qD���D�@ D��HD�D�  D�>�D�� D��qD��qD�>�D�~�D�� D�HD�B�D�� D�� D�  D�@ D�� D��HD�HD�>�D�~�D��HD�HD�AHD�� D�� D�  D�@ D��HD���D���D�AHD��HD���D���D�AHD��HD��qD��qD�@ D�� D��HD�HD�>�D�� D�� D�  D�AHD��HD�D���D�>�D�� D�� D�  D�@ D��HD���D�  D�B�D��HD�� D�HD�AHD�~�D�� D�  D�AHD���D��HD�  D�>�D�~�D�� D�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�>�D�~�D½qD���D�@ DÁHD��HD���D�>�DĀ D��HD�HD�AHDŀ D�� D�  D�@ DƁHD�� D�  D�@ D�~�D�� D�  D�@ DȀ D�� D���D�>�Dɀ Dɾ�D�  D�AHD�~�DʽqD���D�@ Dˀ D�� D�HD�@ D̀ D̾�D���D�@ D̀ D;�D�  D�@ D�~�D�� D�HD�@ Dπ DϾ�D�  D�@ D�~�D�� D�HD�@ Dр DѾ�D���D�@ DҀ DҾ�D�  D�@ DӁHDӾ�D�  D�AHDԀ D��HD�HD�AHDՂ�D��HD�  D�>�D�~�D־�D�  D�@ Dׂ�D��HD�HD�AHD؀ D��HD�HD�@ D�~�D�� D�  D�AHDڀ Dھ�D���D�AHDہHD۾�D���D�@ D�~�Dܾ�D�HD�AHD�}qDݽqD�  D�AHDށHD��HD���D�@ D߀ D��HD��D�AHD�� DྸD�  D�AHD�HD�� D���D�>�D� D��HD�HD�AHD� D�� D�HD�@ D�~�D�� D�HD�AHD� D徸D��qD�>�D� D��HD�  D�>�D� D�� D�  D�B�D� D辸D���D�=qD�~�D龸D�  D�B�DꂏD��HD�HD�AHD� D��HD�  D�=qD�}qD�� D�HD�@ D�HD�� D�HD�AHD�HD�� D�  D�@ D� D�� D���D�>�D�~�D�D�  D�@ D� D�� D�  D�@ D� D�� D�  D�AHD�~�D�qD��qD�>�D� D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�HD�@ D�}qD�� D��D�AHD�~�D���D���D�>�D�~�D���D���D�@ D�w
D�� >�G�>�?#�
?��?���?���?�@\)@&ff@8Q�@L��@aG�@xQ�@��@�{@���@��
@���@���@��
@�\)@��H@�ff@�\)@���A�\A��A�RA33AQ�A�RA$z�A)��A.{A3�
A:=qA>�RAC�
AI��AN�RAU�AZ=qA^�RAc33Ai��An�RAs�
Ax��A|��A���A��
A��RA���A�33A�A���A��HA��A�  A��HA�p�A��A��\A�p�A���A��\A��A�  A��\A�p�A�  A�=qA��A�  A�=qA�z�A�
=Aə�A�z�AθRAУ�A�33A�{A���A��HA��A�  A�\A�p�A�  A�=qA��A�Q�A�33A�A�Q�A��HA�{B ��BB
=BQ�BB33BQ�B	��B
�HBQ�B��B�HB  Bp�B�HB  B�B=qB�B��B=qB\)BQ�Bp�B�RB (�B!G�B"=qB#�B$��B&=qB'\)B(��B)B+
=B,��B.{B/�B0��B2{B3�
B5G�B6�RB8  B9G�B:�RB<Q�B=B>�HB@Q�BABC33BDz�BEBG
=BHQ�BIBK
=BLQ�BM��BN�RBP(�BQ��BR�RBT  BUG�BV�\BX(�BY��BZ�HB\(�B]p�B^�HB`Q�Ba��Bb�RBd  Bep�Bf�HBh(�Bi��Bj�RBl  BmG�Bn�HBp(�BqG�Br�\Bs�
BuG�Bv�RBx(�ByG�BzffB{�
B}G�B~�RB�  B��\B��B��B��\B�G�B��B�z�B��B��
B��\B�33B�B�ffB�
=B�B�z�B���B��B�Q�B�
=B���B�(�B��HB�p�B�(�B���B�\)B��B�z�B��B�B�ffB�
=B���B�(�B��RB�\)B�  B��\B�
=B��B�{B���B�G�B�B�(�B��RB�33B�B�=qB��RB�33B��B�  B�z�B�
=B��B��B�ffB���B�G�B��
B�Q�B��RB�
=B�p�B��B�Q�B��RB�
=B�G�B��B�B�{B�=qB�ffB��\B��\B�z�B��\B��\B��\B��\B��\B��\B�z�B�ffB�ffB�ffB�ffB�z�B�ffB�ffB�Q�B�Q�B�Q�B�ffB�z�B�z�B�z�B��\B�z�B��\B��\B��RB���B��HB���B���B�
=B��B�33B�G�B�\)B��B��B��B��B�{B�(�B�=qB�z�B���B��HB��B�\)B��B�B��B�{B�Q�B�z�B��RB�
=B�\)B���B��B�(�B�ffB���B��HB�33B��B��
B�(�B�z�B���B��B�p�B�B�  B�Q�B���B�
=B�\)B�B�{B�ffB��RB�
=B�\)B��B�{B�z�B��HB�33B���B��
B�(�B�z�B���B�33B��B�  B�ffB���B��B��B��
B�(�B�z�B��HB�G�B��B�{B�z�B��HB�G�B��B�  B�Q�B���B�
=B�p�B�B�=qB£�B��BÅB��
B�=qB�z�B��HB�G�BŮB�{BƏ\B��HB�G�BǮB�  B�ffBȸRB�
=B�p�B�B�(�Bʏ\B���B�G�BˮB�{B̏\B��HB�G�B͙�B��B�Q�BΣ�B�
=B�p�B��
B�Q�BиRB��BхB��B�Q�Bң�B�
=B�\)B�B�(�Bԏ\B���B�\)B��
B�=qB֣�B�
=B�p�B��B�Q�Bأ�B�
=BمB��
B�=qBڏ\B�
=B�\)B�B�(�B܏\B���B�p�B��
B�=qBޣ�B�
=B߅B��
B�=qB��B�
=B�p�B��
B�=qB��B�
=B�p�B��
B�(�B�\B���B�\)B�B�{B�z�B���B�33B癚B�  B�Q�B��B�
=B�p�B��B�=qB��B�
=B�p�B��
B�=qB��B�
=B�\)B��
B�=qB��B�
=B�p�B��
B�=qB��B��B�B��B�Q�B�RB��B�B�  B�ffB��HB�G�B��B�{B�z�B���B�G�B��B�{B�z�B��HB�G�B��B�{B��\B���B�\)B�B�=qB���B�
=B��B��B�ffB���B�G�B��C {C G�C �C C  C33Cp�C��C�
C�CQ�C�\CC��C(�CffC��C�
C
=CG�Cz�C�C�HC(�CQ�C�\CC��C33CffC��C�
C{CG�C�CC  C=qCz�C�RC	  C	=qC	z�C	C
  C
=qC
z�C
�RC
��C33CffC�C��C=qCz�C�RC  C=qCz�C�RC��C(�Cp�C�C��C33Cz�C�RC  CG�C�\C��C�C\)C��C�HC�C\)C��C�HC�CffC�C�C33Cz�CC
=CG�C�\C��C{CQ�C�\C��C{CffC��C�C33Cz�C�RC��C33Cp�C�RC  CG�C�\C�
C�C\)C��C�HC�CQ�C��C�HC(�CffC�C��C33Cz�CC  C=qCz�C�RC��C =qC �C C!
=C!G�C!�C!C"  C"=qC"z�C"C#  C#G�C#�\C#�
C$�C$ffC$�C$�C%33C%p�C%�RC&  C&Q�C&��C&�HC'(�C'p�C'�RC'��C(=qC(z�C(��C){C)\)C)�RC*  C*=qC*�\C*��C+
=C+Q�C+��C+��C,=qC,�C,C-
=C-Q�C-�\C-�HC.(�C.z�C.�RC/  C/=qC/z�C/C0  C0Q�C0��C0�HC1�C1Q�C1��C1�HC2(�C2p�C2�RC3  C3G�C3�C3��C4
=C4Q�C4��C4�C5=qC5�C5��C6{C6Q�C6��C6�C7=qC7��C7�
C8{C8ffC8C9{C9ffC9�C9��C:=qC:�C:�HC;33C;�C;�
C<�C<ffC<�RC=  C=\)C=�C>  C>G�C>��C>�HC?33C?�\C?�HC@33C@z�C@CA{CAffCA�RCB{CB\)CB��CB�CC=qCC�\CC�HCD=qCDz�CDCE{CE\)CE�CF  CF\)CF��CF�CG33CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444444444414444411441414111141111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                    ?��@   @=p�@�  @��R@�  @�\A   A\)A   A,(�A@  A`  A�  A��A�\)A�  A�Q�AϮA�  A�  B   B  B�
B  B   B(  B0(�B8  B@  BH(�BO�
BW�
B_�
Bh(�BpQ�BxQ�B�{B�{B��B�  B�{B�{B�  B�{B�{B�{B�z�B��
B��B�  B�  B�  B�  B�  B�  B�  B�  B��B��B��B�  B�  B��B�  B�  B��B��B�  C 
=C
=C  C  C
=C

=C
=C  C  C  C��C�C��C  C  C  C   C"  C$
=C&
=C(
=C*
=C,  C.  C0
=C2
=C4  C6  C8  C9��C;��C=��C@  CB
=CD
=CF  CH  CJ  CL  CN
=CP  CQ�CS��CV
=CX�CZ{C\
=C]��C_��Cb  Cd
=Ce��Ch  Cj
=Ck��Cm��Cp  Cr  Ct
=Cv  Cw�Cy��C|  C~  C��C���C���C���C�  C�  C���C���C�C�C���C���C���C���C���C�C�C�  C�  C���C���C���C���C���C�  C�
=C�C�C���C���C�C�C�C�C�C�C�  C�
=C�
=C�C�C�  C�  C���C���C���C�C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C���C�  C�  C�C�C���C���C���C�  C���C�  C�C�  C���C�  C�  C���C���C���C���C���C���C���C���C�  C���C���C�
=C�\C�  C���C���C���C���C���C���C�  C�C���C���C���C���C�  C�  C�  C�C�C�C�
=C�C�  C�C�  C���C���C���C�  C�C�  C���C�
=C�
=C���C���C�  C�  C���C���D �D �D  D��DD��D  D� D�qD��D�Dz�D��Dz�D  D� D  D� D	  D	��D	��D
}qD  D}qD��Dz�D�qD� D�D}qD��D}qD�D� D�qD}qD�qD}qD  D� D�qD}qD  D� D�D��D�D��D  Dz�D  D��D  Dz�D�qD��D�D��D  D� D�qD}qD��Dz�D�RD }qD!D!��D"  D"� D#  D#� D$  D$� D%  D%��D&�D&��D'D'� D'�qD(}qD)  D)� D*�D*}qD*�RD+z�D+�qD,� D-�D-�D.D.��D/�D/}qD0  D0��D1�D1�D2D2��D3  D3}qD3�qD4� D5  D5� D6  D6}qD6�qD7� D7�qD8}qD8�qD9z�D9��D:� D;  D;}qD;�qD<� D=  D=� D>�D>}qD>�qD?� D@�D@��D@�qDA� DB  DB� DC  DC� DD  DD� DE�DE� DE��DF}qDF�qDG}qDH  DH� DH�qDI}qDJ�DJ� DK  DK��DL  DL� DM�DM� DM�qDNz�DO  DO��DO�qDP}qDQ  DQ� DR  DR� DS  DS��DT  DT� DU  DU� DV  DV��DW  DW� DX  DX� DY  DY� DZ�DZ��D[  D[��D\  D\� D\�qD]}qD^  D^� D^�qD_� D`  D`� Da  Da� Db�Db� Dc  Dc� Dd  Dd� Dd�qDe}qDf�Df��Df�qDg� Dh  Dh��Di�Di� Dj�Dj� Dj�qDk� Dl  Dl��Dm  Dm}qDn  Dn� Do  Do��Dp  Dp}qDp��Dq� Dr�Dr�DsDs�DtDt�Du�Du� Du�qDv}qDw  Dw��Dx�Dx}qDy  Dy��Dy�qDz� D{�D{��D|�D|��D}D}��D~�D~� D~�qD}qD��D�@ D��HD�� D���D�@ D���D���D���D�AHD�� D�� D���D�>�D�� D�� D�  D�>�D��HD�� D���D�AHD�~�D���D�HD�AHD���D�� D���D�@ D�� D���D�  D�@ D�~�D��HD�  D�>�D�� D���D��qD�>�D�� D�� D�HD�@ D�� D�� D�  D�B�D��HD�� D���D�=qD�~�D��HD�HD�AHD��HD���D�  D�@ D�� D�� D�  D�AHD�� D�� D�HD�@ D�~�D�� D�HD�AHD�� D���D���D�@ D���D�� D���D�>�D�� D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D��HD�D��D�>�D�|)D�� D�  D�=qD�~�D���D���D�>�D�~�D�� D�  D�AHD��HD�D�  D�=qD�~�D�� D�HD�B�D���D��HD�  D�>�D�� D���D���D�B�D��HD��qD��qD�@ D���D�D�  D�@ D�� D���D���D�@ D��HD��HD�  D�AHD�� D���D��D�B�D�~�D���D��D�B�D�� D�� D�HD�=qD�~�D���D�  D�AHD�~�D�� D�  D�=qD�~�D���D�  D�@ D��HD�D�  D�>�D�}qD���D�  D�@ D�� D��qD���D�@ D��HD�D�  D�>�D�� D��qD��qD�>�D�~�D�� D�HD�B�D�� D�� D�  D�@ D�� D��HD�HD�>�D�~�D��HD�HD�AHD�� D�� D�  D�@ D��HD���D���D�AHD��HD���D���D�AHD��HD��qD��qD�@ D�� D��HD�HD�>�D�� D�� D�  D�AHD��HD�D���D�>�D�� D�� D�  D�@ D��HD���D�  D�B�D��HD�� D�HD�AHD�~�D�� D�  D�AHD���D��HD�  D�>�D�~�D�� D�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�>�D�~�D½qD���D�@ DÁHD��HD���D�>�DĀ D��HD�HD�AHDŀ D�� D�  D�@ DƁHD�� D�  D�@ D�~�D�� D�  D�@ DȀ D�� D���D�>�Dɀ Dɾ�D�  D�AHD�~�DʽqD���D�@ Dˀ D�� D�HD�@ D̀ D̾�D���D�@ D̀ D;�D�  D�@ D�~�D�� D�HD�@ Dπ DϾ�D�  D�@ D�~�D�� D�HD�@ Dр DѾ�D���D�@ DҀ DҾ�D�  D�@ DӁHDӾ�D�  D�AHDԀ D��HD�HD�AHDՂ�D��HD�  D�>�D�~�D־�D�  D�@ Dׂ�D��HD�HD�AHD؀ D��HD�HD�@ D�~�D�� D�  D�AHDڀ Dھ�D���D�AHDہHD۾�D���D�@ D�~�Dܾ�D�HD�AHD�}qDݽqD�  D�AHDށHD��HD���D�@ D߀ D��HD��D�AHD�� DྸD�  D�AHD�HD�� D���D�>�D� D��HD�HD�AHD� D�� D�HD�@ D�~�D�� D�HD�AHD� D徸D��qD�>�D� D��HD�  D�>�D� D�� D�  D�B�D� D辸D���D�=qD�~�D龸D�  D�B�DꂏD��HD�HD�AHD� D��HD�  D�=qD�}qD�� D�HD�@ D�HD�� D�HD�AHD�HD�� D�  D�@ D� D�� D���D�>�D�~�D�D�  D�@ D� D�� D�  D�@ D� D�� D�  D�AHD�~�D�qD��qD�>�D� D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�HD�@ D�}qD�� D��D�AHD�~�D���D���D�>�D�~�D���D���D�@ D�w
D�� >�G�>�?#�
?��?���?���?�@\)@&ff@8Q�@L��@aG�@xQ�@��@�{@���@��
@���@���@��
@�\)@��H@�ff@�\)@���A�\A��A�RA33AQ�A�RA$z�A)��A.{A3�
A:=qA>�RAC�
AI��AN�RAU�AZ=qA^�RAc33Ai��An�RAs�
Ax��A|��A���A��
A��RA���A�33A�A���A��HA��A�  A��HA�p�A��A��\A�p�A���A��\A��A�  A��\A�p�A�  A�=qA��A�  A�=qA�z�A�
=Aə�A�z�AθRAУ�A�33A�{A���A��HA��A�  A�\A�p�A�  A�=qA��A�Q�A�33A�A�Q�A��HA�{B ��BB
=BQ�BB33BQ�B	��B
�HBQ�B��B�HB  Bp�B�HB  B�B=qB�B��B=qB\)BQ�Bp�B�RB (�B!G�B"=qB#�B$��B&=qB'\)B(��B)B+
=B,��B.{B/�B0��B2{B3�
B5G�B6�RB8  B9G�B:�RB<Q�B=B>�HB@Q�BABC33BDz�BEBG
=BHQ�BIBK
=BLQ�BM��BN�RBP(�BQ��BR�RBT  BUG�BV�\BX(�BY��BZ�HB\(�B]p�B^�HB`Q�Ba��Bb�RBd  Bep�Bf�HBh(�Bi��Bj�RBl  BmG�Bn�HBp(�BqG�Br�\Bs�
BuG�Bv�RBx(�ByG�BzffB{�
B}G�B~�RB�  B��\B��B��B��\B�G�B��B�z�B��B��
B��\B�33B�B�ffB�
=B�B�z�B���B��B�Q�B�
=B���B�(�B��HB�p�B�(�B���B�\)B��B�z�B��B�B�ffB�
=B���B�(�B��RB�\)B�  B��\B�
=B��B�{B���B�G�B�B�(�B��RB�33B�B�=qB��RB�33B��B�  B�z�B�
=B��B��B�ffB���B�G�B��
B�Q�B��RB�
=B�p�B��B�Q�B��RB�
=B�G�B��B�B�{B�=qB�ffB��\B��\B�z�B��\B��\B��\B��\B��\B��\B�z�B�ffB�ffB�ffB�ffB�z�B�ffB�ffB�Q�B�Q�B�Q�B�ffB�z�B�z�B�z�B��\B�z�B��\B��\B��RB���B��HB���B���B�
=B��B�33B�G�B�\)B��B��B��B��B�{B�(�B�=qB�z�B���B��HB��B�\)B��B�B��B�{B�Q�B�z�B��RB�
=B�\)B���B��B�(�B�ffB���B��HB�33B��B��
B�(�B�z�B���B��B�p�B�B�  B�Q�B���B�
=B�\)B�B�{B�ffB��RB�
=B�\)B��B�{B�z�B��HB�33B���B��
B�(�B�z�B���B�33B��B�  B�ffB���B��B��B��
B�(�B�z�B��HB�G�B��B�{B�z�B��HB�G�B��B�  B�Q�B���B�
=B�p�B�B�=qB£�B��BÅB��
B�=qB�z�B��HB�G�BŮB�{BƏ\B��HB�G�BǮB�  B�ffBȸRB�
=B�p�B�B�(�Bʏ\B���B�G�BˮB�{B̏\B��HB�G�B͙�B��B�Q�BΣ�B�
=B�p�B��
B�Q�BиRB��BхB��B�Q�Bң�B�
=B�\)B�B�(�Bԏ\B���B�\)B��
B�=qB֣�B�
=B�p�B��B�Q�Bأ�B�
=BمB��
B�=qBڏ\B�
=B�\)B�B�(�B܏\B���B�p�B��
B�=qBޣ�B�
=B߅B��
B�=qB��B�
=B�p�B��
B�=qB��B�
=B�p�B��
B�(�B�\B���B�\)B�B�{B�z�B���B�33B癚B�  B�Q�B��B�
=B�p�B��B�=qB��B�
=B�p�B��
B�=qB��B�
=B�\)B��
B�=qB��B�
=B�p�B��
B�=qB��B��B�B��B�Q�B�RB��B�B�  B�ffB��HB�G�B��B�{B�z�B���B�G�B��B�{B�z�B��HB�G�B��B�{B��\B���B�\)B�B�=qB���B�
=B��B��B�ffB���B�G�B��C {C G�C �C C  C33Cp�C��C�
C�CQ�C�\CC��C(�CffC��C�
C
=CG�Cz�C�C�HC(�CQ�C�\CC��C33CffC��C�
C{CG�C�CC  C=qCz�C�RC	  C	=qC	z�C	C
  C
=qC
z�C
�RC
��C33CffC�C��C=qCz�C�RC  C=qCz�C�RC��C(�Cp�C�C��C33Cz�C�RC  CG�C�\C��C�C\)C��C�HC�C\)C��C�HC�CffC�C�C33Cz�CC
=CG�C�\C��C{CQ�C�\C��C{CffC��C�C33Cz�C�RC��C33Cp�C�RC  CG�C�\C�
C�C\)C��C�HC�CQ�C��C�HC(�CffC�C��C33Cz�CC  C=qCz�C�RC��C =qC �C C!
=C!G�C!�C!C"  C"=qC"z�C"C#  C#G�C#�\C#�
C$�C$ffC$�C$�C%33C%p�C%�RC&  C&Q�C&��C&�HC'(�C'p�C'�RC'��C(=qC(z�C(��C){C)\)C)�RC*  C*=qC*�\C*��C+
=C+Q�C+��C+��C,=qC,�C,C-
=C-Q�C-�\C-�HC.(�C.z�C.�RC/  C/=qC/z�C/C0  C0Q�C0��C0�HC1�C1Q�C1��C1�HC2(�C2p�C2�RC3  C3G�C3�C3��C4
=C4Q�C4��C4�C5=qC5�C5��C6{C6Q�C6��C6�C7=qC7��C7�
C8{C8ffC8C9{C9ffC9�C9��C:=qC:�C:�HC;33C;�C;�
C<�C<ffC<�RC=  C=\)C=�C>  C>G�C>��C>�HC?33C?�\C?�HC@33C@z�C@CA{CAffCA�RCB{CB\)CB��CB�CC=qCC�\CC�HCD=qCDz�CDCE{CE\)CE�CF  CF\)CF��CF�CG33CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444444444414444411441414111141111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�$�A�&�A�-A�-A�/A�/A�/A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�33A�33A�1'A�5?A�5?A�7LA�7LA�7LA�5?A�9XA�9XA�7LA�9XA�1'A�1A�1A�%A���A��TA���A�ƨAδ9AΩ�AΛ�AΙ�AΗ�AΓuAΗ�AΙ�AΕ�AΓuA�;dA��A� �A�A�hsA��TA�1A���A�O�A�t�A�"�A��uA��\A�bA���A�bA���A�M�A�9XA��RA��A�z�A�E�A��A��FA���A��A�"�A��\A~{Ax�At�\AoƨAjE�AgAcK�A_�-A\-AWAT�uARAO�AN^5AJ5?AH$�AFVAFAE�mAD�/AFA�AD�AC`BA?��A=�7A=/A=%A<��A<VA<5?A;�A:��A8��A8ffA8�9A8r�A8A7��A7�#A7/A6�jA6A5�7A4Q�A0I�A,��A,I�A+�mA*�jA)S�A(�A(~�A'\)A&��A&�!A'33A'"�A%�wA$�RA$E�A#S�A"��A"ĜA"z�A"�A!��A!�PA!O�A �+A 5?A A��AG�A�A�A�9AQ�A��AS�A�HAM�AJAXA��A�HA�A�A�\AE�A$�A��A��Al�A��A9XA�TAƨA��A�AG�A%AĜA��A5?AA�AS�A"�A��A��AVA�A��AO�A~�A��AC�A
=A��A�\Ar�A�A�A
=AĜAv�AVAA�FAXA�A�jAbA�A
�yA
�!A
��A
jA	ƨA	t�A	O�A	C�A	�AĜAZA��A�-AhsA��A�RA^5AE�A9XA�;AG�A��A�uAM�A �A��A�mA�#A�-AK�AĜA�\A=qAbA�;A�hA"�A �A ��A ��A ~�A b@�;d@�V@��@��#@���@���@�33@���@�ff@�{@��@�p�@��@�9X@���@�33@��@�ff@��^@��`@�1'@�  @�33@�5?@�O�@�j@�Q�@��@�"�@���@�A�@�K�@�@�p�@�1'@�l�@�ff@�ff@�{@��@�j@� �@���@�P@�33@◍@�E�@ᙚ@��`@�@���@�l�@�33@�o@�
=@�p�@۶F@�@���@��@���@��@�~�@�`B@ش9@��
@��@�ff@���@��`@�l�@҇+@�t�@Ұ!@���@�?}@��@���@Ѓ@�(�@Ϯ@�33@·+@͡�@���@̃@�1'@���@ˍP@ʸR@�@�hs@�&�@ȼj@�I�@�(�@��m@�;d@��@���@Ɨ�@�$�@��@ř�@�G�@�%@�b@�"�@��H@\@��@�O�@���@��D@�j@�1@���@��@��@�hs@�I�@��m@���@�K�@�"�@��y@��!@�J@��T@���@��^@��^@�p�@�V@��j@��D@�C�@�M�@�@��h@�Ĝ@�(�@���@�"�@���@�n�@�E�@�@��@�z�@�A�@��@��@��w@���@�|�@�ȴ@�ff@�=q@�@��-@�O�@�&�@���@�A�@��P@�"�@��\@���@�V@���@�I�@��@��H@�~�@�@��@��9@�I�@�1@�ƨ@�t�@�ȴ@��T@�x�@��D@�1'@���@��@���@���@�M�@�5?@�@���@�?}@���@���@��@�bN@�1'@��
@���@�dZ@�C�@���@��R@�=q@�`B@�%@��9@�ƨ@�dZ@�o@�v�@�{@��@���@���@��@�?}@��@���@��
@�@��!@���@�v�@�{@���@�Ĝ@�(�@�  @��
@��@�|�@�S�@��@���@�v�@�n�@�V@�=q@�-@��-@�x�@�?}@��/@�Ĝ@��9@��u@�1'@�K�@���@��@�ȴ@��R@��!@���@��\@�n�@�=q@��@���@��7@���@��@��@�bN@�I�@�A�@��@��@�l�@�33@�o@�@��H@��!@�^5@��@�x�@�?}@�7L@�&�@��@�1'@���@��@��m@��;@��@�l�@�K�@�"�@���@�~�@�n�@�V@�J@��h@�&�@��`@��9@��D@�z�@�bN@�9X@�b@�@\)@~�y@~V@}@|�@|z�@|(�@|1@|1@{��@{�m@{ƨ@{��@{33@z^5@zJ@zJ@y��@yhs@x��@xbN@w��@wK�@v�R@v{@u@u�@t��@t��@tz�@tZ@t1@s��@sdZ@sC�@s"�@s@r�H@r^5@q�^@qhs@q&�@q�@p�`@p�u@pA�@o�@o�;@o�w@o;d@nȴ@nff@nff@nff@nff@nff@nff@nV@nE�@m�T@mp�@l�/@l�j@l�j@l�@l�@lz�@k"�@j�H@j�\@j^5@ihs@h1'@g��@gK�@g
=@f��@f5?@f5?@f$�@e��@e��@ep�@e`B@e�@d�j@d(�@c��@cdZ@c"�@b��@b-@a��@a�7@ax�@aX@`��@`��@`b@_�@_K�@_;d@_+@^��@^�@^ȴ@^�R@^�R@^�R@^�+@^5?@]�@]��@]@\�j@[�@Z�@Z^5@Y�7@X��@X �@X  @Xb@XA�@X �@W�@W��@WK�@W
=@V�+@V5?@V$�@U�@U�@U��@U`B@UV@Tz�@Sƨ@Sƨ@S��@S33@R�@R�\@Q��@Q��@Q��@QX@Q%@P�`@P�9@P�u@Pr�@PA�@P �@Pb@OK�@N�y@N�R@Nv�@N$�@M�@M��@L��@L�j@L(�@K��@Ko@Jn�@I��@HĜ@HQ�@G�;@G+@F��@Fȴ@F�R@F��@FE�@E��@Ep�@E?}@E/@EV@D�@D�j@D9X@C�
@Cƨ@Cƨ@C�F@C��@C��@C�@CC�@C33@B�@B��@B��@B~�@BM�@A��@Ax�@A�@@��@@Ĝ@?�w@>�R@>5?@>{@>@=�@=�T@=�T@=��@=��@=@=@=?}@;33@:��@:�!@:M�@:�@9��@9hs@9&�@8Ĝ@8��@8Q�@8  @7�w@7l�@7
=@6��@65?@6@5��@5`B@4�@4(�@3��@3�F@3S�@2�\@2^5@2�@1�@1�^@1��@1��@1��@1��@1��@17L@0�`@0bN@01'@/��@/\)@/+@/
=@.�@-�@,�/@,j@+��@+o@*��@*~�@*^5@*-@)�@)�7@(Ĝ@(�u@(1'@'|�@'l�@'\)@';d@&��@&E�@%�h@%`B@%/@%�@$�@$�@$j@#ƨ@#S�@#33@#33@#"�@#"�@#@"�@"�H@"�H@"�H@"�H@"��@"�!@"��@"�\@"n�@"^5@!��@!&�@ ��@ Ĝ@ ��@ r�@ 1'@   @�;@��@�P@\)@;d@�@��@��@�R@��@v�@E�@�-@/@��@�D@I�@(�@��@��@��@��@dZ@33@33@"�@��@�!@��@�\@n�@^5@^5@M�@=q@��@�#@��@�@Ĝ@�u@bN@A�@1'@�@|�@+@��@ff@E�@�T@�-@�h@�@�@`B@?}@V@�D@(�@1@��@�m@ƨ@S�@o@�@�@��@�\@~�@^5@J@�^@hs@��@��@r�@A�@1'@ �@  @�;@��@�w@��@\)@
=@�@v�@�T@�-@��@�h@�@p�@p�@`B@O�@��@��@�@z�@1@��@�m@�m@ƨ@�F@��@t�@C�@"�@o@o@o@@
��@
�\@
^5@
�@	��@	��@	��@	�@	�7@�`@��@��@�u@�u@r�@�A� �A� �A�"�A�(�A�$�A�"�A�&�A�/A�-A�&�A�+A�/A�/A�-A�+A�-A�1'A�/A�-A�-A�1'A�33A�/A�-A�1'A�33A�33A�/A�/A�1'A�33A�/A�/A�/A�33A�33A�33A�-A�1'A�33A�1'A�/A�/A�1'A�33A�33A�33A�/A�/A�1'A�33A�1'A�/A�1'A�33A�33A�1'A�1'A�5?A�5?A�33A�1'A�33A�33A�1'A�/A�/A�33A�5?A�5?A�1'A�5?A�7LA�7LA�33A�1'A�5?A�7LA�7LA�33A�33A�5?A�9XA�7LA�33A�33A�7LA�9XA�7LA�5?A�5?A�9XA�9XA�9XA�5?A�5?A�9XA�9XA�7LA�5?A�5?A�9XA�9XA�7LA�1'A�5?A�9XA�9XA�9XA�5?A�7LA�;dA�9XA�7LA�7LA�9XA�;dA�;dA�9XA�7LA�5?A�9XA�;dA�9XA�5?A�33A�5?A�9XA�;dA�7LA�5?A�7LA�9XA�;dA�9XA�5?A�5?A�5?A�JA�
=A�
=A�A�%A�
=A�
=A�%A�%A�1A�1A�
=A�%A�%A�%A�1A�%A�A���A�A�A�  A�A�  A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���AξwAμjAθRAθRAδ9Aβ-AήAάAήAάAΩ�AΧ�AΥ�AΧ�AΡ�AΛ�AΙ�AΗ�AΗ�AΛ�AΛ�AΙ�AΗ�AΕ�AΗ�AΙ�AΗ�AΕ�AΕ�AΗ�AΙ�AΗ�AΓuAΓuAΓuAΗ�AΕ�AΏ\AΏ\AΓuAΙ�AΝ�AΝ�AΛ�AΙ�AΙ�AΛ�AΝ�AΛ�AΗ�AΕ�AΓuAΗ�AΙ�AΗ�AΗ�AΕ�AΕ�AΗ�AΙ�AΗ�AΕ�AΑhAΑhAΑhAΏ\A·+A�z�A�l�A�ZA�E�A�C�A� �A��A͏\A�ZA��A���A���Aʕ�A�oAɛ�A�"�A�p�A��mA���Aŗ�A�\)AĬA��A�`BA�A�A�jA��-A���A�I�A�A�A���A���A���A��\A�=qA��A��A�x�A�?}A�A��mA�ĜA���A��PA�v�A�^5A�C�A�"�A��A��9A��hA�r�A��!A�
=A�|�A�1'A�oA��TA��wA���A�v�A�bNA�K�A�7LA��A�  A��yA���A��jA��!A���A�v�A�-A��A�E�A�A��A���A�n�A�I�A��A��A��wA��A�dZA�Q�A�7LA�&�A��A�  A��`A��^A���A��PA�jA��A��FA�A�A��A�%A�A�A�A�A���A��A��yA��;A���A��FA�z�A�ffA�XA�E�A�33A�"�A��A���A��A���A���A�ĜA��FA���A�z�A�dZA�G�A�"�A���A��/A��!A�r�A��A���A��#A��!A��A�dZA�?}A��A���A���A��TA��;A���A��FA���A��DA�z�A�`BA�K�A�=qA�5?A�1'A� �A���A��jA���A�XA�
=A��
A���A�|�A�bNA�=qA�{A���A��yA��TA��#A�A���A�G�A�JA��TA���A�jA�E�A���A��^A���A�dZA�VA�"�A�ƨA�x�A� �A���A��hA�C�A���A��^A���A�r�A�Q�A�E�A��A��A��A��-A�v�A�?}A�oA���A��TA�ĜA�x�A�-A�-A�&�A�"�A��A�%A��;A��A�~�A�?}A��A�%A��yA�ȴA��^A��A���A���A��DA�v�A�S�A��A��yA��hA�-A���A���A�ffA�A�A�"�A���A��^A�dZA�(�A�VA�  A��TA���A��jA���A��7A�K�A��/A���A�-A��#A�=qA�ƨA���A�  A��A��/A���A���A��A��RA��uA��A�|�A�t�A�n�A�M�A�VA��9A�ZA��A��HA�ƨA��A���A��\A�z�A�hsA�Q�A�=qA�(�A��A���A��A��jA��A�\)A��A���A�l�A�+A���A�Q�A�bNA��!A��A�ZA�33A��HA�t�A���A�z�A�
=A���A��hA�C�A���A�XA�"�A�bA��^A�n�A�M�A�9XA�&�A�$�A��A�%A��#A��PA�1'A��RA��yA�C�A�&�A�JA��HA���A���A��A�n�A�ZA���A��RA�XA�33A��A�
=A��A��AhsA�A~��A~-A}�A|E�Az��Az �Ay�TAy�PAx�Ax1'Awl�Av�jAv$�AuAu�hAu?}At�At^5As�;AsXAr�HArn�Aq�-ApĜAo��AooAm�mAl�Ak��AkhsAk
=Aj��Ai��Aix�Ai&�Ah�Ah�+Ah �Ag�7Af��Af=qAe��Ae�;Ae�wAe`BAd�RAd5?Ac\)Ab(�A`ffA`5?A`�A_��A_�#A_��A_�^A_��A_\)A^��A^(�A]��A\��A\�A[�AZ��AZ1'AY��AX��AX�+AW��AWO�AW
=AV��AVI�AU�;AU7LAU%AT�`ATA�AS��AS�ASO�AR��AR�\AR=qAQ��AQ��AQ�AQ�AP��APVAP(�APAO�TAOƨAO��AO�7AOp�AO\)AO�AN�RANA�AM�AM�AMALv�AK�#AJ��AI�#AIAH�AH�+AH�+AH��AH~�AHA�AH9XAG�AGC�AG�AF��AF9XAF9XAF-AF-AF�AF1AFAE��AF  AF1AFJAFJAF1AE��AE��AE�AE�AE�#AE��AE��AEC�AD�AD�AD�AD��AD�9AE%AEdZAE�AF1AFr�AF�!AF��AFr�AF(�AF  AE�hAE"�AD�`ADv�AD�ACAC��AC�PAC�7AC�7AC�AC�AB�AB  A@ffA?/A>��A>��A>n�A> �A=��A=�hA=t�A=hsA=`BA=S�A=G�A=?}A=33A=&�A=�A=�A=�A=�A=oA=
=A<��A<�A<�`A<��A<ĜA<�jA<�jA<�jA<�9A<�\A<jA<VA<Q�A<M�A<E�A<=qA<9XA<9XA<9XA<9XA<9XA<1'A<�A<1A;�A;�A;�mA;��A;��A;�wA;�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                    A�$�A�&�A�-A�-A�/A�/A�/A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�33A�33A�1'A�5?A�5?A�7LA�7LA�7LA�5?A�9XA�9XA�7LA�9XA�1'A�1A�1A�%A���A��TA���A�ƨAδ9AΩ�AΛ�AΙ�AΗ�AΓuAΗ�AΙ�AΕ�AΓuA�;dA��A� �A�A�hsA��TA�1A���A�O�A�t�A�"�A��uA��\A�bA���A�bA���A�M�A�9XA��RA��A�z�A�E�A��A��FA���A��A�"�A��\A~{Ax�At�\AoƨAjE�AgAcK�A_�-A\-AWAT�uARAO�AN^5AJ5?AH$�AFVAFAE�mAD�/AFA�AD�AC`BA?��A=�7A=/A=%A<��A<VA<5?A;�A:��A8��A8ffA8�9A8r�A8A7��A7�#A7/A6�jA6A5�7A4Q�A0I�A,��A,I�A+�mA*�jA)S�A(�A(~�A'\)A&��A&�!A'33A'"�A%�wA$�RA$E�A#S�A"��A"ĜA"z�A"�A!��A!�PA!O�A �+A 5?A A��AG�A�A�A�9AQ�A��AS�A�HAM�AJAXA��A�HA�A�A�\AE�A$�A��A��Al�A��A9XA�TAƨA��A�AG�A%AĜA��A5?AA�AS�A"�A��A��AVA�A��AO�A~�A��AC�A
=A��A�\Ar�A�A�A
=AĜAv�AVAA�FAXA�A�jAbA�A
�yA
�!A
��A
jA	ƨA	t�A	O�A	C�A	�AĜAZA��A�-AhsA��A�RA^5AE�A9XA�;AG�A��A�uAM�A �A��A�mA�#A�-AK�AĜA�\A=qAbA�;A�hA"�A �A ��A ��A ~�A b@�;d@�V@��@��#@���@���@�33@���@�ff@�{@��@�p�@��@�9X@���@�33@��@�ff@��^@��`@�1'@�  @�33@�5?@�O�@�j@�Q�@��@�"�@���@�A�@�K�@�@�p�@�1'@�l�@�ff@�ff@�{@��@�j@� �@���@�P@�33@◍@�E�@ᙚ@��`@�@���@�l�@�33@�o@�
=@�p�@۶F@�@���@��@���@��@�~�@�`B@ش9@��
@��@�ff@���@��`@�l�@҇+@�t�@Ұ!@���@�?}@��@���@Ѓ@�(�@Ϯ@�33@·+@͡�@���@̃@�1'@���@ˍP@ʸR@�@�hs@�&�@ȼj@�I�@�(�@��m@�;d@��@���@Ɨ�@�$�@��@ř�@�G�@�%@�b@�"�@��H@\@��@�O�@���@��D@�j@�1@���@��@��@�hs@�I�@��m@���@�K�@�"�@��y@��!@�J@��T@���@��^@��^@�p�@�V@��j@��D@�C�@�M�@�@��h@�Ĝ@�(�@���@�"�@���@�n�@�E�@�@��@�z�@�A�@��@��@��w@���@�|�@�ȴ@�ff@�=q@�@��-@�O�@�&�@���@�A�@��P@�"�@��\@���@�V@���@�I�@��@��H@�~�@�@��@��9@�I�@�1@�ƨ@�t�@�ȴ@��T@�x�@��D@�1'@���@��@���@���@�M�@�5?@�@���@�?}@���@���@��@�bN@�1'@��
@���@�dZ@�C�@���@��R@�=q@�`B@�%@��9@�ƨ@�dZ@�o@�v�@�{@��@���@���@��@�?}@��@���@��
@�@��!@���@�v�@�{@���@�Ĝ@�(�@�  @��
@��@�|�@�S�@��@���@�v�@�n�@�V@�=q@�-@��-@�x�@�?}@��/@�Ĝ@��9@��u@�1'@�K�@���@��@�ȴ@��R@��!@���@��\@�n�@�=q@��@���@��7@���@��@��@�bN@�I�@�A�@��@��@�l�@�33@�o@�@��H@��!@�^5@��@�x�@�?}@�7L@�&�@��@�1'@���@��@��m@��;@��@�l�@�K�@�"�@���@�~�@�n�@�V@�J@��h@�&�@��`@��9@��D@�z�@�bN@�9X@�b@�@\)@~�y@~V@}@|�@|z�@|(�@|1@|1@{��@{�m@{ƨ@{��@{33@z^5@zJ@zJ@y��@yhs@x��@xbN@w��@wK�@v�R@v{@u@u�@t��@t��@tz�@tZ@t1@s��@sdZ@sC�@s"�@s@r�H@r^5@q�^@qhs@q&�@q�@p�`@p�u@pA�@o�@o�;@o�w@o;d@nȴ@nff@nff@nff@nff@nff@nff@nV@nE�@m�T@mp�@l�/@l�j@l�j@l�@l�@lz�@k"�@j�H@j�\@j^5@ihs@h1'@g��@gK�@g
=@f��@f5?@f5?@f$�@e��@e��@ep�@e`B@e�@d�j@d(�@c��@cdZ@c"�@b��@b-@a��@a�7@ax�@aX@`��@`��@`b@_�@_K�@_;d@_+@^��@^�@^ȴ@^�R@^�R@^�R@^�+@^5?@]�@]��@]@\�j@[�@Z�@Z^5@Y�7@X��@X �@X  @Xb@XA�@X �@W�@W��@WK�@W
=@V�+@V5?@V$�@U�@U�@U��@U`B@UV@Tz�@Sƨ@Sƨ@S��@S33@R�@R�\@Q��@Q��@Q��@QX@Q%@P�`@P�9@P�u@Pr�@PA�@P �@Pb@OK�@N�y@N�R@Nv�@N$�@M�@M��@L��@L�j@L(�@K��@Ko@Jn�@I��@HĜ@HQ�@G�;@G+@F��@Fȴ@F�R@F��@FE�@E��@Ep�@E?}@E/@EV@D�@D�j@D9X@C�
@Cƨ@Cƨ@C�F@C��@C��@C�@CC�@C33@B�@B��@B��@B~�@BM�@A��@Ax�@A�@@��@@Ĝ@?�w@>�R@>5?@>{@>@=�@=�T@=�T@=��@=��@=@=@=?}@;33@:��@:�!@:M�@:�@9��@9hs@9&�@8Ĝ@8��@8Q�@8  @7�w@7l�@7
=@6��@65?@6@5��@5`B@4�@4(�@3��@3�F@3S�@2�\@2^5@2�@1�@1�^@1��@1��@1��@1��@1��@17L@0�`@0bN@01'@/��@/\)@/+@/
=@.�@-�@,�/@,j@+��@+o@*��@*~�@*^5@*-@)�@)�7@(Ĝ@(�u@(1'@'|�@'l�@'\)@';d@&��@&E�@%�h@%`B@%/@%�@$�@$�@$j@#ƨ@#S�@#33@#33@#"�@#"�@#@"�@"�H@"�H@"�H@"�H@"��@"�!@"��@"�\@"n�@"^5@!��@!&�@ ��@ Ĝ@ ��@ r�@ 1'@   @�;@��@�P@\)@;d@�@��@��@�R@��@v�@E�@�-@/@��@�D@I�@(�@��@��@��@��@dZ@33@33@"�@��@�!@��@�\@n�@^5@^5@M�@=q@��@�#@��@�@Ĝ@�u@bN@A�@1'@�@|�@+@��@ff@E�@�T@�-@�h@�@�@`B@?}@V@�D@(�@1@��@�m@ƨ@S�@o@�@�@��@�\@~�@^5@J@�^@hs@��@��@r�@A�@1'@ �@  @�;@��@�w@��@\)@
=@�@v�@�T@�-@��@�h@�@p�@p�@`B@O�@��@��@�@z�@1@��@�m@�m@ƨ@�F@��@t�@C�@"�@o@o@o@@
��@
�\@
^5@
�@	��@	��@	��@	�@	�7@�`@��@��@�u@�u@r�@�A� �A� �A�"�A�(�A�$�A�"�A�&�A�/A�-A�&�A�+A�/A�/A�-A�+A�-A�1'A�/A�-A�-A�1'A�33A�/A�-A�1'A�33A�33A�/A�/A�1'A�33A�/A�/A�/A�33A�33A�33A�-A�1'A�33A�1'A�/A�/A�1'A�33A�33A�33A�/A�/A�1'A�33A�1'A�/A�1'A�33A�33A�1'A�1'A�5?A�5?A�33A�1'A�33A�33A�1'A�/A�/A�33A�5?A�5?A�1'A�5?A�7LA�7LA�33A�1'A�5?A�7LA�7LA�33A�33A�5?A�9XA�7LA�33A�33A�7LA�9XA�7LA�5?A�5?A�9XA�9XA�9XA�5?A�5?A�9XA�9XA�7LA�5?A�5?A�9XA�9XA�7LA�1'A�5?A�9XA�9XA�9XA�5?A�7LA�;dA�9XA�7LA�7LA�9XA�;dA�;dA�9XA�7LA�5?A�9XA�;dA�9XA�5?A�33A�5?A�9XA�;dA�7LA�5?A�7LA�9XA�;dA�9XA�5?A�5?A�5?A�JA�
=A�
=A�A�%A�
=A�
=A�%A�%A�1A�1A�
=A�%A�%A�%A�1A�%A�A���A�A�A�  A�A�  A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���AξwAμjAθRAθRAδ9Aβ-AήAάAήAάAΩ�AΧ�AΥ�AΧ�AΡ�AΛ�AΙ�AΗ�AΗ�AΛ�AΛ�AΙ�AΗ�AΕ�AΗ�AΙ�AΗ�AΕ�AΕ�AΗ�AΙ�AΗ�AΓuAΓuAΓuAΗ�AΕ�AΏ\AΏ\AΓuAΙ�AΝ�AΝ�AΛ�AΙ�AΙ�AΛ�AΝ�AΛ�AΗ�AΕ�AΓuAΗ�AΙ�AΗ�AΗ�AΕ�AΕ�AΗ�AΙ�AΗ�AΕ�AΑhAΑhAΑhAΏ\A·+A�z�A�l�A�ZA�E�A�C�A� �A��A͏\A�ZA��A���A���Aʕ�A�oAɛ�A�"�A�p�A��mA���Aŗ�A�\)AĬA��A�`BA�A�A�jA��-A���A�I�A�A�A���A���A���A��\A�=qA��A��A�x�A�?}A�A��mA�ĜA���A��PA�v�A�^5A�C�A�"�A��A��9A��hA�r�A��!A�
=A�|�A�1'A�oA��TA��wA���A�v�A�bNA�K�A�7LA��A�  A��yA���A��jA��!A���A�v�A�-A��A�E�A�A��A���A�n�A�I�A��A��A��wA��A�dZA�Q�A�7LA�&�A��A�  A��`A��^A���A��PA�jA��A��FA�A�A��A�%A�A�A�A�A���A��A��yA��;A���A��FA�z�A�ffA�XA�E�A�33A�"�A��A���A��A���A���A�ĜA��FA���A�z�A�dZA�G�A�"�A���A��/A��!A�r�A��A���A��#A��!A��A�dZA�?}A��A���A���A��TA��;A���A��FA���A��DA�z�A�`BA�K�A�=qA�5?A�1'A� �A���A��jA���A�XA�
=A��
A���A�|�A�bNA�=qA�{A���A��yA��TA��#A�A���A�G�A�JA��TA���A�jA�E�A���A��^A���A�dZA�VA�"�A�ƨA�x�A� �A���A��hA�C�A���A��^A���A�r�A�Q�A�E�A��A��A��A��-A�v�A�?}A�oA���A��TA�ĜA�x�A�-A�-A�&�A�"�A��A�%A��;A��A�~�A�?}A��A�%A��yA�ȴA��^A��A���A���A��DA�v�A�S�A��A��yA��hA�-A���A���A�ffA�A�A�"�A���A��^A�dZA�(�A�VA�  A��TA���A��jA���A��7A�K�A��/A���A�-A��#A�=qA�ƨA���A�  A��A��/A���A���A��A��RA��uA��A�|�A�t�A�n�A�M�A�VA��9A�ZA��A��HA�ƨA��A���A��\A�z�A�hsA�Q�A�=qA�(�A��A���A��A��jA��A�\)A��A���A�l�A�+A���A�Q�A�bNA��!A��A�ZA�33A��HA�t�A���A�z�A�
=A���A��hA�C�A���A�XA�"�A�bA��^A�n�A�M�A�9XA�&�A�$�A��A�%A��#A��PA�1'A��RA��yA�C�A�&�A�JA��HA���A���A��A�n�A�ZA���A��RA�XA�33A��A�
=A��A��AhsA�A~��A~-A}�A|E�Az��Az �Ay�TAy�PAx�Ax1'Awl�Av�jAv$�AuAu�hAu?}At�At^5As�;AsXAr�HArn�Aq�-ApĜAo��AooAm�mAl�Ak��AkhsAk
=Aj��Ai��Aix�Ai&�Ah�Ah�+Ah �Ag�7Af��Af=qAe��Ae�;Ae�wAe`BAd�RAd5?Ac\)Ab(�A`ffA`5?A`�A_��A_�#A_��A_�^A_��A_\)A^��A^(�A]��A\��A\�A[�AZ��AZ1'AY��AX��AX�+AW��AWO�AW
=AV��AVI�AU�;AU7LAU%AT�`ATA�AS��AS�ASO�AR��AR�\AR=qAQ��AQ��AQ�AQ�AP��APVAP(�APAO�TAOƨAO��AO�7AOp�AO\)AO�AN�RANA�AM�AM�AMALv�AK�#AJ��AI�#AIAH�AH�+AH�+AH��AH~�AHA�AH9XAG�AGC�AG�AF��AF9XAF9XAF-AF-AF�AF1AFAE��AF  AF1AFJAFJAF1AE��AE��AE�AE�AE�#AE��AE��AEC�AD�AD�AD�AD��AD�9AE%AEdZAE�AF1AFr�AF�!AF��AFr�AF(�AF  AE�hAE"�AD�`ADv�AD�ACAC��AC�PAC�7AC�7AC�AC�AB�AB  A@ffA?/A>��A>��A>n�A> �A=��A=�hA=t�A=hsA=`BA=S�A=G�A=?}A=33A=&�A=�A=�A=�A=�A=oA=
=A<��A<�A<�`A<��A<ĜA<�jA<�jA<�jA<�9A<�\A<jA<VA<Q�A<M�A<E�A<=qA<9XA<9XA<9XA<9XA<9XA<1'A<�A<1A;�A;�A;�mA;��A;��A;�wA;�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B̘B�dB̘B�dB�0B̘B�0B�dB�dB̘B�dB̘B̘B̘B̘B̘B�dB̘B��B�dB��B��B��B̘B��B̘B͟B�B�B�vBϫB��B�NB��BҽBҽB�&B��B�&B�TB�[BӏBѷBΥBуB	"�B
jB
ŢB
�B
��B
�|B
��B
��B
��B
�rB
�|B
�B
�,B
�sB
ߤB
�&B
�B
�B
�_B
t�B
�B
v�B
e�B
4�B
�B	��B	��B	�B	��B	� B	��B	��B	��B	y>B	qB	X�B	R�B	@�B	0!B	$�B	�B	0�B	I�B	GB	H�B	GzB	L�B	P}B	~]B	�VB	��B	�B	��B	�pB	�"B
B
�B
"B
�B
�B
hB
~B
5�B
<B
?�B
GzB
PB
U�B
^�B
d�B
ZB
Z�B
9$B
B
@B
�B
_B
B
YB
%�B
%FB
#nB
#nB
/�B
4B
4�B
+B
*eB
'RB
&�B
%�B
(�B
)�B
-�B
/�B
<B
<B
:�B
<�B
>BB
=qB
?�B
@OB
@�B
A�B
E�B
F?B
I�B
J�B
J�B
Q�B
P�B
QB
S&B
U�B
X�B
X�B
X�B
YKB
YB
\)B
^�B
]/B
^�B
_;B
`B
`�B
aHB
`�B
_pB
^jB
_;B
_�B
a�B
bNB
bNB
cTB
cTB
d�B
ffB
gB
e�B
cTB
a�B
`vB
_�B
_;B
_�B
_B
_B
]�B
YKB
X�B
VB
VmB
W?B
UgB
U�B
S�B
R�B
QNB
PHB
OB
NB
L�B
N<B
M6B
L0B
K^B
J�B
J�B
J#B
I�B
H�B
G�B
G�B
G�B
FB
FtB
D�B
C�B
D�B
C�B
B'B
B'B
B'B
A�B
AUB
@�B
@�B
@�B
@�B
?}B
>wB
=�B
<jB
<�B
<�B
<jB
<�B
=�B
=<B
=B
?HB
>�B
>BB
<B
;0B
=�B
;dB
:^B
9�B
9�B
:*B
9XB
8RB
8B
9$B
8B
7LB
6�B
6�B
6�B
6B
3hB
2-B
4B
1�B
0!B
.B
,qB
+B
)*B
)�B
&LB
&B
"�B
#nB
�B
 \B
�B
!bB
!�B
!�B
 \B
!B
�B
�B
 \B
�B
~B
�B
B
CB
CB
kB
7B
�B
�B
xB
�B
�B
�B
�B
�B
�B
1B
B
{B
B
�B
\B
�B
VB

=B
�B
PB
B
hB
�B
�B
�B
\B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
"B
(B
�B
�B
�B
\B
�B
PB
"B
"B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
\B
�B
�B
�B
\B
bB
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
+B
_B
�B
�B
�B
$B
�B
7B
�B
�B
7B
+B
+B
�B
_B
�B
�B
�B
�B
�B
7B
7B
�B
�B
�B
�B
�B
OB
�B
B
�B
VB
!B
VB
VB
�B
!B
�B
!�B
"4B
#�B
$B
$@B
$B
#�B
#nB
#:B
#B
"�B
#B
#nB
#�B
%FB
%FB
'RB
'�B
'�B
(XB
(�B
)�B
*eB
*eB
*�B
+6B
+kB
+�B
+�B
,B
,qB
-CB
-wB
-wB
-�B
-CB
.IB
-�B
-�B
-�B
/�B
.�B
/�B
0UB
/�B
/�B
/�B
/�B
0UB
0!B
/�B
0�B
0�B
0UB
/�B
/�B
/�B
/�B
0UB
1�B
2�B
2aB
2�B
2�B
33B
3hB
4nB
4�B
4�B
4�B
5B
4�B
4�B
5�B
5�B
6zB
6�B
6�B
6zB
6zB
7B
9XB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8RB
9XB
:*B
9�B
:^B
:�B
:^B
:^B
;0B
;dB
;0B
;�B
;�B
;�B
;�B
<6B
<jB
=<B
>B
=�B
=�B
=�B
=�B
@OB
?HB
?B
?HB
?B
?}B
?�B
?�B
?�B
A B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
D3B
D3B
D3B
D3B
DgB
EB
D�B
E9B
E�B
F?B
FtB
GzB
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
HKB
IB
H�B
H�B
IB
IRB
I�B
I�B
JXB
J�B
K^B
K�B
K�B
L�B
L�B
L�B
MB
MB
M�B
M�B
M�B
M�B
NB
M�B
NB
N�B
OBB
OBB
OvB
OvB
O�B
PHB
PHB
P}B
PHB
P}B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RTB
R�B
R�B
RTB
RTB
Q�B
RTB
S�B
S�B
S[B
R�B
U2B
T�B
U2B
U�B
UgB
V9B
VB
VB
V9B
VmB
V�B
VmB
VmB
VmB
VmB
V�B
VmB
V�B
V�B
WsB
W�B
W
B
W�B
W�B
X�B
YKB
Y�B
Z�B
[#B
Z�B
Z�B
[#B
[WB
[WB
[�B
[WB
[WB
[WB
[WB
[�B
[#B
Z�B
ZB
[�B
[�B
[WB
[�B
\�B
\�B
\�B
\�B
]dB
^B
^�B
^�B
_;B
_;B
_B
_pB
^�B
^�B
^�B
^�B
_B
_pB
_�B
`�B
a|B
aB
aB
aHB
a|B
bB
b�B
bNB
bNB
b�B
c B
c B
c B
c B
c B
cTB
c B
c B
d&B
c�B
c�B
dZB
dZB
dZB
dZB
e,B
d�B
e�B
e`B
e�B
e�B
f�B
g8B
g�B
gmB
hsB
h>B
hsB
h>B
h>B
iB
iDB
iyB
iDB
iDB
iyB
iyB
i�B
j�B
j�B
jB
jB
jB
jKB
jKB
jB
jB
jB
j�B
jB
j�B
jB
j�B
j�B
kQB
kQB
kB
kB
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
lWB
l�B
n/B
m�B
m�B
ncB
ncB
n�B
n/B
ncB
n�B
ncB
o B
o5B
o5B
oiB
o�B
o�B
pB
pB
pB
p�B
q�B
q�B
q�B
qvB
q�B
rGB
rB
rGB
r|B
r�B
r�B
r�B
r�B
r|B
r|B
r�B
sB
s�B
s�B
s�B
tTB
tB
tB
s�B
uZB
v+B
u�B
v�B
w2B
wfB
wfB
w�B
w�B
xB
xlB
y>B
y	B
yrB
zB
y�B
y�B
y�B
y�B
{B
z�B
{B
{B
{B
{JB
{JB
{JB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}"B
}"B
|�B
}�B
~(B
~]B
~(B
~]B
~�B
~�B
.B
~�B
.B
cB
�B
�B
� B
� B
�B
�4B
�4B
�4B
�4B
�;B
�;B
��B
��B
�B
�B
�AB
�uB
�uB
��B
��B
�B
�B
�B
�{B
�{B
�{B
��B
��B
�{B
��B
�{B
��B
��B
��B
��B
�SB
�SB
�SB
�SB
�SB
�B
��B
��B
�YB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�1B
�1B
�lB
�7B
�lB
�7B
�lB
�lB
�	B
�rB
�=B
�=B
�rB
��B
��B
��B
�xB
��B
�JB
��B
�B
�PB
��B
��B
��B
��B
��B
��B
�"B
�"B
��B
��B
��B
�\B
��B
��B
�.B
�.B
�bB
�bB
�bB
�bB
��B
� B
�4B
� B
�hB
�B
�B
�B
�B
�:B
�:B
�oB
��B
��B
�B
�B
�B
�B
�B
�@B
��B
��B
�B
�FB
��B
�B
�B
��B
�MB
�MB
��B
��B
��B
��B
��B�B�6B�dB�dB�jB��B�dB��B˒B�<B�B��B�^B�B͟B��B˒B��B�6B�6B��B�^B�dB͟B�dB�^B��B�B�6B�dB˒B��B�6B�6B˒B��B̘B�B̘B��B��B�6B͟B̘B��B�^B��B�B�jB�dB˒B�B͟B̘B˒B��B�B�jB�0B�^B�dB�jB�B˒B�0B͟B�B�0B˒B�dB͟B�0B˒B��B�6B��B��B��B��B�6B�jB��B�^B̘B͟B��B�0B��B�dB��B�6B�0B��B��B�jB�jB�dB��B�dB͟B�jB�0B��B�dB͟B�jB��B��B�0B��B�jB�dB�0B�6B�B�B��B�0B̘B�jB͟B�0B˒B�0B͟B��B�B̘B��B�B͟B�6B̘B˒B�0B�jB�6B�jB�<B�6B��BуBϫB͟B�pB�vB�HB�BB�pB�B�vB��B�BΥBΥB�B� BΥB�BB�BB��B�HBϫB�B�BϫB�B� B��BѷBбB҉B�HB��BуB��B�B��B��B�&BӏBѷB�B҉B��B��B�[BѷBҽB��B�[B��B҉B��B�&B�&B�,B�[B҉BуB҉B��B��B��B� BҽB�[B��B�&B҉B��B�&BӏB��BуBѷBҽB҉BѷB�B�NB��B��B՛BԕB��BӏBҽB�&B��B�[B� B�B�BуB��BѷB�}B�BBΥB�BB��BΥB��B�^B˒B�0B��B�pB��B�B�[BیB�B�TB�;B�B	6zB	7�B	:^B	B�B	J�B	\]B	b�B	�B	��B	�$B	��B	�B	خB
!bB
\B
B
K�B
y>B
�lB
�6B
�B
�@B
�+B
��B
��B
��B
��B
��B
�+B
��B
�YB
�SB
�B
�MB
��B
��B
��B
��B
��B
��B
�.B
�PB
��B
��B
�	B
��B
��B
�IB
�CB
��B
�'B
�CB
�OB
��B
�'B
��B
�'B
�:B
��B
�'B
��B
�4B
��B
�=B
�}B
�6B
�BB
��B
�jB
֡B
�B
��B
ޞB
�NB
�HB
�B
�B
�ZB
��B
�B
�mB
��B
�B
�B
�B
�B
�lB
�PB
��B
�)B
�B
�B
�B
��B
�B
��B
�"B
�WB
�)B
�B
��B
�oB
�5B
��B
�B
�B
�B
�+B
�B
�B
�B
��B
�B
�+B
�B
��B
��B
��B
��B
�B
��B
��B{B
�	B
��B
��B
��B
��B
��B�B
�JB
�PB
�.B
��B
��B�B
��B
�"B
��B
��B
�B
�PB
�rB
�	B
�xB
�>B
��B
��BuB
��B
�8B
�B
�B
��B
�2B
�B
��B
��B
�B
�B
��B
�fB
�B
��B
��B
�B
�"B
�cB
��B
�WB
��B
�B
�ZB
�B
��B
�B
�)B
�B
�B
�"B
�yB
�`B
�|B
��B
��B
�WB
��B
�cB
�B
�)B
�B
�2B
��B
�B
��B
��B
�GB
��B
�B
ݘB
یB
�B
�5B
�pB
�EB
یB
�#B
��B
��B
��B
ӏB
�B
�}B
��B
�jB
�B
��B
��B
ϫB
�}B
�,B
� B
�pB
�KB
�tB
��B
�B
��B
�aB
��B
�qB
��B
��B
�0B
��B
��B
��B
�B
�?B
��B
�RB
��B
�VB
��B
��B
��B
p�B
k�B
jB
i�B
k�B
��B
t�B
y�B
x8B
}�B
}�B
�iB
��B
��B
��B
�(B
�B
��B
�B
}�B
{B
y�B
{B
w2B
xB
u�B
s�B
o�B
t�B
p;B
o B
oiB
h�B
m�B
x�B
V�B
a�B
T�B
XyB
`�B
F�B
5B
6�B
.�B
5B
1�B
33B
/�B
�B
eB
�B
1B
!bB
SB
�B
MB
{B
�B
;B	��B	��B	�>B	�	B	��B	�>B
B	�%B
B
�B	�8B	��B	�B	��B	��B	��B	��B	یB	خB	��B	ݘB	�;B	��B	�B	�B	�8B	� B	��B	�&B	� B	�TB	�2B	�B	�B	ҽB	бB	�[B	�2B	�sB	бB	��B	��B	��B	�qB	�<B	�B	��B	��B	�B	��B	��B	�B	�[B	��B	�*B	��B	�nB	�B	��B	�=B	��B	��B	�B	�B	~]B	|�B	}VB	��B	�oB	u�B	rGB	l�B	m]B	sMB	qAB	lWB	qB	~]B	rB	[WB	ZB	Z�B	]dB	U�B	ZB	U2B	Y�B	Q�B	UgB	WsB	XyB	S&B	OBB	QNB	K^B	K^B	G�B	>BB	D�B	<jB	6FB	6�B	6FB	6zB	7�B	-B	/�B	6�B	,qB	*�B	&LB	'�B	'�B	&�B	"hB	�B	&LB	%B	"�B	!-B	B	B	B	�B	B	CB	�B	B	&LB	+B	=<B	5B	A�B	B�B	F?B	XyB	W
B	R�B	I�B	A�B	>�B	>�B	?}B	FB	H�B	C�B	N<B	MjB	H�B	N�B	JXB	E�B	FtB	E�B	GzB	GzB	GEB	GB	FtB	HB	G�B	H�B	H�B	J�B	J�B	K�B	L�B	N<B	OB	P�B	Q�B	O�B	Q�B	M6B	H�B	Q�B	B[B	aB	v`B	u�B	v�B	�rB	�JB	��B	�MB	��B	��B	��B	��B	��B	��B	�nB	�OB	��B	��B	�=B	�qB	�\B	�OB	��B	��B	�B	��B	��B	�zB	��B	�sB	�
B	�QB	ٴB	��B	��B	�/B	��B	�dB	�B	�|B	�B	��B	�B	�KB	�B	��B	�B	�B	��B
oB
{B
{B
{B
SB
B
PB
VB
�B
�B
�B
�B
"B
�B
�B
�B
PB
"B
�B
VB
�B
"B
�B
(B
�B
(B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                    B�B�JB�B�JB�B��B�JB��B�B�B�JB�B�JB�JB�JB�JB�JB�B�JB�B�B�B�B�B�JB�B�JB�QB��B��B�(B�]BʗB� B˞B�oB�oB��B̤B��B�B�B�AB�iB�WB�5B	�B
d1B
�TB
�7B
�B
�.B
��B
�kB
�7B
�$B
�.B
��B
��B
�%B
�VB
��B
ƳB
��B
�B
n:B
��B
p{B
_GB
.TB
�B	�eB	�B	��B	��B	��B	��B	��B	�zB	r�B	j�B	R�B	L�B	:5B	)�B	�B	6B	*�B	ClB	@�B	B�B	A,B	FJB	J/B	xB	�B	�jB	��B	ЈB	�"B	��B	��B
6B
�B
<B
tB
B
0B
/ZB
5�B
9�B
A,B
I�B
OMB
X�B
^�B
S�B
T8B
2�B
�B
�B
zB
B
�B
B
aB
�B
 B
 B
)jB
-�B
.TB
$�B
$B
!B
 3B
�B
"?B
#�B
'^B
)5B
5�B
5�B
4EB
6QB
7�B
7#B
9cB
:B
:�B
;;B
?TB
?�B
ClB
DsB
D�B
K5B
J�B
J�B
L�B
OMB
R�B
R�B
R`B
R�B
S1B
U�B
XPB
V�B
X�B
X�B
Y�B
Z\B
Z�B
Z�B
Y"B
XB
X�B
YVB
[cB
\ B
\ B
]B
]B
^AB
`B
`�B
_�B
]B
[cB
Z(B
Y�B
X�B
Y�B
X�B
X�B
WJB
R�B
R�B
O�B
PB
P�B
OB
OMB
MAB
L;B
K B
I�B
H�B
G�B
FB
G�B
F�B
E�B
EB
D>B
DsB
C�B
C8B
B2B
A�B
A`B
A�B
?�B
@&B
>NB
=�B
>NB
=�B
;�B
;�B
;�B
;pB
;B
:�B
:5B
:jB
:jB
9/B
8)B
7WB
6B
6�B
6QB
6B
6QB
7�B
6�B
6�B
8�B
8�B
7�B
5�B
4�B
7�B
5B
4B
3�B
3>B
3�B
3
B
2B
1�B
2�B
1�B
0�B
0�B
0`B
0�B
/�B
-B
+�B
-�B
+�B
)�B
'�B
&#B
$�B
"�B
#yB
�B
�B
�B
 B
<B
B
jB
B
HB
HB
B
�B
jB
dB
B
�B
0B
�B
�B
�B
�B
B
�B
LB
�B
*B
EB
�B
[B
�B
�B
�B
�B
�B
-B
�B

}B
	B
<B
B
�B
 tB
B
�B
B

IB

IB
	�B
	B
<B
�B
	wB
qB
6B
�B
�B
�B
6B
6B
�B
�B
�B
�B
�B
qB
kB
qB
	B
kB
B
�B
�B
6B
�B
kB
�B

}B
<B
qB
<B

}B
	�B
	B
�B
�B
	�B
	B

B
	�B

}B
UB
B
�B
�B
OB
�B
�B
�B
[B
3B
�B
?B
�B
�B
B
?B
�B
nB
�B
�B
�B
LB
LB
�B
�B
�B
�B
B
zB
zB
�B
LB
�B
�B
�B
RB
dB
dB
dB
�B
B
jB
�B
6B
B
�B
B
B
�B
�B
�B
HB
�B
UB
�B
�B
�B
UB
 B
�B
�B
NB
�B
 B
UB
�B
�B
!B
!9B
!�B
"
B
"�B
#EB
$B
$B
$�B
$�B
%B
%�B
%QB
%�B
&#B
&�B
')B
')B
'�B
&�B
'�B
'�B
'�B
'�B
)5B
(�B
)5B
*B
)�B
)�B
)�B
)5B
*B
)�B
)jB
*<B
*pB
*B
)�B
)jB
)5B
)�B
*B
+vB
,HB
,B
,HB
,|B
,�B
-B
. B
.�B
.�B
.�B
.�B
.�B
.�B
/�B
/ZB
0,B
0`B
0`B
0,B
0,B
0�B
3
B
2mB
2mB
2mB
2mB
2mB
28B
28B
2mB
28B
28B
2B
3
B
3�B
3�B
4B
4EB
4B
4B
4�B
5B
4�B
5B
5B
5KB
5B
5�B
6B
6�B
7�B
7�B
7WB
7WB
7�B
:B
8�B
8�B
8�B
8�B
9/B
9cB
9cB
9�B
:�B
:5B
:5B
:jB
;;B
<vB
=|B
=|B
=�B
=�B
=�B
=�B
>B
>�B
>�B
>�B
?TB
?�B
@&B
A,B
A`B
A�B
A`B
A`B
A`B
A`B
A�B
A`B
A�B
B�B
B�B
BfB
B�B
CB
ClB
ClB
D
B
D>B
EB
EDB
E�B
FJB
FB
FB
F�B
F�B
GQB
GQB
G�B
G�B
G�B
G�B
G�B
HWB
H�B
H�B
I(B
I(B
I]B
I�B
I�B
J/B
I�B
J/B
K B
K5B
K�B
KiB
K5B
K5B
K5B
K5B
KiB
K5B
K�B
LB
L�B
L;B
LB
LB
K�B
LB
M�B
MAB
MB
L�B
N�B
N|B
N�B
OMB
OB
O�B
O�B
O�B
O�B
PB
PSB
PB
PB
PB
PB
P�B
PB
P�B
P�B
Q%B
QZB
P�B
Q�B
Q�B
R`B
R�B
SfB
TlB
T�B
T�B
T�B
T�B
U	B
U	B
U>B
U	B
U	B
U	B
U	B
U>B
T�B
TlB
S�B
U>B
UrB
U	B
U�B
VDB
VxB
VxB
VxB
WB
W�B
X�B
X�B
X�B
X�B
X�B
Y"B
X�B
XPB
X�B
X�B
X�B
Y"B
YVB
Z\B
[.B
Z�B
Z�B
Z�B
[.B
[�B
\4B
\ B
\ B
\�B
\�B
\�B
\�B
\�B
\�B
]B
\�B
\�B
]�B
]�B
]�B
^B
^B
^B
^B
^�B
^uB
_GB
_B
_�B
_�B
`MB
`�B
aSB
aB
b%B
a�B
b%B
a�B
a�B
b�B
b�B
c+B
b�B
b�B
c+B
c+B
c_B
deB
deB
d1B
d1B
d1B
c�B
c�B
d1B
d1B
d1B
deB
d1B
deB
d1B
deB
deB
eB
eB
d�B
d�B
e7B
f=B
frB
frB
frB
frB
f�B
f�B
frB
frB
frB
f	B
f=B
g�B
gxB
gxB
hB
hB
hJB
g�B
hB
hJB
hB
h�B
h�B
h�B
iB
iPB
i�B
i�B
i�B
i�B
jVB
k\B
k�B
k\B
k(B
k\B
k�B
k�B
k�B
l.B
lbB
lbB
lbB
lbB
l.B
l.B
l�B
l�B
m4B
m4B
m�B
nB
m�B
m�B
mhB
oB
o�B
ouB
p{B
p�B
qB
qB
qLB
q�B
q�B
rB
r�B
r�B
s$B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
vB
v7B
v7B
v7B
vkB
v7B
v�B
vkB
v�B
vkB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
xB
w�B
xB
xwB
xwB
x�B
x�B
x�B
yB
yIB
y~B
y�B
y�B
y~B
y�B
y�B
y�B
y�B
z�B
z�B
{UB
{�B
{�B
{�B
{�B
|'B
|'B
|\B
|�B
|�B
|�B
|�B
}-B
}-B
}-B
}bB
}bB
}-B
}bB
}-B
}bB
}�B
}�B
~3B
B
B
B
B
B
~�B
:B
�B
�B
��B
��B
��B
�zB
�zB
��B
��B
��B
��B
��B
��B
�B
��B
�B
��B
�B
�B
��B
�$B
��B
��B
�$B
�XB
�XB
��B
�*B
��B
��B
�eB
��B
�B
�6B
�kB
�kB
�kB
��B
��B
��B
��B
�<B
�<B
�qB
�B
��B
��B
��B
��B
�B
�B
�B
�B
�IB
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
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
�[B
��B
��B
��B
��B
��B
�aB
��B
��B
�gB
�gB
�gB
��B
��BƳB��B�B�B�BǅB�B�sB�DB��BƳBŭB�BƳB�QB�B�DBŭB��B��B�yB�B�B�QB�B�B�yBƳB��B�B�DB�B��B��B�DB�yB�JBǹB�JB�yBŭB��B�QB�JB�yB�BŭBƳB�B�B�DBƳB�QB�JB�DB�yBƳB�B��B�B�B�BƳB�DB��B�QBƳB��B�DB�B�QB��B�DBŭB��BǅB�BŭBŭB��B�B�B�B�JB�QBǅB��B�yB�BǅB��B��BŭBŭB�B�B�B�yB�B�QB�B��BŭB�B�QB�BŭB�yB��BǅB�B�B��B��BƳBƳB�yB��B�JB�B�QB��B�DB��B�QBǅBƳB�JB�yBƳB�QB��B�JB�DB��B�B��B�B��B��BȋB�5B�]B�QB�"B�(B��B��B�"BǹB�(BɑB��B�WB�WB��B��B�WB��B��B�B��B�]B��B��B�]B��B��B˞B�iB�cB�;B��B˞B�5B˞B��B˞BͪB��B�AB�iB��B�;B�uBͪB�B�iB�oB̤B�B�uB�;B˞B��B��B��B�B�;B�5B�;BͪBͪB̤B��B�oB�BͪB��B�;B˞B��B�AB̤B�5B�iB�oB�;B�iB��B� B�uBΰB�MB�GBͪB�AB�oB��B̤B�B��B��B��B�5B˞B�iB�/B��B�WB��BɑB�WB�B�B�DB��B�B�"BȋBƳB�B�>B�B�B��B�\B	0,B	1gB	4B	<AB	DsB	VB	\4B	��B	�gB	��B	�WB	��B	�`B
B
	B
�B
E�B
r�B
�B
��B
��B
��B
��B
��B
�UB
�[B
��B
�[B
��B
�tB
�B
�B
��B
��B
��B
�[B
��B
��B
��B
��B
��B
�B
�HB
�OB
��B
�zB
�?B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�<B
��B
�jB
��B
�[B
��B
�/B
��B
��B
ҔB
�B
�SB
��B
ӚB
�PB
� B
��B
�AB
�GB
�B
�{B
�B
�B
�B
�GB
�GB
�eB
�bB
�B
�B
�xB
��B
�=B
�lB
��B
�B
�eB
�B
��B
�	B
��B
�B
�nB
�!B
��B
�B
�B
�\B
�\B
��B
�:B
�bB
�4B
�B
�hB
��B
�B
�{B
�SB
�_B
�=B
�1B
��B
�CB
�-B
�B
�7B
��B
�kB
�7B
��B
�UB
��B
�B
��B
�B
�7B
�UB
��B
��B
�kB
�eB
��B
�B
�$B
�B
�*B
��B
��B
�B
�'B
�_B
��B
�B
�:B
�B
��B
�:B
�B
�B
�bB
�\B
�~B
�B
�4B
�rB
�~B
�bB
��B
�B
�B
�	B
�{B
�MB
�B
��B
�B
��B
��B
��B
��B
��B
�+B
�B
�.B
ڑB
ӚB
�	B
ۗB
�B
��B
��B
�B
��B
�uB
�:B
ڑB
ݣB
��B
ީB
׳B
�JB
�>B
��B
��B
�"B
��B
�>B
��B
�|B
̤B
�|B
�AB
��B
�/B
ȋB
�B
ƳB
ǅB
ɑB
�]B
�/B
��B
��B
�"B
��B
�&B
�vB
��B
�5B
�B
��B
�#B
�sB
��B
��B
�8B
�TB
�NB
��B
��B
��B
�B
��B
�B
�NB
��B
��B
jVB
e7B
c�B
c�B
elB
z�B
n:B
sYB
q�B
w=B
w�B
zB
��B
��B
�aB
��B
��B
}�B
yIB
w�B
t�B
s�B
t�B
p�B
q�B
ouB
m�B
iPB
n:B
i�B
h�B
iB
bYB
gxB
r�B
P�B
[cB
NGB
R+B
Z�B
@�B
.�B
0�B
(�B
.�B
+�B
,�B
)jB
�B
B
�B
�B
B
B
�B	��B
-B	�:B	��B	�CB	�eB	��B	�B	�_B	��B	��B	��B	��B

IB	��B	ۗB	��B	ީB	؅B	ًB	ԠB	�>B	�`B	էB	�JB	��B	ݣB	�cB	�:B	��B	��B	ڑB	��B	��B	�B	��B	�4B	�lB	�oB	�cB	�B	��B	�%B	�cB	ǅB	áB	��B	�#B	��B	��B	�8B	�2B	��B	��B	�QB	��B	�B	�?B	��B	�?B	� B	��B	��B	��B	��B	��B	|�B	z�B	xB	v7B	wB	|\B	{!B	o�B	k�B	f�B	gB	l�B	j�B	f	B	j�B	xB	k�B	U	B	S�B	T�B	WB	OMB	S�B	N�B	SfB	K5B	OB	Q%B	R+B	L�B	H�B	K B	EB	EB	A`B	7�B	>NB	6B	/�B	0`B	/�B	0,B	1�B	&�B	)5B	0�B	&#B	$�B	�B	!mB	!9B	 3B	B	<B	�B	�B	NB	�B	�B	�B	�B	^B	�B	�B	�B	�B	�B	$�B	6�B	.�B	;;B	<vB	?�B	R+B	P�B	L�B	ClB	;pB	8]B	8]B	9/B	?�B	BfB	=|B	G�B	GB	B2B	HWB	D
B	?TB	@&B	?�B	A,B	A,B	@�B	@�B	@&B	A�B	A�B	BfB	BfB	D�B	D�B	E�B	FJB	G�B	H�B	JcB	K5B	I�B	KiB	F�B	B�B	K5B	<B	Z�B	pB	ouB	p�B	�$B	��B	��B	��B	�gB	��B	��B	�pB	�UB	��B	� B	�B	��B	��B	��B	�#B	�B	�B	��B	��B	��B	��B	�RB	�,B	�5B	�%B	мB	�B	�fB	�rB	�xB	��B	֭B	�B	ٿB	�.B	�:B	�uB	�B	��B	�7B	�xB	�B	�:B	�kB	�!B	�-B	�-B	�-B	�B
�B
B
B
6B
�B
6B
kB
�B
qB
<B
kB
B
�B
�B
B
	CB
�B
�B
�B
<B
�B
	wG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223254                            20230426223254AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622325420230426223254  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622325420230426223254QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622325420230426223254QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               