CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-03-14T22:28:53Z creation; 2023-04-26T19:14:25Z DMQC;      
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
_FillValue        G�O�     x  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  d`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʨ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ҈   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20180314222853  20230426191425  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_006                 7316_008644_006                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�Sv�)^�@�Sv�)^�11  @�Sv��"�@�Sv��"�@*'j@@*'j@�d,P���*�d,P���*11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�\@E�@�G�@�  @��R@޸RA ��A��A ��A,��A@  A`  A�Q�A�Q�A�  A�  A�  AϮA߮A�A��B�
B  B�
B   B(  B0  B8(�B@(�BH  BP  BX  B_�
Bh  Bo�
Bx  B�{B�{B�{B�  B�{B�  B��B�{B�{B�{B��B��
B�  B�(�B��B��B��B��B��B�  B�{B�  B�  B�  B��B�  B�  B��
B��
B��B�  B�  C 
=C  C��C  C
=C
  C��C  C
=C  C  C��C��C
=C
=C��C�C!��C$  C&  C'��C*  C,  C.�C/��C2  C3��C6  C8  C:  C<  C>  C@  CB  CD  CF  CG��CJ  CL  CN  CP
=CR
=CT  CV  CW��CZ
=C\
=C^  C`
=Cb
=Cd  Ce��Ch  Cj  Cl  Cn  Cp  Cr
=Ct
=Cv  Cx  Cz  C|  C~  C�C�C���C���C���C�C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C���C���C���C�  C�  C�  C�C�  C�  C�C�  C�  C���C�  C�C�  C���C���C���C�  C�C�C�C�  C�  C�  C���C�  C���C�  C�
=C�  C�  C�C�  C���C���C���C�  C�  C���C�C�  C�  C�  C���C�  C�  C�C�  C���C���C�  C�  C���C���C���C���C�  C�C�C�C�C�C�  C���C�  C�  C�  C�C�C�  C�  C���C�  C�C�C�
=C�C�C�  C���C���C���C�  C�  C���C�  C�
=C�C�C�C�  C�C�C�C�C�  C�  C�  C���C�  C�
=C�C�  C�D �D � D  D� D  D� D�D� D  D}qD�qD� D�D� D  D� D  D}qD	  D	� D
  D
� D  D��D  D� D��D}qD�qD}qD  D}qD�qD}qD�qD}qD�qD��DD�D�D��D�qDz�D  D��D�D��D  D}qD  D� DD� D�qD��DD�D�D��D�qDz�D�qD� D�qD z�D �qD!��D"D"��D#�D#� D$  D$� D$�qD%}qD&�D&��D'�D'}qD(  D(� D)�D)� D)�RD*z�D*��D+z�D+��D,z�D,�qD-� D.  D.� D/  D/��D/�qD0}qD1�D1��D2D2�D3�D3��D4�D4� D4�qD5z�D6  D6��D6�qD7� D8  D8� D9  D9}qD:  D:��D:�qD;z�D<  D<��D=�D=� D>  D>}qD>�qD?� D@  D@}qD@�qDA� DB  DB� DC  DC� DD  DD��DEDE��DF  DF��DG  DG}qDH  DH� DI  DIz�DJ  DJ��DK�DK� DL  DL� DM�DM��DN  DN� DO  DO��DP  DP}qDP�qDQz�DR  DR��DS  DS��DT�DT� DU  DU}qDV  DV��DW  DW� DW�qDX� DY  DY� DY�qDZ� D[D[��D\�D\��D]  D]� D^  D^� D^�qD_� D`  D`}qD`�qDaz�Da�qDb� Dc  Dc� Dc�qDd� De�De�Df  Df� Df�qDg}qDh�Dh��Di  Di� Di�qDj� DkDk��Dl  Dl}qDm  Dm� Dn�Dn� Dn�qDo� Dp  Dp��Dq  Dq� Dr  Dr}qDr�qDs}qDt  Dt� Du  Du��Dv  Dv� Dw  Dw}qDw�qDx��Dy�Dy}qDz  Dz� D{  D{� D|  D|� D}  D}��D~  D~� D~��D}qD�qD�>�D�~�D���D�  D�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�� D��HD���D�>�D�� D�� D�  D�>�D�~�D���D���D�@ D�� D���D�  D�>�D�}qD�� D���D�@ D���D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D���D�>�D�� D�� D�  D�@ D�� D���D�  D�@ D�~�D���D�  D�AHD��HD��HD�  D�>�D�� D��HD�  D�@ D�� D��HD��D�AHD�� D�� D�  D�>�D�� D��HD�HD�@ D�� D��HD�HD�@ D��HD�D�HD�@ D�� D���D���D�@ D�� D���D���D�>�D�~�D���D�  D�@ D��HD�D��D�@ D��HD���D��qD�@ D��HD�� D�  D�@ D�}qD��qD�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�@ D��HD�� D���D�>�D�}qD��qD���D�B�D�~�D���D��D�AHD���D��HD���D�@ D�� D��HD�HD�AHD�� D��qD���D�@ D�~�D��qD�HD�@ D�~�D�� D�HD�@ D��HD�� D�  D�B�D���D�� D��qD�>�D��HD�� D�  D�@ D�}qD���D���D�@ D�� D���D�  D�AHD�� D�� D�HD�@ D�� D�� D�  D�>�D�~�D�� D�HD�@ D�� D��HD�HD�AHD���D��HD��D�AHD��HD�� D�HD�AHD��HD��HD�HD�@ D��HD��HD��D�B�D���D���D�  D�AHD�� D��HD���D�>�D��HD�� D���D�>�D�~�D���D���D�B�D��HD�� D�  D�@ D��HD��HD�HD�@ D��HD�� D���D�@ D���D�� D�HD�@ D�� D��HD���D�@ D�� D�� D���D�>�D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�HD�AHD��HD�� D���D�@ D�~�D�� D�HD�AHDÁHD��HD�  D�AHDĀ Dľ�D���D�>�Dŀ D�� D�  D�@ Dƀ D�� D�  D�>�D�~�D�� D�  D�@ DȁHDȾ�D�  D�@ D�~�D�� D�HD�AHDʀ D�� D�  D�AHDˀ D˾�D���D�>�D�~�D�� D�  D�>�D�}qD�� D�  D�@ D΁HD�� D�  D�@ D�~�DϾ�D�  D�@ DЁHD�� D���D�@ Dр DѾ�D���D�@ DҀ D��HD�  D�AHDӀ D�� D�  D�AHDԀ DԾ�D���D�@ DՁHDսqD���D�>�Dր D��HD�  D�>�D�~�D��HD�HD�AHD؂�D�� D���D�@ DفHD��HD�HD�AHDځHD�� D�  D�@ D�~�D۾�D��qD�>�D�~�D�� D�HD�AHD݀ Dݾ�D�  D�@ Dހ D��HD��D�@ D߀ D��HD�HD�@ D�� D��HD���D�>�D�HD��HD�  D�@ D� D�� D�HD�>�D�~�D㾸D�  D�@ D� D�� D�HD�>�D�}qD�� D��D�AHD� D�� D�HD�@ D� D�D��D�AHD�HD�� D�  D�@ D� D�� D�  D�@ D� D��HD�HD�@ D�~�D뾸D�  D�AHD�HD쾸D�  D�AHD� D���D�HD�>�D�~�D�� D���D�=qD�~�D�� D���D�>�D��HD�� D�  D�AHD�HD�D���D�AHD�D�� D�HD�AHD� D�� D�HD�AHD�D��HD���D�>�D�� D��qD���D�@ D�~�D�� D��D�@ D�~�D���D���D�>�D�}qD�� D�  D�=qD�� D�D��D�"�>�?�?B�\?�  ?��R?�p�?�G�?��H@
=q@�R@333@@  @O\)@^�R@s33@��\@�=q@���@�(�@��
@���@�@�p�@���@У�@�Q�@�  @�=q@�z�@��HAG�A�A
=qA\)A�\AffA�A   A$z�A'�A,��A1G�A6ffA:=qA>{AB�\AG�AL��AP��AU�AY��A^�RAc�
Ah��Al��Aq�Aw
=A|(�A���A��\A��A��A�=qA�z�A�
=A�G�A�(�A��RA�G�A�33A�p�A�  A��HA��A�\)A�G�A��A�ffA���A�33A��A�
=A��A�(�A�{A�Q�A��HA��A�
=A���A˅A�{AУ�Aҏ\A���A�
=A��A�(�A�ffA��A�33A�A�Q�A�\A���A�RA�G�A�(�A�ffA�Q�A��HA�p�B   B ��B{B\)B��B�B�HB(�B	G�B
�\B�
B�B{B\)Bz�BB
=BQ�BG�B�\B�
BG�BffB\)Bz�B�B33B z�B!��B"�RB$  B%�B&�\B'�
B)�B*=qB+\)B,z�B-B/33B0Q�B1��B2�RB3�
B4��B6{B7�B8��B:{B;
=B<Q�B=��B?
=B@Q�BA��BB�RBC�
BD��BF=qBG�BI�BJ=qBK�BL��BM�BO�BP��BR{BS33BTQ�BUBW
=BXz�BYBZ�HB\(�B]��B^�HB`Q�Bap�Bb�RBd  Bep�Bf�RBh(�Bi�BjffBk�
Bm�Bn�\Bo�Bp��Br{Bs33Btz�Bu�Bw33Bxz�Byp�Bz�RB|  B}G�B~�\B�B�ffB���B��B�(�B���B�\)B��B�z�B��B��
B�ffB���B��B�(�B��HB���B�(�B���B�\)B�  B��\B�G�B��B��\B�33B��B�Q�B��HB���B�(�B���B���B�{B���B�\)B�  B��\B�G�B��B��\B�33B��B��\B�33B�B�Q�B��HB��B�(�B��HB��B�(�B���B�\)B��B�z�B��B��
B�z�B��B��B�=qB���B�p�B�{B���B�\)B�  B���B�33B�B�ffB���B���B�=qB��HB��B�(�B��RB�\)B��B�z�B��B���B�{B��\B���B�\)B�B�{B�z�B���B�33B��B��
B�(�B�z�B��HB�G�B���B�  B�ffB��RB��B��B��B�=qB���B���B�\)B��B�{B�ffB���B��B�p�B�B�(�B�z�B���B�33B��B��
B�=qB�z�B��HB�33BÙ�B��B�Q�Bģ�B�
=B�p�B��
B�=qBƣ�B�
=B�p�B��
B�Q�Bȣ�B�
=BɅB��B�Q�Bʣ�B��B˅B��B�Q�B̸RB��B͙�B�  B�z�B��HB�\)B�B�(�BЏ\B���B�p�B��B�ffB���B�33BӮB�(�Bԣ�B�
=Bՙ�B�  B�z�B���BׅB��B�ffB��HB�\)B��
B�=qB���B�33B�B�=qBܸRB�G�B�B�=qB���B�G�B�B�Q�B���B�\)B��
B�Q�B��HB�\)B��
B�Q�B��HB�\)B��B�z�B���B�B�  B�\B��B�B�=qB���B�\)B��B�z�B���B�B�  B�\B��B�B�(�B�RB�G�B�B�Q�B��HB�p�B��B�z�B�
=B��B�{B��\B��B���B�(�B��RB�G�B��
B�ffB���B��B�{B���B�33B�B�=qB���B�G�B��
C 33C �C ��C{CffC��C��C=qCz�C��C
=CQ�C�\C�
C�CffC�RC  CG�C��C�HC(�Cp�C�RC  CG�C�C�
C{C\)C�C��C	G�C	�\C	�HC
(�C
z�C
C
=CQ�C��C�HC(�Cp�C�RC  CQ�C��C�HC33Cz�C�RC  CG�C�C��C�CffC�C��C=qC�C��C
=CQ�C��C�HC33Cz�C��C�CffC�C��C=qCz�C��C�CffC�RC  CG�C�\C��C{CQ�C��C�HC33C�C��C
=CQ�C�\C�HC(�Cp�C��C��CG�C�\CC
=CQ�C��C�C(�CffC�C  CG�C�C��C 
=C \)C ��C �C!(�C!ffC!�C"  C"=qC"�\C"��C#
=C#Q�C#��C#�HC$(�C$p�C$�RC$��C%33C%�C%�
C&{C&G�C&�\C&�HC'(�C'p�C'�C'��C(33C(�C(��C)
=C)G�C)�C)��C*{C*\)C*�\C*��C+{C+\)C+��C+�
C,�C,ffC,��C,�
C-{C-\)C-��C-��C-��C.=qC.p�C.�\C.�C.C.�HC/  C/�C/33C/G�C/\)C/ffC/z�C/�\C/�C/C/�
C/�HC/��C0
=C0�C0=qC0\)C0p�C0�C0�\C0��C0C0�HC0��C1
=C1�C133C1Q�C1p�C1�C1��C1�C1C1�
C1��C2{C233C2=qC2Q�C2p�C2�C2��C2C2��C2�C3
=C3(�C3=qC3G�C3\)C3z�C3��C3�RC3��C3�HC4  C4(�C4=qC4\)C4ffC4�C4�C4C4�
C4��C5�C5G�C5\)C5ffC5�\C5�RC5�
C5�C6  C633C6Q�C6ffC6z�C6��C6��C6�HC6��C7�C7G�C7ffC7z�C7��C7C7�HC7��C8�C8=qC8ffC8z�C8��C8C8�C9  C9�C9Q�C9z�C9��C9�RC9�HC:  C:(�C:\)C:z�C:��C:�RC:�HC;{C;33C;\)C;p�C;��C;C;��C<{C<33C<Q�C<�\C<�C<��C<�C=(�C=G�C=ffC=��C=C=�HC>  C>33C>\)C>z�C>��C>�
C?  C?{C?G�C?p�C?��C?�C?�C@{C@=qC@Q�C@z�C@�RC@�
C@��CA�CAQ�CAz�CA��CACA�CB
=CB33CBffCB�CB��CB��CC  CC{CC=qCCp�CC��CC�RCC�
CD
=CD(�CDG�CDz�CD��CDCD�CE�CE33CE\)CE�\CE�CE��CF  CF(�CFG�CFp�CF��CF��CF�HCG
=CG=qCGffCG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             ?�  @�\@E�@�G�@�  @��R@޸RA ��A��A ��A,��A@  A`  A�Q�A�Q�A�  A�  A�  AϮA߮A�A��B�
B  B�
B   B(  B0  B8(�B@(�BH  BP  BX  B_�
Bh  Bo�
Bx  B�{B�{B�{B�  B�{B�  B��B�{B�{B�{B��B��
B�  B�(�B��B��B��B��B��B�  B�{B�  B�  B�  B��B�  B�  B��
B��
B��B�  B�  C 
=C  C��C  C
=C
  C��C  C
=C  C  C��C��C
=C
=C��C�C!��C$  C&  C'��C*  C,  C.�C/��C2  C3��C6  C8  C:  C<  C>  C@  CB  CD  CF  CG��CJ  CL  CN  CP
=CR
=CT  CV  CW��CZ
=C\
=C^  C`
=Cb
=Cd  Ce��Ch  Cj  Cl  Cn  Cp  Cr
=Ct
=Cv  Cx  Cz  C|  C~  C�C�C���C���C���C�C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C���C���C���C�  C�  C�  C�C�  C�  C�C�  C�  C���C�  C�C�  C���C���C���C�  C�C�C�C�  C�  C�  C���C�  C���C�  C�
=C�  C�  C�C�  C���C���C���C�  C�  C���C�C�  C�  C�  C���C�  C�  C�C�  C���C���C�  C�  C���C���C���C���C�  C�C�C�C�C�C�  C���C�  C�  C�  C�C�C�  C�  C���C�  C�C�C�
=C�C�C�  C���C���C���C�  C�  C���C�  C�
=C�C�C�C�  C�C�C�C�C�  C�  C�  C���C�  C�
=C�C�  C�D �D � D  D� D  D� D�D� D  D}qD�qD� D�D� D  D� D  D}qD	  D	� D
  D
� D  D��D  D� D��D}qD�qD}qD  D}qD�qD}qD�qD}qD�qD��DD�D�D��D�qDz�D  D��D�D��D  D}qD  D� DD� D�qD��DD�D�D��D�qDz�D�qD� D�qD z�D �qD!��D"D"��D#�D#� D$  D$� D$�qD%}qD&�D&��D'�D'}qD(  D(� D)�D)� D)�RD*z�D*��D+z�D+��D,z�D,�qD-� D.  D.� D/  D/��D/�qD0}qD1�D1��D2D2�D3�D3��D4�D4� D4�qD5z�D6  D6��D6�qD7� D8  D8� D9  D9}qD:  D:��D:�qD;z�D<  D<��D=�D=� D>  D>}qD>�qD?� D@  D@}qD@�qDA� DB  DB� DC  DC� DD  DD��DEDE��DF  DF��DG  DG}qDH  DH� DI  DIz�DJ  DJ��DK�DK� DL  DL� DM�DM��DN  DN� DO  DO��DP  DP}qDP�qDQz�DR  DR��DS  DS��DT�DT� DU  DU}qDV  DV��DW  DW� DW�qDX� DY  DY� DY�qDZ� D[D[��D\�D\��D]  D]� D^  D^� D^�qD_� D`  D`}qD`�qDaz�Da�qDb� Dc  Dc� Dc�qDd� De�De�Df  Df� Df�qDg}qDh�Dh��Di  Di� Di�qDj� DkDk��Dl  Dl}qDm  Dm� Dn�Dn� Dn�qDo� Dp  Dp��Dq  Dq� Dr  Dr}qDr�qDs}qDt  Dt� Du  Du��Dv  Dv� Dw  Dw}qDw�qDx��Dy�Dy}qDz  Dz� D{  D{� D|  D|� D}  D}��D~  D~� D~��D}qD�qD�>�D�~�D���D�  D�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�� D��HD���D�>�D�� D�� D�  D�>�D�~�D���D���D�@ D�� D���D�  D�>�D�}qD�� D���D�@ D���D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D���D�>�D�� D�� D�  D�@ D�� D���D�  D�@ D�~�D���D�  D�AHD��HD��HD�  D�>�D�� D��HD�  D�@ D�� D��HD��D�AHD�� D�� D�  D�>�D�� D��HD�HD�@ D�� D��HD�HD�@ D��HD�D�HD�@ D�� D���D���D�@ D�� D���D���D�>�D�~�D���D�  D�@ D��HD�D��D�@ D��HD���D��qD�@ D��HD�� D�  D�@ D�}qD��qD�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�@ D��HD�� D���D�>�D�}qD��qD���D�B�D�~�D���D��D�AHD���D��HD���D�@ D�� D��HD�HD�AHD�� D��qD���D�@ D�~�D��qD�HD�@ D�~�D�� D�HD�@ D��HD�� D�  D�B�D���D�� D��qD�>�D��HD�� D�  D�@ D�}qD���D���D�@ D�� D���D�  D�AHD�� D�� D�HD�@ D�� D�� D�  D�>�D�~�D�� D�HD�@ D�� D��HD�HD�AHD���D��HD��D�AHD��HD�� D�HD�AHD��HD��HD�HD�@ D��HD��HD��D�B�D���D���D�  D�AHD�� D��HD���D�>�D��HD�� D���D�>�D�~�D���D���D�B�D��HD�� D�  D�@ D��HD��HD�HD�@ D��HD�� D���D�@ D���D�� D�HD�@ D�� D��HD���D�@ D�� D�� D���D�>�D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�HD�AHD��HD�� D���D�@ D�~�D�� D�HD�AHDÁHD��HD�  D�AHDĀ Dľ�D���D�>�Dŀ D�� D�  D�@ Dƀ D�� D�  D�>�D�~�D�� D�  D�@ DȁHDȾ�D�  D�@ D�~�D�� D�HD�AHDʀ D�� D�  D�AHDˀ D˾�D���D�>�D�~�D�� D�  D�>�D�}qD�� D�  D�@ D΁HD�� D�  D�@ D�~�DϾ�D�  D�@ DЁHD�� D���D�@ Dр DѾ�D���D�@ DҀ D��HD�  D�AHDӀ D�� D�  D�AHDԀ DԾ�D���D�@ DՁHDսqD���D�>�Dր D��HD�  D�>�D�~�D��HD�HD�AHD؂�D�� D���D�@ DفHD��HD�HD�AHDځHD�� D�  D�@ D�~�D۾�D��qD�>�D�~�D�� D�HD�AHD݀ Dݾ�D�  D�@ Dހ D��HD��D�@ D߀ D��HD�HD�@ D�� D��HD���D�>�D�HD��HD�  D�@ D� D�� D�HD�>�D�~�D㾸D�  D�@ D� D�� D�HD�>�D�}qD�� D��D�AHD� D�� D�HD�@ D� D�D��D�AHD�HD�� D�  D�@ D� D�� D�  D�@ D� D��HD�HD�@ D�~�D뾸D�  D�AHD�HD쾸D�  D�AHD� D���D�HD�>�D�~�D�� D���D�=qD�~�D�� D���D�>�D��HD�� D�  D�AHD�HD�D���D�AHD�D�� D�HD�AHD� D�� D�HD�AHD�D��HD���D�>�D�� D��qD���D�@ D�~�D�� D��D�@ D�~�D���D���D�>�D�}qD�� D�  D�=qD�� D�D��G�O�>�?�?B�\?�  ?��R?�p�?�G�?��H@
=q@�R@333@@  @O\)@^�R@s33@��\@�=q@���@�(�@��
@���@�@�p�@���@У�@�Q�@�  @�=q@�z�@��HAG�A�A
=qA\)A�\AffA�A   A$z�A'�A,��A1G�A6ffA:=qA>{AB�\AG�AL��AP��AU�AY��A^�RAc�
Ah��Al��Aq�Aw
=A|(�A���A��\A��A��A�=qA�z�A�
=A�G�A�(�A��RA�G�A�33A�p�A�  A��HA��A�\)A�G�A��A�ffA���A�33A��A�
=A��A�(�A�{A�Q�A��HA��A�
=A���A˅A�{AУ�Aҏ\A���A�
=A��A�(�A�ffA��A�33A�A�Q�A�\A���A�RA�G�A�(�A�ffA�Q�A��HA�p�B   B ��B{B\)B��B�B�HB(�B	G�B
�\B�
B�B{B\)Bz�BB
=BQ�BG�B�\B�
BG�BffB\)Bz�B�B33B z�B!��B"�RB$  B%�B&�\B'�
B)�B*=qB+\)B,z�B-B/33B0Q�B1��B2�RB3�
B4��B6{B7�B8��B:{B;
=B<Q�B=��B?
=B@Q�BA��BB�RBC�
BD��BF=qBG�BI�BJ=qBK�BL��BM�BO�BP��BR{BS33BTQ�BUBW
=BXz�BYBZ�HB\(�B]��B^�HB`Q�Bap�Bb�RBd  Bep�Bf�RBh(�Bi�BjffBk�
Bm�Bn�\Bo�Bp��Br{Bs33Btz�Bu�Bw33Bxz�Byp�Bz�RB|  B}G�B~�\B�B�ffB���B��B�(�B���B�\)B��B�z�B��B��
B�ffB���B��B�(�B��HB���B�(�B���B�\)B�  B��\B�G�B��B��\B�33B��B�Q�B��HB���B�(�B���B���B�{B���B�\)B�  B��\B�G�B��B��\B�33B��B��\B�33B�B�Q�B��HB��B�(�B��HB��B�(�B���B�\)B��B�z�B��B��
B�z�B��B��B�=qB���B�p�B�{B���B�\)B�  B���B�33B�B�ffB���B���B�=qB��HB��B�(�B��RB�\)B��B�z�B��B���B�{B��\B���B�\)B�B�{B�z�B���B�33B��B��
B�(�B�z�B��HB�G�B���B�  B�ffB��RB��B��B��B�=qB���B���B�\)B��B�{B�ffB���B��B�p�B�B�(�B�z�B���B�33B��B��
B�=qB�z�B��HB�33BÙ�B��B�Q�Bģ�B�
=B�p�B��
B�=qBƣ�B�
=B�p�B��
B�Q�Bȣ�B�
=BɅB��B�Q�Bʣ�B��B˅B��B�Q�B̸RB��B͙�B�  B�z�B��HB�\)B�B�(�BЏ\B���B�p�B��B�ffB���B�33BӮB�(�Bԣ�B�
=Bՙ�B�  B�z�B���BׅB��B�ffB��HB�\)B��
B�=qB���B�33B�B�=qBܸRB�G�B�B�=qB���B�G�B�B�Q�B���B�\)B��
B�Q�B��HB�\)B��
B�Q�B��HB�\)B��B�z�B���B�B�  B�\B��B�B�=qB���B�\)B��B�z�B���B�B�  B�\B��B�B�(�B�RB�G�B�B�Q�B��HB�p�B��B�z�B�
=B��B�{B��\B��B���B�(�B��RB�G�B��
B�ffB���B��B�{B���B�33B�B�=qB���B�G�B��
C 33C �C ��C{CffC��C��C=qCz�C��C
=CQ�C�\C�
C�CffC�RC  CG�C��C�HC(�Cp�C�RC  CG�C�C�
C{C\)C�C��C	G�C	�\C	�HC
(�C
z�C
C
=CQ�C��C�HC(�Cp�C�RC  CQ�C��C�HC33Cz�C�RC  CG�C�C��C�CffC�C��C=qC�C��C
=CQ�C��C�HC33Cz�C��C�CffC�C��C=qCz�C��C�CffC�RC  CG�C�\C��C{CQ�C��C�HC33C�C��C
=CQ�C�\C�HC(�Cp�C��C��CG�C�\CC
=CQ�C��C�C(�CffC�C  CG�C�C��C 
=C \)C ��C �C!(�C!ffC!�C"  C"=qC"�\C"��C#
=C#Q�C#��C#�HC$(�C$p�C$�RC$��C%33C%�C%�
C&{C&G�C&�\C&�HC'(�C'p�C'�C'��C(33C(�C(��C)
=C)G�C)�C)��C*{C*\)C*�\C*��C+{C+\)C+��C+�
C,�C,ffC,��C,�
C-{C-\)C-��C-��C-��C.=qC.p�C.�\C.�C.C.�HC/  C/�C/33C/G�C/\)C/ffC/z�C/�\C/�C/C/�
C/�HC/��C0
=C0�C0=qC0\)C0p�C0�C0�\C0��C0C0�HC0��C1
=C1�C133C1Q�C1p�C1�C1��C1�C1C1�
C1��C2{C233C2=qC2Q�C2p�C2�C2��C2C2��C2�C3
=C3(�C3=qC3G�C3\)C3z�C3��C3�RC3��C3�HC4  C4(�C4=qC4\)C4ffC4�C4�C4C4�
C4��C5�C5G�C5\)C5ffC5�\C5�RC5�
C5�C6  C633C6Q�C6ffC6z�C6��C6��C6�HC6��C7�C7G�C7ffC7z�C7��C7C7�HC7��C8�C8=qC8ffC8z�C8��C8C8�C9  C9�C9Q�C9z�C9��C9�RC9�HC:  C:(�C:\)C:z�C:��C:�RC:�HC;{C;33C;\)C;p�C;��C;C;��C<{C<33C<Q�C<�\C<�C<��C<�C=(�C=G�C=ffC=��C=C=�HC>  C>33C>\)C>z�C>��C>�
C?  C?{C?G�C?p�C?��C?�C?�C@{C@=qC@Q�C@z�C@�RC@�
C@��CA�CAQ�CAz�CA��CACA�CB
=CB33CBffCB�CB��CB��CC  CC{CC=qCCp�CC��CC�RCC�
CD
=CD(�CDG�CDz�CD��CDCD�CE�CE33CE\)CE�\CE�CE��CF  CF(�CFG�CFp�CF��CF��CF�HCG
=CG=qCGffCG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A��A��#A��#A��/A��/A��/A��/A��/A��/A�AҴ9Aҥ�Aҥ�Aҟ�Aҡ�Aң�Aҝ�Aҕ�Aҗ�Aҏ\A҅AҁA�x�A�p�A�n�A�bNA�\)A�S�A�K�A�?}A�+A��A�VA���A��AѬA�VA�ȴA��#A�ĜA˙�A�~�A�A��A�M�A�A��^A�M�A�;dA�$�A�/A��A�G�A��\A�(�A��-A��+A�`BA�n�A���A�&�A�oA�A�A�l�A�=qA���A�-A�p�A��7A�/A�A���A�ĜA�ȴA�I�A�ȴA�A�r�A��uA�XA�  A�bNA}�A{dZAyx�Ar�\An�`Al �Ag\)A_�FA]�A[&�AW%AVjAR5?AO��ALȴAHbNAF��AD��AD�AC�7A@ZA>��A<�A;oA:n�A<�A;33A:A9hsA97LA9�A9�A9/A8�A8n�A61'A3�;A3��A3C�A2M�A0�A0jA0��A1��A/�A+�TA+�FA+`BA*1A)��A'dZA%�-A%O�A$�yA$5?A#�
A#t�A"��A"bA!�PA!/A �A v�A �A�A�/AM�A�RAbA  A�A�;A�#AA��At�AS�A��AI�AO�A�A��A�!A��AJA�AE�A�AƨA�7A33A�A5?A�#A��A�wA��A��A��A��A�-A�^AA��A��AVA �Ap�AVAn�A9XA9XA �A�;AƨA��A�7A�AM�A�A��Ap�AO�A"�A�/A�uAQ�A1AXA
�A
r�A	�;A	t�A	XA	G�A	oA��A�AbNA��A�-AdZA7LA�AoA��A��Av�A-A��A�
A��AS�A�yA^5A{A�mA��A�A`BA33AȴA�uAffA5?A�wA33AoA ĜA (�@���@�n�@�@��@�V@���@�1@�v�@�@��@�l�@�E�@�`B@�%@��/@���@�\)@�-@�7L@�9@@�K�@�+@���@�\@�-@��#@���@��@�|�@�V@�^@�V@���@�Q�@�F@�R@�-@��/@�b@�+@���@�^5@��@���@��m@�\)@��y@�~�@�=q@�`B@�1'@ۅ@�"�@�@�~�@�x�@���@���@���@���@�dZ@�;d@�
=@�n�@���@�G�@ԓu@�(�@ӶF@ӕ�@�l�@�\)@�C�@�33@���@��@�p�@��@��m@υ@϶F@�@�^5@��@�r�@�(�@˾w@�l�@ʰ!@�V@���@�@�p�@��@�1'@�|�@�"�@��y@���@�^5@�%@�(�@��@ÍP@�"�@�ff@���@��@��T@���@�@���@�X@���@�z�@�|�@�V@��#@�X@�Z@���@��@�t�@�;d@�"�@�o@��y@���@�V@�@�hs@���@��@��@���@�l�@��H@��+@�5?@���@�&�@��u@��m@�K�@��@��!@�M�@�J@���@�hs@�?}@��@��D@�I�@�(�@� �@�1@��m@�K�@�ȴ@��+@�^5@�-@�@�O�@��9@�1'@���@�K�@�+@�o@���@�{@��^@���@���@��@�7L@��/@��@�Q�@��@�ƨ@���@�\)@�+@��@�
=@��H@��@���@�^5@�@�O�@��@��@�Q�@���@���@�l�@��R@�~�@�ff@�-@���@��@�?}@�V@��j@�Q�@���@���@�33@�v�@��7@���@��D@� �@��w@�t�@�K�@�+@�@�n�@��#@��#@���@��@�/@��`@�Ĝ@�Q�@��m@�ƨ@���@��@�|�@�\)@���@��@���@��-@�&�@�r�@���@�33@��H@���@�~�@�J@���@��^@���@�hs@�&�@��@�%@��u@�A�@���@��P@�dZ@�K�@�33@�ȴ@��@��^@�O�@���@�j@� �@���@���@��@�t�@�C�@��@�@���@��y@���@�=q@���@���@�p�@�/@��@�Ĝ@��9@�bN@��@�C�@��@�@��@��H@��R@�n�@�-@��@��@��@���@���@��/@���@�Q�@�b@���@��@��H@�n�@�@��#@���@�p�@��@�z�@�@+@}�@}p�@}�@|��@|(�@{ƨ@{�@{dZ@{"�@z^5@y�@yG�@xA�@w+@v�y@v{@uV@t�j@t�D@t9X@s��@s@r��@r�\@rM�@r=q@r-@rJ@q�#@qx�@p��@o�w@n�R@n$�@m�@m�@m�-@m�@mO�@lj@l9X@kS�@jn�@j-@jJ@i��@i�#@i�^@ix�@i&�@hr�@g�@gl�@g;d@g�@fȴ@f��@fE�@e�T@e/@d��@d(�@cƨ@c�F@c�F@c��@c�@c"�@b=q@a��@`��@`1'@_�@_|�@^��@]�-@]`B@]/@]V@\�/@\�@\��@\��@\9X@[�F@[dZ@Z�H@Z��@Z��@ZM�@Y��@Y��@Yhs@Y&�@X�`@X�9@Xr�@W�@W
=@V��@U�@U@U�h@UO�@U?}@U�@T��@T�@T��@T�/@T�@S��@St�@SC�@S"�@R�@R��@R��@R�!@Rn�@Q�#@Q&�@Q%@P��@P�`@P��@PQ�@O�w@N��@N�+@M@L��@L��@L�j@Lj@L(�@K�m@K�
@Kƨ@K��@Kt�@J�H@J��@Jn�@J�@I�^@IX@I%@H��@H��@H��@H�9@H�u@HQ�@H  @G�@Gl�@G+@F�+@F$�@E�-@E?}@D��@D�j@Dj@D1@C�m@C�F@CdZ@C"�@B��@B�!@B~�@Bn�@B-@A�^@A��@A��@A�7@A%@@r�@@  @?�;@?��@?
=@>�+@>$�@=@=��@=/@<��@<�@;��@:��@9��@9X@9G�@8��@8Ĝ@8��@8r�@8  @7l�@7;d@7
=@6��@6{@5@5�h@5/@4��@4�D@3�m@3�F@3t�@3"�@2�@2�\@2J@2J@1��@1x�@1x�@1x�@1G�@1�@0��@0��@0bN@0A�@0  @/l�@/�@.�@.��@.5?@-�@-��@-�h@-?}@,��@,��@,Z@+��@+o@*��@*-@)�#@)�7@)7L@)7L@)&�@(��@(�u@(1'@'�P@'|�@'\)@&��@&5?@%�T@%�-@%�@%O�@$�/@$�D@$I�@$9X@$(�@#��@#��@#�@#t�@#dZ@#C�@"��@"~�@"M�@"�@!��@!�^@!x�@!X@!7L@!%@ �u@ �@ r�@ r�@ r�@ bN@ bN@ Q�@ Q�@ Q�@�@��@�w@�w@�w@�@��@��@��@�P@��@�R@�R@�+@E�@�T@�-@��@z�@z�@z�@j@9X@(�@Z@��@z�@Z@9X@9X@��@��@"�@o@�H@��@�\@n�@-@��@�@�^@hs@G�@�@��@��@Ĝ@��@bN@ �@�w@|�@|�@l�@\)@;d@
=@ȴ@�+@ff@V@$�@�@�T@��@�-@�h@�@`B@?}@/@�@�@�@Z@�@��@�m@��@dZ@C�@"�@�@��@~�@=q@��@x�@G�@�@�`@Ĝ@�9@�u@Q�@b@  @�;@��@�P@�P@|�@\)@;d@�y@��@ff@V@5?@$�@�T@�T@�T@�-@p�@?}@�@V@�@�D@��@�D@�D@z�@z�@z�@z�@j@9X@��@��@S�@C�@C�@C�@C�A��
A��A���A���A��
A��#A��A��A��
A��A��/A��A��A��#A��#A��/A��#A��/A��HA��;A��#A��A��;A��HA��;A��#A��/A��;A��/A��#A��#A��;A��;A��;A��#A��/A��;A��/A��#A��AҺ^AҺ^A���AҶFAҲ-AҲ-AҰ!Aҩ�Aҡ�Aң�Aң�Aҧ�AҬAҧ�Aң�Aҥ�AҬAҧ�Aҡ�Aҥ�Aҧ�Aҩ�Aҥ�Aҡ�Aҙ�Aҝ�Aҝ�Aҡ�Aҟ�Aҝ�Aқ�Aҧ�Aң�Aҡ�Aң�Aҡ�Aҟ�Aҥ�Aҥ�Aң�Aҡ�Aң�Aҥ�Aҡ�Aқ�Aҙ�Aҝ�Aҙ�Aҕ�Aҗ�AғuAғuAҍPAҏ\Aҗ�Aқ�Aҝ�Aҝ�Aҗ�AғuAғuAҗ�Aҕ�Aҕ�AғuAҕ�AғuA҇+A҃A҃A҇+A҉7A҅A҅A҅A҇+A҇+A҃A�~�A�~�AҁAҁA�~�A�z�A�x�A�v�A�x�A�x�A�v�A�p�A�n�A�p�A�t�A�p�A�l�A�jA�n�A�r�A�p�A�l�A�l�A�jA�jA�jA�hsA�bNA�^5A�`BA�^5A�^5A�\)A�^5A�\)A�\)A�XA�VA�XA�XA�XA�Q�A�O�A�M�A�M�A�Q�A�O�A�M�A�G�A�G�A�G�A�C�A�C�A�A�A�A�A�=qA�7LA�/A�/A�-A�+A�&�A�"�A��A� �A��A��A��A�{A�bA�oA�{A�bA�JA�1A�1A�%A���A���A��A��A��A��A��TA��/A���A���A�ĜAѼjAѼjAѶFAѮAѣ�Aџ�Aщ7A�z�A�dZA�S�A�I�A�E�A�;dA�"�A�oA���A��A�ƨAЛ�A�Q�A���A�t�A���AμjA�n�A�C�Aͥ�A�p�A�A̼jA�hsA�$�A�  A���A��HA˺^A˙�A�M�A�$�A�VA���A�ĜA�z�A�7LA��#AɑhA�v�A�K�A�bA���A�~�A�^5A�K�A�;dA�"�A�bA��A��#A�ƨA�r�AƸRA�\)A�ĜA�(�A��A�dZAÕ�Aº^A�"�A��uA�1'A�oA���A�oA���A�bA���A�$�A��A�n�A�=qA�"�A��#A���A�%A�z�A�hsA��A���A��jA�S�A���A�t�A��A��A��HA�%A��7A�ffA�?}A��A�
=A�A��A��;A��PA��RA�9XA���A�dZA�5?A�"�A��A�%A���A���A��mA���A�ĜA��RA���A���A��DA�v�A�hsA�S�A�5?A�+A�&�A�$�A��A��A�oA���A���A���A�ƨA���A��9A��uA�\)A�7LA�1A��#A���A�ffA�S�A�M�A�G�A�?}A�&�A��/A���A�n�A� �A�
=A�  A��yA���A��!A���A�^5A�%A��A��A��#A�ȴA��^A��-A��DA�A�A��yA��A�C�A��A���A�VA�33A�bA���A��!A�p�A�/A��yA��A�`BA�5?A��A��/A���A��A�XA�(�A���A��9A��DA�=qA��A��RA���A�|�A�M�A���A��^A��PA�hsA�M�A�9XA�$�A�{A�A���A��HA��^A��hA�|�A�\)A�&�A�ƨA�XA���A��/A���A���A�ƨA��RA��A���A���A���A���A�|�A�hsA�Q�A�5?A�(�A���A��RA�O�A��A���A���A��wA��FA��DA�=qA��yA��FA�t�A�"�A��mA��A��A�S�A�5?A�bA��A��-A�hsA�/A���A�~�A�/A��#A��A�G�A��/A���A�x�A�S�A�I�A�7LA��A��A���A�M�A��A���A�?}A��yA�hsA� �A���A�x�A�5?A�(�A� �A�oA��mA���A�ȴA��^A���A���A���A�t�A�5?A���A�&�A���A�dZA�+A��/A��9A���A��\A�p�A�jA�hsA�^5A�VA�E�A�=qA�-A��A�
=A��TA��;A���A��FA���A�A�A��/A�l�A�=qA��A��A�A�A�A���A���A�l�A�-A�%A��yA�A�$�A��FA�&�A��FA�l�A�p�A��A���A�bNA�XA�Q�A�A�A��A��`A��^A��A�/A�t�A�=qA�33A���A���A��uA�z�A�`BA�S�A�A�A�1'A��A��A~n�A}�A}�PA|��A|^5A|-A| �A|bA{�^A{hsAz��AzZAz(�Az �Az�Az�AzJAy�#Ay%Aw��Au�FAtQ�As�-AsG�Arv�Aq?}Ap-Ao��Aol�AoG�Ao
=An��An��Anz�An=qAm��AmXAl�!Ak�TAk7LAj��Aj��Ajv�AiAhȴAh(�Ag;dAfZAe��AdM�Ab�Aa�mA`��A^�A^��A^�uA^z�A^VA^JA]��A]��A]x�A]7LA]�A\�/A\z�A\v�A\r�A\jA\ffA\ffA\ZA\E�A\$�A[��A[��A[��A[G�A[+A[�A[oA[AZ��AZ�AZ�yAZ�`AZ�/AZ��AZĜAZ�jAZ��AY�AXĜAW�AW��AWO�AW�AW
=AW
=AWVAWVAW
=AW%AV��AWAV��AV��AV�/AV��AV�AV��AV��AV��AV��AV��AV��AV��AV��AVĜAV�RAV��AV�RAV�!AV�\AVz�AV�AVI�AVE�AV1'AV�AU��AU�;AU��AUx�AU"�ATȴAT^5AS�mASp�AR��ARbNAQ�;AQhsAQ/AQ�AP�/APĜAP��AP��AP��APjAPE�AP5?AP�APbAO�AO�^AO|�AOt�AO`BAO\)AO\)AOdZAOXAOK�AOXAOO�AO?}AO"�AN��AN��AN�AM��AMAL(�AKhsAJ��AJbNAI��AI��AIO�AI�AH�yAH�AHz�AHZAHVAHI�AH1'AH�AH1AG��AG�mAGƨAG�-AG��AGXAG&�AG�AF��AF�yAF�AF�AFr�AF �AE�
AE��AE�AE|�AE`BAE?}AE&�AD��AD�`AD��AD��AD�!AD��AD�DADv�ADffADQ�AD=qAD1'AD(�AD$�AD�AD1AD  AC��AC�AC�TAC�mAC�mAC�;AC�
AC�#AC��AC��AC��AC�FAC��AC`BAC+AB�/AB��AB5?AA��AA�hAA7LA@�RA@9XA@  A?�wA?�^A?�A?hsA?dZA?S�A?K�A?C�A??}A?+A?�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             A��
A��A��#A��#A��/A��/A��/A��/A��/A��/A�AҴ9Aҥ�Aҥ�Aҟ�Aҡ�Aң�Aҝ�Aҕ�Aҗ�Aҏ\A҅AҁA�x�A�p�A�n�A�bNA�\)A�S�A�K�A�?}A�+A��A�VA���A��AѬA�VA�ȴA��#A�ĜA˙�A�~�A�A��A�M�A�A��^A�M�A�;dA�$�A�/A��A�G�A��\A�(�A��-A��+A�`BA�n�A���A�&�A�oA�A�A�l�A�=qA���A�-A�p�A��7A�/A�A���A�ĜA�ȴA�I�A�ȴA�A�r�A��uA�XA�  A�bNA}�A{dZAyx�Ar�\An�`Al �Ag\)A_�FA]�A[&�AW%AVjAR5?AO��ALȴAHbNAF��AD��AD�AC�7A@ZA>��A<�A;oA:n�A<�A;33A:A9hsA97LA9�A9�A9/A8�A8n�A61'A3�;A3��A3C�A2M�A0�A0jA0��A1��A/�A+�TA+�FA+`BA*1A)��A'dZA%�-A%O�A$�yA$5?A#�
A#t�A"��A"bA!�PA!/A �A v�A �A�A�/AM�A�RAbA  A�A�;A�#AA��At�AS�A��AI�AO�A�A��A�!A��AJA�AE�A�AƨA�7A33A�A5?A�#A��A�wA��A��A��A��A�-A�^AA��A��AVA �Ap�AVAn�A9XA9XA �A�;AƨA��A�7A�AM�A�A��Ap�AO�A"�A�/A�uAQ�A1AXA
�A
r�A	�;A	t�A	XA	G�A	oA��A�AbNA��A�-AdZA7LA�AoA��A��Av�A-A��A�
A��AS�A�yA^5A{A�mA��A�A`BA33AȴA�uAffA5?A�wA33AoA ĜA (�@���@�n�@�@��@�V@���@�1@�v�@�@��@�l�@�E�@�`B@�%@��/@���@�\)@�-@�7L@�9@@�K�@�+@���@�\@�-@��#@���@��@�|�@�V@�^@�V@���@�Q�@�F@�R@�-@��/@�b@�+@���@�^5@��@���@��m@�\)@��y@�~�@�=q@�`B@�1'@ۅ@�"�@�@�~�@�x�@���@���@���@���@�dZ@�;d@�
=@�n�@���@�G�@ԓu@�(�@ӶF@ӕ�@�l�@�\)@�C�@�33@���@��@�p�@��@��m@υ@϶F@�@�^5@��@�r�@�(�@˾w@�l�@ʰ!@�V@���@�@�p�@��@�1'@�|�@�"�@��y@���@�^5@�%@�(�@��@ÍP@�"�@�ff@���@��@��T@���@�@���@�X@���@�z�@�|�@�V@��#@�X@�Z@���@��@�t�@�;d@�"�@�o@��y@���@�V@�@�hs@���@��@��@���@�l�@��H@��+@�5?@���@�&�@��u@��m@�K�@��@��!@�M�@�J@���@�hs@�?}@��@��D@�I�@�(�@� �@�1@��m@�K�@�ȴ@��+@�^5@�-@�@�O�@��9@�1'@���@�K�@�+@�o@���@�{@��^@���@���@��@�7L@��/@��@�Q�@��@�ƨ@���@�\)@�+@��@�
=@��H@��@���@�^5@�@�O�@��@��@�Q�@���@���@�l�@��R@�~�@�ff@�-@���@��@�?}@�V@��j@�Q�@���@���@�33@�v�@��7@���@��D@� �@��w@�t�@�K�@�+@�@�n�@��#@��#@���@��@�/@��`@�Ĝ@�Q�@��m@�ƨ@���@��@�|�@�\)@���@��@���@��-@�&�@�r�@���@�33@��H@���@�~�@�J@���@��^@���@�hs@�&�@��@�%@��u@�A�@���@��P@�dZ@�K�@�33@�ȴ@��@��^@�O�@���@�j@� �@���@���@��@�t�@�C�@��@�@���@��y@���@�=q@���@���@�p�@�/@��@�Ĝ@��9@�bN@��@�C�@��@�@��@��H@��R@�n�@�-@��@��@��@���@���@��/@���@�Q�@�b@���@��@��H@�n�@�@��#@���@�p�@��@�z�@�@+@}�@}p�@}�@|��@|(�@{ƨ@{�@{dZ@{"�@z^5@y�@yG�@xA�@w+@v�y@v{@uV@t�j@t�D@t9X@s��@s@r��@r�\@rM�@r=q@r-@rJ@q�#@qx�@p��@o�w@n�R@n$�@m�@m�@m�-@m�@mO�@lj@l9X@kS�@jn�@j-@jJ@i��@i�#@i�^@ix�@i&�@hr�@g�@gl�@g;d@g�@fȴ@f��@fE�@e�T@e/@d��@d(�@cƨ@c�F@c�F@c��@c�@c"�@b=q@a��@`��@`1'@_�@_|�@^��@]�-@]`B@]/@]V@\�/@\�@\��@\��@\9X@[�F@[dZ@Z�H@Z��@Z��@ZM�@Y��@Y��@Yhs@Y&�@X�`@X�9@Xr�@W�@W
=@V��@U�@U@U�h@UO�@U?}@U�@T��@T�@T��@T�/@T�@S��@St�@SC�@S"�@R�@R��@R��@R�!@Rn�@Q�#@Q&�@Q%@P��@P�`@P��@PQ�@O�w@N��@N�+@M@L��@L��@L�j@Lj@L(�@K�m@K�
@Kƨ@K��@Kt�@J�H@J��@Jn�@J�@I�^@IX@I%@H��@H��@H��@H�9@H�u@HQ�@H  @G�@Gl�@G+@F�+@F$�@E�-@E?}@D��@D�j@Dj@D1@C�m@C�F@CdZ@C"�@B��@B�!@B~�@Bn�@B-@A�^@A��@A��@A�7@A%@@r�@@  @?�;@?��@?
=@>�+@>$�@=@=��@=/@<��@<�@;��@:��@9��@9X@9G�@8��@8Ĝ@8��@8r�@8  @7l�@7;d@7
=@6��@6{@5@5�h@5/@4��@4�D@3�m@3�F@3t�@3"�@2�@2�\@2J@2J@1��@1x�@1x�@1x�@1G�@1�@0��@0��@0bN@0A�@0  @/l�@/�@.�@.��@.5?@-�@-��@-�h@-?}@,��@,��@,Z@+��@+o@*��@*-@)�#@)�7@)7L@)7L@)&�@(��@(�u@(1'@'�P@'|�@'\)@&��@&5?@%�T@%�-@%�@%O�@$�/@$�D@$I�@$9X@$(�@#��@#��@#�@#t�@#dZ@#C�@"��@"~�@"M�@"�@!��@!�^@!x�@!X@!7L@!%@ �u@ �@ r�@ r�@ r�@ bN@ bN@ Q�@ Q�@ Q�@�@��@�w@�w@�w@�@��@��@��@�P@��@�R@�R@�+@E�@�T@�-@��@z�@z�@z�@j@9X@(�@Z@��@z�@Z@9X@9X@��@��@"�@o@�H@��@�\@n�@-@��@�@�^@hs@G�@�@��@��@Ĝ@��@bN@ �@�w@|�@|�@l�@\)@;d@
=@ȴ@�+@ff@V@$�@�@�T@��@�-@�h@�@`B@?}@/@�@�@�@Z@�@��@�m@��@dZ@C�@"�@�@��@~�@=q@��@x�@G�@�@�`@Ĝ@�9@�u@Q�@b@  @�;@��@�P@�P@|�@\)@;d@�y@��@ff@V@5?@$�@�T@�T@�T@�-@p�@?}@�@V@�@�D@��@�D@�D@z�@z�@z�@z�@j@9X@��@��@S�@C�@C�@C�G�O�A��
A��A���A���A��
A��#A��A��A��
A��A��/A��A��A��#A��#A��/A��#A��/A��HA��;A��#A��A��;A��HA��;A��#A��/A��;A��/A��#A��#A��;A��;A��;A��#A��/A��;A��/A��#A��AҺ^AҺ^A���AҶFAҲ-AҲ-AҰ!Aҩ�Aҡ�Aң�Aң�Aҧ�AҬAҧ�Aң�Aҥ�AҬAҧ�Aҡ�Aҥ�Aҧ�Aҩ�Aҥ�Aҡ�Aҙ�Aҝ�Aҝ�Aҡ�Aҟ�Aҝ�Aқ�Aҧ�Aң�Aҡ�Aң�Aҡ�Aҟ�Aҥ�Aҥ�Aң�Aҡ�Aң�Aҥ�Aҡ�Aқ�Aҙ�Aҝ�Aҙ�Aҕ�Aҗ�AғuAғuAҍPAҏ\Aҗ�Aқ�Aҝ�Aҝ�Aҗ�AғuAғuAҗ�Aҕ�Aҕ�AғuAҕ�AғuA҇+A҃A҃A҇+A҉7A҅A҅A҅A҇+A҇+A҃A�~�A�~�AҁAҁA�~�A�z�A�x�A�v�A�x�A�x�A�v�A�p�A�n�A�p�A�t�A�p�A�l�A�jA�n�A�r�A�p�A�l�A�l�A�jA�jA�jA�hsA�bNA�^5A�`BA�^5A�^5A�\)A�^5A�\)A�\)A�XA�VA�XA�XA�XA�Q�A�O�A�M�A�M�A�Q�A�O�A�M�A�G�A�G�A�G�A�C�A�C�A�A�A�A�A�=qA�7LA�/A�/A�-A�+A�&�A�"�A��A� �A��A��A��A�{A�bA�oA�{A�bA�JA�1A�1A�%A���A���A��A��A��A��A��TA��/A���A���A�ĜAѼjAѼjAѶFAѮAѣ�Aџ�Aщ7A�z�A�dZA�S�A�I�A�E�A�;dA�"�A�oA���A��A�ƨAЛ�A�Q�A���A�t�A���AμjA�n�A�C�Aͥ�A�p�A�A̼jA�hsA�$�A�  A���A��HA˺^A˙�A�M�A�$�A�VA���A�ĜA�z�A�7LA��#AɑhA�v�A�K�A�bA���A�~�A�^5A�K�A�;dA�"�A�bA��A��#A�ƨA�r�AƸRA�\)A�ĜA�(�A��A�dZAÕ�Aº^A�"�A��uA�1'A�oA���A�oA���A�bA���A�$�A��A�n�A�=qA�"�A��#A���A�%A�z�A�hsA��A���A��jA�S�A���A�t�A��A��A��HA�%A��7A�ffA�?}A��A�
=A�A��A��;A��PA��RA�9XA���A�dZA�5?A�"�A��A�%A���A���A��mA���A�ĜA��RA���A���A��DA�v�A�hsA�S�A�5?A�+A�&�A�$�A��A��A�oA���A���A���A�ƨA���A��9A��uA�\)A�7LA�1A��#A���A�ffA�S�A�M�A�G�A�?}A�&�A��/A���A�n�A� �A�
=A�  A��yA���A��!A���A�^5A�%A��A��A��#A�ȴA��^A��-A��DA�A�A��yA��A�C�A��A���A�VA�33A�bA���A��!A�p�A�/A��yA��A�`BA�5?A��A��/A���A��A�XA�(�A���A��9A��DA�=qA��A��RA���A�|�A�M�A���A��^A��PA�hsA�M�A�9XA�$�A�{A�A���A��HA��^A��hA�|�A�\)A�&�A�ƨA�XA���A��/A���A���A�ƨA��RA��A���A���A���A���A�|�A�hsA�Q�A�5?A�(�A���A��RA�O�A��A���A���A��wA��FA��DA�=qA��yA��FA�t�A�"�A��mA��A��A�S�A�5?A�bA��A��-A�hsA�/A���A�~�A�/A��#A��A�G�A��/A���A�x�A�S�A�I�A�7LA��A��A���A�M�A��A���A�?}A��yA�hsA� �A���A�x�A�5?A�(�A� �A�oA��mA���A�ȴA��^A���A���A���A�t�A�5?A���A�&�A���A�dZA�+A��/A��9A���A��\A�p�A�jA�hsA�^5A�VA�E�A�=qA�-A��A�
=A��TA��;A���A��FA���A�A�A��/A�l�A�=qA��A��A�A�A�A���A���A�l�A�-A�%A��yA�A�$�A��FA�&�A��FA�l�A�p�A��A���A�bNA�XA�Q�A�A�A��A��`A��^A��A�/A�t�A�=qA�33A���A���A��uA�z�A�`BA�S�A�A�A�1'A��A��A~n�A}�A}�PA|��A|^5A|-A| �A|bA{�^A{hsAz��AzZAz(�Az �Az�Az�AzJAy�#Ay%Aw��Au�FAtQ�As�-AsG�Arv�Aq?}Ap-Ao��Aol�AoG�Ao
=An��An��Anz�An=qAm��AmXAl�!Ak�TAk7LAj��Aj��Ajv�AiAhȴAh(�Ag;dAfZAe��AdM�Ab�Aa�mA`��A^�A^��A^�uA^z�A^VA^JA]��A]��A]x�A]7LA]�A\�/A\z�A\v�A\r�A\jA\ffA\ffA\ZA\E�A\$�A[��A[��A[��A[G�A[+A[�A[oA[AZ��AZ�AZ�yAZ�`AZ�/AZ��AZĜAZ�jAZ��AY�AXĜAW�AW��AWO�AW�AW
=AW
=AWVAWVAW
=AW%AV��AWAV��AV��AV�/AV��AV�AV��AV��AV��AV��AV��AV��AV��AV��AVĜAV�RAV��AV�RAV�!AV�\AVz�AV�AVI�AVE�AV1'AV�AU��AU�;AU��AUx�AU"�ATȴAT^5AS�mASp�AR��ARbNAQ�;AQhsAQ/AQ�AP�/APĜAP��AP��AP��APjAPE�AP5?AP�APbAO�AO�^AO|�AOt�AO`BAO\)AO\)AOdZAOXAOK�AOXAOO�AO?}AO"�AN��AN��AN�AM��AMAL(�AKhsAJ��AJbNAI��AI��AIO�AI�AH�yAH�AHz�AHZAHVAHI�AH1'AH�AH1AG��AG�mAGƨAG�-AG��AGXAG&�AG�AF��AF�yAF�AF�AFr�AF �AE�
AE��AE�AE|�AE`BAE?}AE&�AD��AD�`AD��AD��AD�!AD��AD�DADv�ADffADQ�AD=qAD1'AD(�AD$�AD�AD1AD  AC��AC�AC�TAC�mAC�mAC�;AC�
AC�#AC��AC��AC��AC�FAC��AC`BAC+AB�/AB��AB5?AA��AA�hAA7LA@�RA@9XA@  A?�wA?�^A?�A?hsA?dZA?S�A?K�A?C�A??}A?+A?�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�jB�jB�5B�jB�jB�jB�jB�5B�5B�5B�HB�jB�jB�jB��BޞBޞB��B��B�BߤB��B�BB�B�NB�B�B��B�B��B�
B��B��B�/B�oB�%B��B	�B	9XB	��B
GEB
�OB
ܒB�B1'B^jB�MB��B��B�RB�^B�LB�UB��B�B�B��B��B��B�|B�B͟B��B�9B��B�eB��Br�BTaB9XB�B;B
ܒB
�'B
�RB
��B
�MB
y�B
cTB
6�B
!bB	�B	�B	��B	��B	��B	��B	�AB	dZB	ZQB	HKB	$@B	�B��B��B��B��BɺB�EB��B��B��B��B�FB�jB�BB�qB��BچB	:B	<�B	S&B	Z�B	kB	�B	��B	�OB	҉B	��B	�B	ȴB	��B	�)B	�jB	��B	��B	�`B	��B	�`B	��B	��B
�B	��B	��B	��B	��B	�;B	�|B	�%B	�`B	��B	�VB	��B
B
B
B
B
�B
�B
JB
�B
�B
MB
�B
�B
�B
B
�B
�B
+B
_B
B
IB
OB
B
 \B
!-B
%zB
'B
#:B
#�B
#:B
#�B
%zB
#�B
$tB
)�B
)�B
)�B
*�B
,=B
.�B
4�B
7�B
9�B
;0B
=B
?�B
@�B
@�B
@�B
E�B
K�B
K�B
JXB
J#B
K�B
K^B
K^B
K�B
J�B
K^B
GEB
F�B
F�B
F�B
F�B
GB
G�B
HKB
G�B
GzB
H�B
EB
D�B
FtB
F?B
HKB
H�B
HB
GzB
GEB
H�B
F�B
FtB
E�B
EB
D�B
DgB
D�B
D3B
C�B
B�B
B�B
B�B
B�B
A�B
A�B
>�B
<jB
;0B
9�B
8�B
9$B
8�B
8�B
6�B
5�B
4�B
3�B
0�B
/�B
/�B
-�B
,B
,qB
+kB
+B
+kB
+�B
,�B
*�B
(XB
)�B
(XB
&LB
$�B
#�B
#:B
"�B
"�B
 �B
�B
OB
�B
�B
xB
xB
xB
IB
�B
B
xB
xB
qB
�B
7B
�B
7B
�B
�B
7B
_B
$B
B
{B
FB
�B
�B
B
FB
FB
B
�B
@B
B
:B
 B
�B
�B
�B
�B
�B
�B
hB
hB
�B
�B
 B
 B
�B
hB
�B
4B
hB
4B
 B
�B
bB
�B
�B
�B
bB
VB
xB
"B
\B
�B
PB

rB

�B
	�B
�B
�B
�B
�B
fB
�B

=B

=B
fB
	�B

=B

rB

rB
DB
	�B
	�B

�B

�B
B
�B
B
B
B
�B
�B
�B
B
�B
JB
�B
�B
�B
(B
�B
�B
(B
(B
�B
�B
�B
VB
�B
bB
B
�B
@B
B
uB
B
�B
�B
{B
�B
�B
�B
�B
SB
B
B
�B
�B
B
�B
MB
MB
MB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
�B
B
B
�B
B
7B
	B
=B
qB
�B
xB
�B
B
B
B
IB
�B
�B
xB
�B
�B
B
B
B
B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
OB
OB
�B
�B
�B
�B
!B
~B
B
B
�B
OB
�B
�B
VB
�B
!�B
!�B
!-B
"4B
#nB
$�B
$�B
$�B
%zB
%B
%�B
&B
%�B
%�B
%�B
&�B
%�B
%FB
$�B
$B
#B
#�B
#:B
#�B
#:B
#nB
$tB
%B
%B
%FB
%�B
&�B
&LB
&�B
'�B
'�B
($B
'�B
'�B
'�B
'RB
'�B
'RB
&�B
'�B
(�B
)*B
)_B
)_B
)�B
)�B
)�B
*0B
*�B
*�B
*�B
*�B
+B
+6B
+�B
,B
,B
,qB
,qB
,=B
,B
,�B
-wB
-�B
-�B
.IB
.B
.B
.}B
/OB
/OB
/�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
1[B
1[B
1�B
2�B
2�B
2�B
2-B
33B
3�B
49B
4B
4�B
5tB
5?B
5tB
5�B
6B
6B
6FB
6B
6B
6zB
6zB
6�B
7B
7�B
7�B
8RB
8�B
8RB
8RB
8�B
9$B
9$B
9�B
9�B
9�B
9�B
9�B
9�B
:*B
:^B
;0B
<B
=qB
>B
>B
>B
>�B
>�B
>�B
?�B
?B
@B
@�B
@�B
@�B
@�B
AUB
@�B
A B
A�B
B�B
C�B
C�B
C�B
C�B
D3B
D3B
D�B
D�B
FB
FB
FtB
F�B
F�B
FtB
F?B
F?B
F�B
GB
GzB
HKB
HKB
HKB
H�B
I�B
I�B
J#B
J#B
I�B
JXB
JXB
J#B
I�B
J�B
J�B
J�B
K�B
K)B
K)B
K�B
K�B
K)B
K�B
K�B
K�B
K�B
L0B
L0B
MB
MB
MjB
M6B
M6B
NB
MjB
M�B
M�B
M�B
MjB
M6B
M6B
NpB
N�B
OvB
OBB
OvB
OvB
O�B
OvB
O�B
P�B
O�B
O�B
O�B
O�B
O�B
P}B
P}B
QNB
PHB
Q�B
RTB
Q�B
RTB
R B
R�B
R�B
R�B
RTB
R�B
R�B
S&B
S[B
S[B
S�B
S�B
TaB
T�B
TaB
T�B
T�B
T,B
T�B
T�B
U2B
T�B
U�B
V9B
W?B
W�B
WsB
W�B
XB
XEB
X�B
XyB
X�B
X�B
XyB
X�B
X�B
X�B
YB
X�B
YB
YKB
YKB
YB
YB
Z�B
Z�B
[#B
[#B
[WB
[�B
\)B
\�B
\�B
\�B
]/B
]/B
]�B
]�B
^�B
_�B
_pB
_pB
_�B
`B
_�B
_�B
`vB
`�B
`�B
`�B
aB
a|B
a|B
a|B
a�B
bB
a�B
b�B
bB
b�B
c B
c B
c�B
c�B
c�B
d&B
dZB
d&B
c�B
dZB
dZB
dZB
d�B
d�B
d�B
d�B
e�B
e`B
e�B
e�B
e�B
f2B
e�B
ffB
ffB
f�B
f�B
gB
gmB
g�B
g�B
hsB
h�B
h�B
h�B
hsB
hsB
h�B
h�B
i�B
jB
i�B
i�B
kB
j�B
kQB
kB
kB
k�B
l"B
l"B
l�B
l�B
l�B
l�B
m)B
m]B
m)B
l�B
m]B
m�B
n/B
ncB
o B
n�B
o B
o5B
o5B
oiB
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qvB
qvB
qvB
q�B
q�B
q�B
q�B
q�B
qvB
q�B
rB
rB
rGB
r|B
r|B
sB
sMB
t�B
t�B
t�B
t�B
t�B
t�B
uZB
v�B
x8B
y	B
y>B
y>B
y>B
zxB
z�B
{JB
z�B
{B
{JB
{JB
{B
{�B
{�B
{�B
|�B
|�B
|�B
}"B
}"B
}"B
}"B
}VB
}VB
}�B
~(B
}�B
}�B
~(B
~(B
}�B
~(B
~�B
~�B
~�B
~�B
cB
�B
cB
�B
�B
�4B
� B
� B
�iB
�iB
�iB
�iB
�B
�;B
�;B
�oB
��B
��B
�B
�B
�AB
�AB
�uB
�uB
��B
�GB
�GB
�GB
��B
��B
��B
��B
��B
�MB
��B
�MB
��B
�B
�B
�B
��B
�B
�B
��B
�SB
�%B
��B
��B
��B
��B
�%B
�SB
�YB
�%B
��B
��B
�YB
�_B
�_B
��B
��B
�+B
��B
��B
��B
�+B
��B
�fB
��B
��B
��B
�B
��B
�7B
�1B�5B�5B�jB�B��B��BݘB�B�;BݘB�/B�jBޞB��B�5BݘBߤB�5BݘB�B�;BߤBݘB�dB�jB�;B�jB��B�jB�B�B��BݘB�5B�pB��B�/B�dB�5BߤB�B�pB��B��B�BBݘB��B�pB��B�B�B�]BܒB�pB�;B�BݘB��B�BޞB�B�/B��B�B�pB��B�dB�dB�pB�B��B�/BݘB�B�;B��BޞBݘBݘBޞBߤB�jB��B�B�vB��B��B�5B��B�jB�B�B�BB�BB��B�jBݘB�5B�vB�|B�5B�BޞB�B�B�pB��B�;B��B�vB�pB��B�BB��B�B�;B�jB��B�B��B�BBߤB�BB�HB�B�B��B�BB��B� B�B�B�B�B�B�B��B�B��B��B��B�B�TB�B�B�B�ZB��B��B�`B�&B�B�ZB��B��B��B�B��B�&B�2B�B��B��B�`B��B��B�B�mB�
B�>B�mB�mB�B�sB�B�B�B�KB�KB��B�B��B�B�B�B��B�cB�cB��B�WB��B� B�5B��B�B�iB��B�B�vB�B�B�B��B��B��B��B�>B��B��B�]B	 4B	oB	fB	B	:B	�B	�B	�B	�B	"4B	&LB	-B	5�B	9$B	D�B	XEB	}�B	�B	��B	�aB	��B	�B
�B
B
9�B
FtB
a|B
oiB
{B
}�B
�{B
�oB
�:B
��B
��B
��B
ŢB
� B
�QB
�B
��B
��B
	BBeB#�B0�B,qB,B-�B0�B2aB6�B5tB5tBH�BS�Ba�Bu%Bp�Bk�B{B��B��B�B�4B�oB��B�"B�{B��B�OB��B��BrB�B��B��B�=B��B�nB�=B��B��B��B�0B��B��B��B��B��B�B�<B��B��B�9B�UB�=B�kB�B�B�BB�aB�mB�tB��B��B�6B�dB�6B��B�<B�'B��B�mB�B��B�zB�BɺBɆB̘B�6BуBѷB��B��BҽB� B��BרB҉BѷB�NB�[B�EB��B��B��B�B�iB�AB�B��B� B� B�B��B��BoB;B��B��B�B��B�(B�VBBMB�>B�rB�xB�>B��B��B��B �B��B��B�B��B�(B�B��B��B�DB��B��B��B��B�+B�%B�cB�B��B�B��B�/B�B�B�`B��B�B�5B��BҽB�BB�&B҉BɺB��B� B��B�wB�B�B��B��B��B��B�B�3B��B��B�dB��B��B��B��B��B�B�!B�B��B��B�	B��B�kB��B�_B��B��B�CB�_B��B�JB�{B�BzBy�B}VB~]Bt�Bm�Bm�Bi�Bd�B\�B[�BTaBS&BR BQ�B?�BB'BL0B?HB49B3�B0�B+B(XB.B�B�BVB
=B�B�B	B
=B�B
�VB
�B
�B
�B
�B
�B
�)B
�B
�B
ȴB
�?B
�zB
ƨB
�'B
�HB
�}B
�BB
�0B
�dB
��B
� B
�B
��B
�B
��B
�7B
�OB
��B
�xB
��B
�	B
�+B
�1B
��B
�%B
�uB
�{B
~]B
� B
~�B
��B
w�B
uZB
y>B
tTB
��B
qAB
o�B
[�B
]�B
bB
[�B
NpB
L�B
b�B
+kB
,�B
%�B
 \B
#nB
&LB
%zB
_B
�B
)�B
&�B	��B	�fB	�B	�,B	�ZB	�fB	�8B	�B	ܒB	��B	��B	�|B	�fB	��B	�vB	�6B	��B	�<B	�XB	�B	�?B	��B	��B	�?B	��B	�*B	��B	�hB	�=B	�@B	��B	��B	��B	��B	�@B	�tB	��B	��B	�B	� B	��B	��B	��B	��B	�RB	�VB	z�B	y�B	�B	�{B	s�B	o�B	j�B	e�B	h
B	c�B	^�B	`B	aHB	`BB	d&B	]�B	e�B	VmB	O�B	K�B	L�B	P�B	R�B	F?B	K)B	=qB	<B	I�B	<6B	/OB	E9B	$B	�B	�B	�B	@B	B	$B	�B	�B		�B	�B	
�B	�B	�B	�B	�B	�B	�B	SB	B	{B	�B	MB	MB	AB�(B�"B�B�B�"B�JB��B�>B�8B�B��B��B��B		lB�]B��B��B��B�yB�mB�B�fB��B�B�mB�sB��B��B�B�sB�B�
B�DB�8B�B�B�DB��B�fB�2B��B�8B�fB�mB�
B�2B�"B�B��B�B��B�B��B�B�B�B�B�&B��B�WBیB��B�)B��B�EB�B��B�}B�pB�B��B��B�dB�dB�XB˒BɆB��B��BΥBɺB�RB�KB�BƨB�KBɆB�9B��B��B�tB��B�RB�RB�EB��BʌB��B��B��B�BB��B�B�B��B�wB��B��B�^B��B�6B�0B��B�*B�dB��B��B�qB�*B�B�RB��B��B�B��B�qB��B��B�nB�?B��B��B�B��B�B��B��B�B��B�tB�?B�B�FB��B��B��B�B��B��B�zB��B��B�B��B�B��B�B�?B��B�zB�B�tB�zB��B�B��B�nB��B�B�?B�nB��B�B��B��B�B�aB��B�}B��B��B��B��B�BB��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             B�jB�jB�OB�jBބBބBބB�jBޞB�pB�B��BބBޞB��B޸B��B�B��B�VB��B�B��B�bB�hB��B��B�B��B�8B�B�QB�]B�B�[B�FB	 4B	�B	ESB	�EB
M6B
��B
��B!B<�BsB�B��B��B�	B̳B�3B�SB҉B�B�"BBB�B��B��B�FB��B��B��B��B��B}"B_;BEB!BvB
�B
ɠB
ðB
�$B
�KB
�mB
s�B
J#B
5�B	�xB	��B	�AB	�B	��B	�B	�pB	n�B	j�B	_�B	-CB	�B	DB��B�MB��B�B�2B�uB��B�$B��B��B�'BĜBðB��B�mB	B	@�B	T�B	[�B	k�B	�B	��B	�;B	��B	�_B	�B	��B	�XB	ΥB	��B	ΊB	�oB	��B	�tB
�B	�LB	�}B
�B
�B
�B	��B	�;B	��B	��B	��B	��B	�B	�cB
 �B
[B
B
�B
YB
tB
	7B
�B
B
B
�B
�B
�B
B
�B
�B
B
�B
�B
�B
 �B
!B
!B
 �B
!�B
'�B
*KB
&2B
$�B
#�B
$�B
&�B
%B
&�B
*�B
)�B
*0B
+B
,qB
.�B
4�B
7�B
9�B
;B
=B
@�B
B�B
C�B
B�B
G+B
NB
L~B
JrB
J�B
L�B
K�B
K�B
LB
L�B
N"B
H�B
HB
GzB
G+B
G�B
HB
H�B
IRB
H�B
J	B
J�B
FB
F�B
G�B
F�B
H�B
I�B
H�B
G�B
I7B
J#B
G�B
G�B
FYB
EmB
EB
D�B
EmB
E9B
D�B
C�B
C-B
CaB
C�B
C�B
C�B
?�B
="B
<B
:DB
9XB
9�B
9�B
9XB
7fB
6zB
6�B
5tB
1�B
0�B
1�B
/5B
./B
-�B
+�B
+�B
,=B
,�B
/�B
+�B
*B
,�B
*B
'�B
%`B
$B
#�B
%,B
$�B
"�B
 B
 BB
�B
�B
B
�B
/B
B
�B
�B
�B
�B
�B
B
�B
�B
qB
xB
�B
�B
�B
�B
�B
MB
�B
mB
SB
B
2B
B
�B
�B
MB
FB
�B
hB
�B
�B
uB
 B
4B
:B
oB
�B
 B
�B
�B
TB
�B
:B
�B
�B
�B
hB
4B
 B
NB
TB
�B
�B
oB
B
^B
vB
�B
.B
<B
)B
xB

�B

#B
	�B
�B
zB
	B
	�B
�B
xB
	B

=B

�B
^B
�B
�B

XB

�B
xB
�B
�B
�B
0B
JB
0B
�B
dB
~B
B
�B
pB
�B
�B
vB
�B
\B
(B
�B
\B
(B
�B
�B
BB
B
B
�B
�B
FB
�B
�B
B
�B
�B
MB
B
�B
�B
�B
�B
�B
�B
2B
2B
�B
�B
�B
B
�B
�B
�B
B
9B
�B
sB
mB
9B
SB
B

B
B
_B
B
KB
KB
KB
�B
�B
kB
7B
B
B
�B
�B
�B
�B
�B
xB
�B
B
~B
IB
IB
�B
�B
�B
IB
B
�B
�B
�B
�B
�B
VB
OB
;B
B
�B
�B
pB
�B
pB
�B
�B
!B
�B
B
�B
 'B
 �B
�B
B
�B
�B
�B
;B
;B
�B
 �B
"�B
!�B
!bB
"�B
$B
%`B
$�B
%�B
&LB
%`B
&2B
&LB
%�B
&B
'B
($B
&2B
%�B
%�B
%zB
$�B
$�B
#�B
$@B
#�B
$ZB
$�B
%FB
%`B
%�B
&fB
&�B
&�B
'�B
(sB
(�B
(�B
(>B
($B
(
B
(>B
)DB
'�B
'�B
(sB
)�B
)�B
)�B
)�B
*B
*0B
*0B
*B
*�B
*�B
*�B
+kB
+�B
,"B
,=B
,WB
,�B
,�B
,�B
,qB
,�B
-�B
.�B
./B
.B
.cB
.IB
.}B
/ B
/�B
/�B
0UB
0�B
0;B
/�B
/�B
0oB
1'B
1'B
1�B
2aB
1�B
2|B
3hB
3MB
2�B
2�B
3�B
4�B
5?B
4�B
5�B
5�B
5�B
5�B
6FB
6zB
6`B
6zB
6`B
6�B
6�B
72B
7�B
8B
88B
8RB
9>B
8�B
8�B
8�B
9$B
9�B
9rB
9�B
9�B
:B
9�B
9�B
:*B
:�B
;B
<PB
=B
>B
>BB
>B
>]B
>�B
>�B
?�B
@ B
@ B
@�B
@�B
@�B
@�B
@�B
A�B
A;B
A�B
BuB
C�B
DB
C�B
C�B
DMB
DgB
D�B
E9B
ESB
F�B
F�B
F�B
F�B
F�B
F�B
FYB
F�B
GzB
G�B
HKB
H�B
H�B
H�B
IlB
J�B
I�B
JXB
J=B
I�B
J�B
JrB
J=B
JXB
KB
J�B
KxB
K�B
K^B
KxB
K�B
K�B
K�B
LB
L0B
K�B
LJB
L�B
MB
M�B
M�B
M�B
MjB
M�B
N"B
M�B
M�B
M�B
M�B
M�B
MjB
M�B
N�B
OB
O�B
OvB
O�B
O�B
O�B
O�B
P}B
QNB
O�B
O�B
O�B
O�B
P.B
QB
Q4B
Q�B
QB
R�B
R�B
Q�B
R�B
RoB
R�B
R�B
R�B
R�B
R�B
S&B
SuB
S�B
S�B
T,B
TaB
T�B
T�B
T{B
T�B
T�B
TaB
UB
UB
U�B
UB
VB
V�B
W�B
XB
W�B
X+B
X_B
X�B
YB
X�B
YB
X�B
X�B
Y1B
YB
YB
Y1B
X�B
YB
YeB
YeB
Y�B
ZB
[	B
[WB
[WB
[qB
[�B
\CB
\�B
\�B
\�B
]B
]�B
]�B
^OB
^�B
_�B
_�B
_�B
_�B
`B
`'B
`B
`\B
`�B
`�B
`�B
a-B
a�B
a�B
a�B
a�B
bNB
bhB
bNB
c B
bhB
c:B
cTB
c�B
dB
c�B
d&B
dZB
dZB
d&B
c�B
d�B
d�B
d�B
d�B
d�B
eB
e`B
e�B
e�B
e�B
e�B
fLB
fLB
fLB
f�B
f�B
g8B
gB
g�B
g�B
g�B
hsB
h�B
h�B
i*B
h�B
h�B
h�B
iDB
iDB
jKB
j0B
i�B
jeB
k�B
k6B
k�B
kQB
kQB
l"B
lqB
lqB
l�B
l�B
l�B
mB
mCB
mwB
mCB
l�B
m�B
m�B
ncB
n�B
oB
oB
oOB
oOB
oOB
o�B
pB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q�B
q�B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
r-B
rGB
rB
r|B
r�B
r�B
shB
tB
u?B
t�B
t�B
t�B
t�B
uB
u%B
v`B
xRB
y$B
yXB
y>B
y�B
z�B
{B
{dB
{B
{dB
{dB
{dB
{�B
{�B
|B
|B
|�B
|�B
|�B
}<B
}<B
}<B
}VB
}�B
}�B
~(B
~]B
}�B
}�B
~BB
~BB
~(B
~wB
~�B
B
B
.B
�B
�B
}B
�B
�B
�OB
�B
�B
��B
��B
��B
��B
�UB
��B
�UB
��B
��B
��B
�'B
�AB
�uB
�uB
��B
��B
�B
��B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�9B
�B
�9B
�B
�9B
�mB
��B
��B
�?B
��B
��B
��B
��B
�%B
��B
��B
�YB
��B
��B
��B
�zB
�_B
�B
��B
�EB
��B
��B
��B
�EB
�1B
��B
�KB
�B
��B
�B
��B
�7G�O�B�5B�5B�jB�B��B��BݘB�B�;BݘB�/B�jBޞB��B�5BݘBߤB�5BݘB�B�;BߤBݘB�dB�jB�;B�jB��B�jB�B�B��BݘB�5B�pB��B�/B�dB�5BߤB�B�pB��B��B�BBݘB��B�pB��B�B�B�]BܒB�pB�;B�BݘB��B�BޞB�B�/B��B�B�pB��B�dB�dB�pB�B��B�/BݘB�B�;B��BޞBݘBݘBޞBߤB�jB��B�B�vB��B��B�5B��B�jB�B�B�BB�BB��B�jBݘB�5B�vB�|B�5B�BޞB�B�B�pB��B�;B��B�vB�pB��B�BB��B�B�;B�jB��B�B��B�BBߤB�BB�HB�B�B��B�BB��B� B�B�B�B�B�B�B��B�B��B��B��B�B�TB�B�B�B�ZB��B��B�`B�&B�B�ZB��B��B��B�B��B�&B�2B�B��B��B�`B��B��B�B�mB�
B�>B�mB�mB�B�sB�B�B�B�KB�KB��B�B��B�B�B�B��B�cB�cB��B�WB��B� B�5B��B�B�iB��B�B�vB�B�B�B��B��B��B��B�>B��B��B�]B	 4B	oB	fB	B	:B	�B	�B	�B	�B	"4B	&LB	-B	5�B	9$B	D�B	XEB	}�B	�B	��B	�aB	��B	�B
�B
B
9�B
FtB
a|B
oiB
{B
}�B
�{B
�oB
�:B
��B
��B
��B
ŢB
� B
�QB
�B
��B
��B
	BBeB#�B0�B,qB,B-�B0�B2aB6�B5tB5tBH�BS�Ba�Bu%Bp�Bk�B{B��B��B�B�4B�oB��B�"B�{B��B�OB��B��BrB�B��B��B�=B��B�nB�=B��B��B��B�0B��B��B��B��B��B�B�<B��B��B�9B�UB�=B�kB�B�B�BB�aB�mB�tB��B��B�6B�dB�6B��B�<B�'B��B�mB�B��B�zB�BɺBɆB̘B�6BуBѷB��B��BҽB� B��BרB҉BѷB�NB�[B�EB��B��B��B�B�iB�AB�B��B� B� B�B��B��BoB;B��B��B�B��B�(B�VBBMB�>B�rB�xB�>B��B��B��B �B��B��B�B��B�(B�B��B��B�DB��B��B��B��B�+B�%B�cB�B��B�B��B�/B�B�B�`B��B�B�5B��BҽB�BB�&B҉BɺB��B� B��B�wB�B�B��B��B��B��B�B�3B��B��B�dB��B��B��B��B��B�B�!B�B��B��B�	B��B�kB��B�_B��B��B�CB�_B��B�JB�{B�BzBy�B}VB~]Bt�Bm�Bm�Bi�Bd�B\�B[�BTaBS&BR BQ�B?�BB'BL0B?HB49B3�B0�B+B(XB.B�B�BVB
=B�B�B	B
=B�B
�VB
�B
�B
�B
�B
�B
�)B
�B
�B
ȴB
�?B
�zB
ƨB
�'B
�HB
�}B
�BB
�0B
�dB
��B
� B
�B
��B
�B
��B
�7B
�OB
��B
�xB
��B
�	B
�+B
�1B
��B
�%B
�uB
�{B
~]B
� B
~�B
��B
w�B
uZB
y>B
tTB
��B
qAB
o�B
[�B
]�B
bB
[�B
NpB
L�B
b�B
+kB
,�B
%�B
 \B
#nB
&LB
%zB
_B
�B
)�B
&�B	��B	�fB	�B	�,B	�ZB	�fB	�8B	�B	ܒB	��B	��B	�|B	�fB	��B	�vB	�6B	��B	�<B	�XB	�B	�?B	��B	��B	�?B	��B	�*B	��B	�hB	�=B	�@B	��B	��B	��B	��B	�@B	�tB	��B	��B	�B	� B	��B	��B	��B	��B	�RB	�VB	z�B	y�B	�B	�{B	s�B	o�B	j�B	e�B	h
B	c�B	^�B	`B	aHB	`BB	d&B	]�B	e�B	VmB	O�B	K�B	L�B	P�B	R�B	F?B	K)B	=qB	<B	I�B	<6B	/OB	E9B	$B	�B	�B	�B	@B	B	$B	�B	�B		�B	�B	
�B	�B	�B	�B	�B	�B	�B	SB	B	{B	�B	MB	MB	AB�(B�"B�B�B�"B�JB��B�>B�8B�B��B��B��B		lB�]B��B��B��B�yB�mB�B�fB��B�B�mB�sB��B��B�B�sB�B�
B�DB�8B�B�B�DB��B�fB�2B��B�8B�fB�mB�
B�2B�"B�B��B�B��B�B��B�B�B�B�B�&B��B�WBیB��B�)B��B�EB�B��B�}B�pB�B��B��B�dB�dB�XB˒BɆB��B��BΥBɺB�RB�KB�BƨB�KBɆB�9B��B��B�tB��B�RB�RB�EB��BʌB��B��B��B�BB��B�B�B��B�wB��B��B�^B��B�6B�0B��B�*B�dB��B��B�qB�*B�B�RB��B��B�B��B�qB��B��B�nB�?B��B��B�B��B�B��B��B�B��B�tB�?B�B�FB��B��B��B�B��B��B�zB��B��B�B��B�B��B�B�?B��B�zB�B�tB�zB��B�B��B�nB��B�B�?B�nB��B�B��B��B�B�aB��B�}B��B��B��B��B�BB��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<[(�<V�0<#�
<#�
<+�:<#�
<T�v<�]�<���<�>�<�ju<���<��<i#<#�
<#�
<-<.Sz<#�
<mgt<W��<A$�<A$�<#�
<#�
<#�
<#�
<#�
<0�m<C�Q<LFi<Y�<;�w<|5<#�
<#�
<SX�<#�
<#�
<V2�<���<�42<���<�<�Ҭ<Dlq<*�_<#�
<��{<^\<E��<���<�[@<2�:<%�'<eWE<#�
<q"<3�4<C�N<v`Y<#�
<#�
<#�
<#�
<C�N<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Q�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018031422285320180314222853IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018032500022620180325000226QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018032500022620180325000226QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107550620190521075506IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                