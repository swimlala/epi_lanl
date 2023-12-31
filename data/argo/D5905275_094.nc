CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-08-03T22:51:19Z creation; 2023-04-26T19:14:30Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20200803225119  20230426191430  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               ^   ^AA  AOAO7316_008644_094                 7316_008644_094                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�-�I��@�-�I��11  @�-�}Vl�@�-�}Vl�@'�r2L�@'�r2L��c��"&��c��"&�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�@B�\@�G�@�G�@�G�@޸RA   A��A   A,(�A@  A`��A�  A�Q�A�Q�A��A�  AϮA߮A�  B   B(�B�
B�
B (�B(  B/�
B8(�B@(�BH(�BP(�BW�B_�Bg�
Bo�
Bw�
B�  B�{B�  B�  B�  B�  B�  B�{B�  B�  B��B�  B�{B�{B�{B�(�B�{B�  B�  B�{B�  B�  B�  B�  B�{B��B��B�  B�  B�  B�{B�  B��C  C
=C  C
=C

=C
=C  C  C��C  C
=C  C��C  C��C��C"
=C$
=C&
=C'��C*  C,  C.
=C0{C2{C4
=C6  C8
=C:
=C<
=C>  C?�CA��CD  CE��CG�CI��CK��CM�CP  CR
=CT  CV  CX  CY��C\  C^  C_��Ca��Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq��Cs��Cv  Cx  Cz  C|
=C~  C��C�C�C�  C���C�  C�C�  C�  C�  C�
=C�C�  C�
=C�C�  C���C���C�  C�  C���C���C�C�C�  C���C���C���C�  C�C�  C���C�C�C���C�  C�C�  C���C���C�C�C���C���C���C�  C�C�C�
=C�
=C�  C�  C���C���C�  C�  C�
=C�
=C���C���C���C���C�  C�C�C�  C�
=C�C���C���C�  C�  C�C���C�  C�  C�C�
=C�C�  C���C�  C�C�C�  C�  C�C�C�  C���C�  C�C�C�C�  C���C�  C���C���C���C���C���C���C�C�C���C���C�  C�  C�C���C�  C�C���C���C���C���C�
=C�
=C�C���C���C�  C�C�
=C�  C�  C�C���D }qD  D� D�qD}qD  D� D  D� D�qD}qD��Dz�D  D� D  D� D	  D	��D
�D
� D
�qD}qD  D}qD��D� D  D}qD�D� D�qD��D�qDz�D�qD��D  D}qD  D� D  D}qD�D� D  D��D�D� D  D� D  D� D�qD��DD�DD� D�qD� D  D� D   D � D!�D!��D!�qD"}qD#  D#� D#�qD$� D%  D%}qD&�D&��D'  D'� D(  D(� D)  D)� D*  D*� D+�D+��D+�qD,z�D,�qD-� D-�qD.��D/�D/� D0  D0}qD1�D1��D2  D2}qD3  D3��D4�D4� D4�qD5� D6D6� D7  D7�D8  D8� D8�qD9� D:  D:� D;�D;� D<�D<��D=�D=��D=�qD>}qD?  D?� D@  D@� DA  DA��DB�DB}qDB�qDC��DD�DD� DE  DE}qDF  DF}qDF��DG}qDH  DH� DI�DI� DI�qDJ� DK�DK}qDL  DL� DL�qDM}qDM�qDN��DO�DO��DP  DP� DQ�DQ� DR�DR��DR�qDS� DT�DT��DUDU��DU�qDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\}qD\�qD]� D^�D^� D_�D_� D`  D`� Da  Da��Db�Db� Dc�Dc��Dc�qDd��De  De}qDe��Df}qDg  Dg� Dg�qDh}qDh�qDi� DjDj��Dk  Dk}qDl  Dl� Dm  Dm� Dn�Dn� Dn�qDo� Dp  Dp}qDq�Dq� Dq�qDr��Ds�Ds}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw� DxDx��Dy  Dy� Dz  Dz��D{D{� D{�qD|z�D}  D}�D~�D~� D  D��D�qD�@ D���D�� D�  D�>�D�}qD�� D�HD�AHD�� D�D�HD�AHD��HD�� D�  D�AHD�� D�� D�  D�@ D��HD��HD��D�@ D�~�D�� D�HD�@ D�~�D��HD�HD�>�D�~�D��HD��D�@ D�~�D�� D�  D�AHD���D��HD���D�@ D�� D��qD��qD�>�D���D�� D���D�>�D�� D��HD���D�@ D�� D��qD���D�>�D�� D��HD�HD�@ D��HD�� D���D�@ D�� D���D��qD�>�D�� D��HD�  D�@ D��HD�� D���D�AHD�� D���D���D�>�D��HD��HD�HD�AHD��HD��HD���D�=qD�~�D�� D�HD�B�D��HD�� D���D�=qD�� D���D��qD�@ D��HD��HD���D�@ D�~�D��qD���D�@ D��HD��HD�  D�>�D��HD���D��qD�>�D�� D���D��qD�>�D�� D���D��qD�=qD�~�D���D�  D�@ D��HD��HD�HD�AHD���D��HD�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�B�D���D�� D��qD�>�D�~�D���D�  D�@ D��HD��HD�HD�AHD���D�D�  D�>�D�~�D�� D�HD�@ D��HD��HD���D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�� D��HD��D�B�D�~�D��qD�  D�AHD�� D���D�  D�>�D�}qD���D���D�>�D�~�D���D�HD�AHD�~�D���D�HD�B�D���D��HD���D�@ D�~�D�� D�HD�>�D�� D��HD�  D�>�D�� D��HD���D�=qD�~�D��HD���D�>�D�� D�D�HD�@ D�~�D��qD���D�@ D�~�D��qD��qD�>�D��HD��HD�HD�AHD�� D��qD�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D���D�>�D�~�D���D�  D�AHD�D��HD�  D�@ DÀ D��HD�HD�@ DāHD�� D�  D�@ D�~�DŽqD���D�AHDƀ D�� D�  D�@ Dǀ D�� D�  D�>�D�~�D�� D�HD�AHDɀ D�� D�  D�@ Dʀ D�� D�  D�AHDˀ D�� D�  D�@ D�~�D��HD��D�AHD�~�D;�D�  D�@ D�~�Dξ�D�  D�@ DρHD��HD�  D�@ DЀ Dо�D��qD�>�Dр D��HD�HD�@ DҀ DҾ�D�HD�AHDӀ D��HD�HD�AHDԁHD��HD�  D�AHDՂ�D��HD���D�@ DցHD��HD�  D�>�D׀ D��HD�HD�@ D؀ Dؾ�D���D�>�Dـ D�� D�  D�@ Dڀ D�� D�HD�AHDۀ D۽qD���D�>�D�~�DܽqD���D�@ D݁HD�� D���D�@ DށHD�� D�  D�>�D�~�D��HD�  D�@ D��HDྸD�  D�AHD� DᾸD�  D�>�D�~�D�� D���D�>�D� D��HD�HD�@ D� D�� D�  D�@ D�HD徸D��qD�>�D�~�D�qD���D�@ D炏D��HD���D�>�D�HD��HD�HD�AHD�~�D龸D��D�C�DꂏD��HD�  D�@ D� D�� D�  D�AHD�HD��HD��D�@ D�~�D���D�  D�>�D�~�D�qD��qD�>�D� D�� D�  D�>�D�~�D�D�  D�@ D�HD�� D�  D�AHD�HD�D�  D�AHD�D��HD�  D�@ D�~�D�� D�  D�=qD�~�D�D�  D�@ D�� D��qD���D�>�D�~�D�� D�  D�@ D��HD��HD�  D�@ D��HD��HD���D�E>�?.{?aG�?�=q?�{?�
=?��@\)@(�@.{@=p�@W
=@h��@u@��
@�\)@�Q�@�  @�=q@�33@�(�@��
@�\)@ٙ�@�G�@�=q@�z�@��RA�
A�Ap�A�\AffA�HA ��A%�A(��A.{A333A8Q�A<(�AAG�AFffAJ�HAP  AU�AZ=qA^�RAb�\Ag�Amp�Aq�AuA{�A�Q�A�=qA�z�A�\)A��A��
A�{A���A��A�A�  A��HA�p�A��A��A�z�A�\)A���A��A�ffA���A��HA�p�A�  A��HA���A�\)A��A�z�A�ffA���A��
A�{A�Q�Aҏ\A�p�A�  A�=qA�z�A�\)AᙚA�A�ffA���A��HA�p�A�  A�\A�z�A�
=A��A��A�{B z�B��B�\B�
BG�BffB\)B��B
{B33BQ�Bp�B
=BQ�BG�B�\B(�Bp�B�\B�
BG�B�RB�
B�B�\B   B!�B"=qB#�
B%G�B&�\B'�
B)G�B*�RB,(�B-p�B.�HB0Q�B1B3
=B4(�B5B733B8��B9B;33B<��B>=qB?�B@��BBffBC�
BE�BFffBG�
BIp�BJ�RBK�
BMp�BN�HBP(�BQp�BS
=BTz�BU��BV�HBXQ�BY�B[33B\z�B]�B_�B`��Ba�Bc\)Bd��Bf=qBg\)Bh��Bj=qBk�Bl��Bn{Bo�Bp��Br=qBs�Bt��BvffBw�Bx��Bz{B{�B|��B~{B\)B�Q�B��B��B�Q�B��HB���B�=qB��HB�p�B�{B��HB��B�(�B���B�p�B�=qB��HB�p�B�{B���B�p�B�(�B��RB�G�B�  B��RB�\)B��
B�z�B�33B��
B�ffB�
=B��B�Q�B���B���B�{B��RB�\)B�  B��\B��B��B�Q�B�
=B���B�(�B��RB�\)B�  B���B�G�B��
B�z�B��B��
B��\B�33B��
B�ffB�
=B�B�z�B��B�B�ffB�
=B�B��\B�G�B��B��\B�33B��
B�z�B�33B��B��\B�33B��B��\B�G�B��B���B�G�B�{B���B��B�Q�B�
=B��B�z�B�33B�  B��RB��B�Q�B�
=B�B\B�G�B�  BĸRB�p�B�(�B��HBǙ�B�Q�B�
=B�B�ffB�
=B˙�B�(�Ḅ�B��BͅB�  B�ffB��HB�33BυB��
B�(�B�ffBУ�B�
=B�\)BѮB�{B�z�B��HB�33Bә�B��B�=qBԏ\B���B�G�BծB�{B֏\B���B�G�Bי�B��B�=qBأ�B���B�\)BٮB�{Bڏ\B���B�G�BۮB�{B�ffB���B��B݅B��
B�=qBޣ�B��B߅B�  B�Q�B��B���B�\)B�B�{B�ffB���B�33B㙚B�  B�z�B��HB�33B噚B�  B�Q�B���B��B�B�  B�ffB��HB�G�B�B�(�B�z�B���B�\)B�B�{B�z�B��HB�G�B홚B�{B�z�B�
=B�\)B��
B�=qB��B�
=B�B��B�Q�B���B�G�B�B�(�B���B�
=B��B��B�ffB���B�33B��B�(�B��\B���B�p�B��B�Q�B���B�33B���B�{B�z�B��HB�\)B��
B�=qB���B��B��C   C 33C p�C ��C �HC�CQ�C�\C��C  C=qCz�C�RC��C(�CffC��C��C
=C=qCz�C�C�C�CQ�C�\CC��C(�CffC��C��C
=C=qCz�C�C�C{CQ�C�CC��C	(�C	ffC	��C	�HC
{C
Q�C
�C
C
��C33Cp�C��C�HC�CQ�C�\C��C
=CG�Cz�C�RC��C(�CffC��C�HC{C\)C�\C��C
=C=qCp�C�C�C�CffC��C�HC{C\)C��C�
C�CffC��C�HC(�CffC�C�HC�CffC��C�HC�C\)C��C��C{CQ�C�\C�
C{C\)C��C�HC33Cz�C�RC  CG�C�C��C
=CQ�C�\C��C�C\)C��C�C=qC�C��C
=CQ�C��C�
C�CffC�C�C =qC �C ��C!�C!ffC!�C"  C"=qC"�\C"�
C#{C#ffC#��C#�C$=qC$�C$��C%�C%p�C%C&
=C&\)C&��C&�C'(�C'z�C'C(
=C(Q�C(��C(�C)(�C)z�C)C*
=C*\)C*�C*��C+G�C+�\C+�HC,(�C,p�C,�RC-
=C-\)C-��C-�C.=qC.�C.�
C/33C/z�C/��C0{C0\)C0�C1  C1G�C1��C1�C2=qC2��C2�C333C3�C3��C4�C4ffC4C5{C5ffC5�RC6
=C6\)C6��C6��C7=qC7�C7�
C8(�C8�C8�
C9(�C9�C9��C:{C:ffC:�RC;{C;ffC;�RC<{C<ffC<�C<��C=G�C=��C=�C>=qC>��C>�HC?33C?�C?�
C@(�C@z�C@��CA�CAz�CA�
CB33CBz�CBCC{CCp�CCCD{CDp�CDCE{CE\)CE�CF  CF\)CF�RCG
=CGQ�CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         ?�=q@�@B�\@�G�@�G�@�G�@޸RA   A��A   A,(�A@  A`��A�  A�Q�A�Q�A��A�  AϮA߮A�  B   B(�B�
B�
B (�B(  B/�
B8(�B@(�BH(�BP(�BW�B_�Bg�
Bo�
Bw�
B�  B�{B�  B�  B�  B�  B�  B�{B�  B�  B��B�  B�{B�{B�{B�(�B�{B�  B�  B�{B�  B�  B�  B�  B�{B��B��B�  B�  B�  B�{B�  B��C  C
=C  C
=C

=C
=C  C  C��C  C
=C  C��C  C��C��C"
=C$
=C&
=C'��C*  C,  C.
=C0{C2{C4
=C6  C8
=C:
=C<
=C>  C?�CA��CD  CE��CG�CI��CK��CM�CP  CR
=CT  CV  CX  CY��C\  C^  C_��Ca��Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq��Cs��Cv  Cx  Cz  C|
=C~  C��C�C�C�  C���C�  C�C�  C�  C�  C�
=C�C�  C�
=C�C�  C���C���C�  C�  C���C���C�C�C�  C���C���C���C�  C�C�  C���C�C�C���C�  C�C�  C���C���C�C�C���C���C���C�  C�C�C�
=C�
=C�  C�  C���C���C�  C�  C�
=C�
=C���C���C���C���C�  C�C�C�  C�
=C�C���C���C�  C�  C�C���C�  C�  C�C�
=C�C�  C���C�  C�C�C�  C�  C�C�C�  C���C�  C�C�C�C�  C���C�  C���C���C���C���C���C���C�C�C���C���C�  C�  C�C���C�  C�C���C���C���C���C�
=C�
=C�C���C���C�  C�C�
=C�  C�  C�C���D }qD  D� D�qD}qD  D� D  D� D�qD}qD��Dz�D  D� D  D� D	  D	��D
�D
� D
�qD}qD  D}qD��D� D  D}qD�D� D�qD��D�qDz�D�qD��D  D}qD  D� D  D}qD�D� D  D��D�D� D  D� D  D� D�qD��DD�DD� D�qD� D  D� D   D � D!�D!��D!�qD"}qD#  D#� D#�qD$� D%  D%}qD&�D&��D'  D'� D(  D(� D)  D)� D*  D*� D+�D+��D+�qD,z�D,�qD-� D-�qD.��D/�D/� D0  D0}qD1�D1��D2  D2}qD3  D3��D4�D4� D4�qD5� D6D6� D7  D7�D8  D8� D8�qD9� D:  D:� D;�D;� D<�D<��D=�D=��D=�qD>}qD?  D?� D@  D@� DA  DA��DB�DB}qDB�qDC��DD�DD� DE  DE}qDF  DF}qDF��DG}qDH  DH� DI�DI� DI�qDJ� DK�DK}qDL  DL� DL�qDM}qDM�qDN��DO�DO��DP  DP� DQ�DQ� DR�DR��DR�qDS� DT�DT��DUDU��DU�qDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\}qD\�qD]� D^�D^� D_�D_� D`  D`� Da  Da��Db�Db� Dc�Dc��Dc�qDd��De  De}qDe��Df}qDg  Dg� Dg�qDh}qDh�qDi� DjDj��Dk  Dk}qDl  Dl� Dm  Dm� Dn�Dn� Dn�qDo� Dp  Dp}qDq�Dq� Dq�qDr��Ds�Ds}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw� DxDx��Dy  Dy� Dz  Dz��D{D{� D{�qD|z�D}  D}�D~�D~� D  D��D�qD�@ D���D�� D�  D�>�D�}qD�� D�HD�AHD�� D�D�HD�AHD��HD�� D�  D�AHD�� D�� D�  D�@ D��HD��HD��D�@ D�~�D�� D�HD�@ D�~�D��HD�HD�>�D�~�D��HD��D�@ D�~�D�� D�  D�AHD���D��HD���D�@ D�� D��qD��qD�>�D���D�� D���D�>�D�� D��HD���D�@ D�� D��qD���D�>�D�� D��HD�HD�@ D��HD�� D���D�@ D�� D���D��qD�>�D�� D��HD�  D�@ D��HD�� D���D�AHD�� D���D���D�>�D��HD��HD�HD�AHD��HD��HD���D�=qD�~�D�� D�HD�B�D��HD�� D���D�=qD�� D���D��qD�@ D��HD��HD���D�@ D�~�D��qD���D�@ D��HD��HD�  D�>�D��HD���D��qD�>�D�� D���D��qD�>�D�� D���D��qD�=qD�~�D���D�  D�@ D��HD��HD�HD�AHD���D��HD�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�B�D���D�� D��qD�>�D�~�D���D�  D�@ D��HD��HD�HD�AHD���D�D�  D�>�D�~�D�� D�HD�@ D��HD��HD���D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�� D��HD��D�B�D�~�D��qD�  D�AHD�� D���D�  D�>�D�}qD���D���D�>�D�~�D���D�HD�AHD�~�D���D�HD�B�D���D��HD���D�@ D�~�D�� D�HD�>�D�� D��HD�  D�>�D�� D��HD���D�=qD�~�D��HD���D�>�D�� D�D�HD�@ D�~�D��qD���D�@ D�~�D��qD��qD�>�D��HD��HD�HD�AHD�� D��qD�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D���D�>�D�~�D���D�  D�AHD�D��HD�  D�@ DÀ D��HD�HD�@ DāHD�� D�  D�@ D�~�DŽqD���D�AHDƀ D�� D�  D�@ Dǀ D�� D�  D�>�D�~�D�� D�HD�AHDɀ D�� D�  D�@ Dʀ D�� D�  D�AHDˀ D�� D�  D�@ D�~�D��HD��D�AHD�~�D;�D�  D�@ D�~�Dξ�D�  D�@ DρHD��HD�  D�@ DЀ Dо�D��qD�>�Dр D��HD�HD�@ DҀ DҾ�D�HD�AHDӀ D��HD�HD�AHDԁHD��HD�  D�AHDՂ�D��HD���D�@ DցHD��HD�  D�>�D׀ D��HD�HD�@ D؀ Dؾ�D���D�>�Dـ D�� D�  D�@ Dڀ D�� D�HD�AHDۀ D۽qD���D�>�D�~�DܽqD���D�@ D݁HD�� D���D�@ DށHD�� D�  D�>�D�~�D��HD�  D�@ D��HDྸD�  D�AHD� DᾸD�  D�>�D�~�D�� D���D�>�D� D��HD�HD�@ D� D�� D�  D�@ D�HD徸D��qD�>�D�~�D�qD���D�@ D炏D��HD���D�>�D�HD��HD�HD�AHD�~�D龸D��D�C�DꂏD��HD�  D�@ D� D�� D�  D�AHD�HD��HD��D�@ D�~�D���D�  D�>�D�~�D�qD��qD�>�D� D�� D�  D�>�D�~�D�D�  D�@ D�HD�� D�  D�AHD�HD�D�  D�AHD�D��HD�  D�@ D�~�D�� D�  D�=qD�~�D�D�  D�@ D�� D��qD���D�>�D�~�D�� D�  D�@ D��HD��HD�  D�@ D��HD��HD���G�O�>�?.{?aG�?�=q?�{?�
=?��@\)@(�@.{@=p�@W
=@h��@u@��
@�\)@�Q�@�  @�=q@�33@�(�@��
@�\)@ٙ�@�G�@�=q@�z�@��RA�
A�Ap�A�\AffA�HA ��A%�A(��A.{A333A8Q�A<(�AAG�AFffAJ�HAP  AU�AZ=qA^�RAb�\Ag�Amp�Aq�AuA{�A�Q�A�=qA�z�A�\)A��A��
A�{A���A��A�A�  A��HA�p�A��A��A�z�A�\)A���A��A�ffA���A��HA�p�A�  A��HA���A�\)A��A�z�A�ffA���A��
A�{A�Q�Aҏ\A�p�A�  A�=qA�z�A�\)AᙚA�A�ffA���A��HA�p�A�  A�\A�z�A�
=A��A��A�{B z�B��B�\B�
BG�BffB\)B��B
{B33BQ�Bp�B
=BQ�BG�B�\B(�Bp�B�\B�
BG�B�RB�
B�B�\B   B!�B"=qB#�
B%G�B&�\B'�
B)G�B*�RB,(�B-p�B.�HB0Q�B1B3
=B4(�B5B733B8��B9B;33B<��B>=qB?�B@��BBffBC�
BE�BFffBG�
BIp�BJ�RBK�
BMp�BN�HBP(�BQp�BS
=BTz�BU��BV�HBXQ�BY�B[33B\z�B]�B_�B`��Ba�Bc\)Bd��Bf=qBg\)Bh��Bj=qBk�Bl��Bn{Bo�Bp��Br=qBs�Bt��BvffBw�Bx��Bz{B{�B|��B~{B\)B�Q�B��B��B�Q�B��HB���B�=qB��HB�p�B�{B��HB��B�(�B���B�p�B�=qB��HB�p�B�{B���B�p�B�(�B��RB�G�B�  B��RB�\)B��
B�z�B�33B��
B�ffB�
=B��B�Q�B���B���B�{B��RB�\)B�  B��\B��B��B�Q�B�
=B���B�(�B��RB�\)B�  B���B�G�B��
B�z�B��B��
B��\B�33B��
B�ffB�
=B�B�z�B��B�B�ffB�
=B�B��\B�G�B��B��\B�33B��
B�z�B�33B��B��\B�33B��B��\B�G�B��B���B�G�B�{B���B��B�Q�B�
=B��B�z�B�33B�  B��RB��B�Q�B�
=B�B\B�G�B�  BĸRB�p�B�(�B��HBǙ�B�Q�B�
=B�B�ffB�
=B˙�B�(�Ḅ�B��BͅB�  B�ffB��HB�33BυB��
B�(�B�ffBУ�B�
=B�\)BѮB�{B�z�B��HB�33Bә�B��B�=qBԏ\B���B�G�BծB�{B֏\B���B�G�Bי�B��B�=qBأ�B���B�\)BٮB�{Bڏ\B���B�G�BۮB�{B�ffB���B��B݅B��
B�=qBޣ�B��B߅B�  B�Q�B��B���B�\)B�B�{B�ffB���B�33B㙚B�  B�z�B��HB�33B噚B�  B�Q�B���B��B�B�  B�ffB��HB�G�B�B�(�B�z�B���B�\)B�B�{B�z�B��HB�G�B홚B�{B�z�B�
=B�\)B��
B�=qB��B�
=B�B��B�Q�B���B�G�B�B�(�B���B�
=B��B��B�ffB���B�33B��B�(�B��\B���B�p�B��B�Q�B���B�33B���B�{B�z�B��HB�\)B��
B�=qB���B��B��C   C 33C p�C ��C �HC�CQ�C�\C��C  C=qCz�C�RC��C(�CffC��C��C
=C=qCz�C�C�C�CQ�C�\CC��C(�CffC��C��C
=C=qCz�C�C�C{CQ�C�CC��C	(�C	ffC	��C	�HC
{C
Q�C
�C
C
��C33Cp�C��C�HC�CQ�C�\C��C
=CG�Cz�C�RC��C(�CffC��C�HC{C\)C�\C��C
=C=qCp�C�C�C�CffC��C�HC{C\)C��C�
C�CffC��C�HC(�CffC�C�HC�CffC��C�HC�C\)C��C��C{CQ�C�\C�
C{C\)C��C�HC33Cz�C�RC  CG�C�C��C
=CQ�C�\C��C�C\)C��C�C=qC�C��C
=CQ�C��C�
C�CffC�C�C =qC �C ��C!�C!ffC!�C"  C"=qC"�\C"�
C#{C#ffC#��C#�C$=qC$�C$��C%�C%p�C%C&
=C&\)C&��C&�C'(�C'z�C'C(
=C(Q�C(��C(�C)(�C)z�C)C*
=C*\)C*�C*��C+G�C+�\C+�HC,(�C,p�C,�RC-
=C-\)C-��C-�C.=qC.�C.�
C/33C/z�C/��C0{C0\)C0�C1  C1G�C1��C1�C2=qC2��C2�C333C3�C3��C4�C4ffC4C5{C5ffC5�RC6
=C6\)C6��C6��C7=qC7�C7�
C8(�C8�C8�
C9(�C9�C9��C:{C:ffC:�RC;{C;ffC;�RC<{C<ffC<�C<��C=G�C=��C=�C>=qC>��C>�HC?33C?�C?�
C@(�C@z�C@��CA�CAz�CA�
CB33CBz�CBCC{CCp�CCCD{CDp�CDCE{CE\)CE�CF  CF\)CF�RCG
=CGQ�CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aݺ^Aݧ�Aݥ�Aݝ�A�v�A�dZA�`BA�Q�A�G�A�E�A�E�A�?}A�=qA�9XA�9XA�9XA�5?A�33A�1'A�+A��A���A�A�K�A۾wA�9XAھwA�bNA�=qA�/A��A�JA���A��A��/AٸRAٗ�A�^5A؁A��A�I�A�$�AӰ!A�7LA�K�AЃAǟ�A�=qA²-A��RA�&�A�?}A���A�jA���A��9A�-A���A���A��A� �A���A�-A�?}A���A�A���A�z�A�r�A���A�x�A��A���A��DA�As�-An��Ah�jA^-AX�RAWO�AUVAS�AQƨAN��ALE�AHĜAF��AE��AC��A@�HA?��A>�/A=�hA<��A;��A:�uA9�A8�\A81A8  A7�
A6=qA5�-A4��A4{A3A3/A2jA0��A/K�A-�A,��A,M�A,ZA-;dA,�A+A*��A*ZA)�;A)�PA)O�A(ĜA'|�A&ȴA&~�A&5?A%��A$��A#C�A"��A"�DA!�A!�
A!��A!ƨA!A!��A!|�A!%A (�A�hA+A��A1AA��AO�A��A�/A��A��A9XA�^AhsAXA
=A5?AJA�A�yA=qAA�`A��A�!AbNA�;A?}A��A^5A��AA��AjA�AdZA�HAffA9XA|�A��A��A�A�A�A(�A7LA��A��AbNAA�AAK�A
��A
v�A
 �A	��A	
=A~�A  A�
AC�A��A��A��A  A�-A|�A�jA��AbA?}A��A��A��AXA/A �`A ��A I�A �@��@�33@�+@��!@�O�@���@��u@�1'@�|�@��P@�C�@���@�E�@�@���@���@�@��@�?}@�D@�ƨ@���@�V@��T@�O�@�Ĝ@� �@@홚@�G�@��`@�Q�@�z�@�I�@��m@�|�@�!@��@�p�@�bN@睲@�"�@��H@�n�@�~�@�{@�7@��`@�A�@㕁@�dZ@�@�=q@�%@���@�|�@�
=@�^5@�$�@�p�@�V@���@���@܃@�9X@�"�@ڏ\@�^5@��@ى7@���@�Z@�\)@�n�@պ^@Ձ@�Ĝ@���@�Q�@�l�@���@�o@җ�@���@϶F@��y@��#@��@�7L@��`@��m@�\)@�o@��H@�V@���@�%@�I�@�|�@��@�~�@�^5@�$�@��@őh@ģ�@��@î@�@�@��#@�?}@���@�Q�@��P@�o@���@�@���@�&�@�I�@�  @��
@��P@�"�@��@���@�E�@��^@�X@��@��@� �@�ƨ@�|�@��H@��!@�$�@�@��#@��7@�X@���@�r�@���@�|�@�dZ@�\)@�\)@�"�@��+@��7@��@��@� �@��
@��@�|�@�K�@��H@��!@�v�@�-@�@��T@��7@�X@�7L@�Ĝ@�ƨ@�33@�ȴ@�ff@�{@��^@��h@�X@�&�@��`@��9@��D@�9X@�j@� �@��@�dZ@�33@��y@�^5@��@�@�x�@�p�@���@��9@�r�@�  @��P@�\)@���@�M�@���@�G�@��`@�r�@��m@���@�dZ@�"�@�ȴ@��@��h@�V@��/@��u@�j@�9X@�ƨ@�S�@���@�v�@�n�@�M�@��@��T@���@�p�@�/@��@�(�@��@��@��!@���@��!@���@�v�@�@�p�@�p�@���@��h@�G�@���@���@�(�@��;@��w@���@��P@�l�@�o@�~�@�J@��-@�X@�O�@�7L@�V@��j@�I�@�b@��w@�C�@���@��@��@�@�hs@�&�@���@�Ĝ@�j@�I�@�I�@�A�@�1'@� �@���@���@�|�@�o@�
=@��H@���@�v�@�$�@�x�@�?}@��@���@��9@�r�@��@��
@��w@�C�@�o@��@���@�v�@�-@��@��7@�&�@���@�z�@�I�@�ƨ@�t�@�C�@���@��y@��@��R@�~�@�=q@��@�@�X@��@��`@���@��@�b@\)@~�y@~�@~�@~��@~5?@~$�@}�@}��@}�-@}?}@|I�@{��@z��@z��@z~�@z^5@zJ@yhs@w��@w+@v��@v��@v$�@u�-@up�@uO�@t��@t�j@t(�@s�
@sdZ@s"�@r��@q��@q��@q�@p��@pbN@p1'@p  @o;d@n�@n�R@n��@nv�@nV@n{@m�T@m�-@m�h@m`B@m?}@m/@l��@lz�@lI�@l1@kƨ@kC�@jn�@jJ@i��@i�#@i��@i��@ix�@iG�@i%@h��@h��@hbN@g�P@g+@fȴ@f5?@e�@e�h@ep�@eO�@e�@d��@dz�@dZ@cƨ@cC�@c@b�\@b�@ax�@`Ĝ@`bN@`Q�@` �@_�;@^�@^�+@^E�@^{@]�T@]�h@]O�@\�/@\z�@\z�@\(�@[ƨ@[�@[C�@[@ZM�@YX@Y%@X�`@XĜ@XQ�@W;d@Vv�@V@U��@T��@T�D@TI�@T�@S�F@S"�@R��@R^5@Q�@Q��@Q��@Qhs@Q%@P�9@P�u@P�u@PA�@O|�@O+@N��@N�@Nȴ@N��@NE�@N@M�-@M�h@MO�@L��@L(�@KdZ@KS�@Ko@J�\@I�@Ix�@IG�@I�@I%@H��@HĜ@Hr�@Hb@G�@G��@G�w@G�@G�P@Gl�@G�@F�+@Fff@FE�@E�T@E�h@E/@D��@D�/@D��@Dz�@D9X@Cƨ@C��@CdZ@C33@B�@B�\@Bn�@Bn�@B^5@Ahs@A�@@��@@�@@�@@r�@@r�@@1'@?�;@?��@?|�@?K�@?�@>�R@>E�@=�@=�@<��@<(�@;�
@:�@:=q@:J@9�@9�^@9hs@8��@8r�@8Q�@8  @7��@7|�@6��@6ȴ@6�+@6v�@65?@5�@5�-@5`B@4��@4�j@4I�@49X@4�@3��@3o@2��@1�#@1hs@1&�@0�`@0Ĝ@0Q�@0  @/�;@/�P@/|�@/\)@/�@.��@.�y@.ȴ@.ff@-�@-@-�h@-�@,�j@,Z@,1@+�
@+ƨ@+ƨ@+ƨ@+��@+t�@+33@*��@*�!@*��@*�\@*n�@*�@)�#@)hs@(�9@(r�@(1'@'�@'�P@'l�@'K�@';d@'+@'�@&�y@&�R@&v�@&ff@&V@&E�@&5?@&@%�T@%��@%�-@%�h@%�h@%�h@%�h@%O�@%�@$��@$�/@$�/@$��@$�j@$��@$j@$�@#��@#ƨ@#�@#S�@#@"��@"^5@"�@"J@!�@!��@!�7@!hs@!&�@ �9@ r�@  �@�;@�@��@|�@|�@K�@�@ȴ@�R@�+@E�@{@�T@@�-@�h@�@�j@��@z�@z�@z�@z�@z�@9X@(�@1@��@�
@��@dZ@C�@@~�@�@��@hs@hs@X@&�@%@��@��@�9@�u@bN@A�@1'@b@�@��@�w@��@�@�y@ȴ@��@v�@V@$�@�T@��@��@p�@?}@V@�@�j@�D@I�@(�@�@�m@�@C�@@��@�\@~�@n�@�@��@�7@hs@7L@�`@�`@��@�@bN@A�@b@��@�w@�w@�w@��@l�@;d@�@
=@�@�R@ff@V@E�@E�@5?@@�@@`B@?}@/@��@�/@��@��@��@��@��@��@�@�@�m@��@�@�@�@t�@t�AݬAݺ^Aݺ^AݶFAݺ^AݸRAݩ�Aݡ�Aݛ�Aݣ�Aݥ�AݬAݛ�Aݡ�Aݡ�Aݏ\A݁A�t�A�jA�ffA�bNA�bNA�ffA�dZA�`BA�VA�^5A�S�A�K�A�K�A�K�A�E�A�C�A�G�A�G�A�C�A�C�A�G�A�C�A�;dA�A�A�A�A�=qA�;dA�=qA�?}A�=qA�;dA�9XA�=qA�=qA�9XA�9XA�;dA�;dA�7LA�;dA�=qA�;dA�9XA�9XA�=qA�;dA�7LA�9XA�=qA�;dA�7LA�7LA�;dA�9XA�5?A�7LA�7LA�1'A�/A�5?A�7LA�5?A�1'A�5?A�5?A�1'A�/A�33A�1'A�-A�/A�1'A�1'A�/A�-A�1'A�1'A�(�A�&�A�&�A�$�A��A��A��A��A�VA�VA�JA�1A�A���A��HA��#A��/A��/A���A�ƨAܺ^AܶFAܩ�A�~�A�v�A�`BA�I�A�-A�{A�A��`A���Aۺ^A۰!Aۛ�A�x�A�ffA�VA�=qA�+A��A���A��yA���AڸRAڰ!Aڡ�AڃA�x�A�bNA�`BA�S�A�M�A�G�A�G�A�C�A�9XA�7LA�7LA�33A�/A�+A�-A�1'A�/A�$�A�"�A��A�{A�oA�oA�{A�VA�1A�JA�JA�A���A���A���A���A��A��A��A��A��yA��mA��yA��`A��HA��;A��;A��#A���A���A�ƨAٸRA٬A٥�Aٟ�Aٝ�Aٝ�Aٙ�AّhAٕ�Aُ\Aى7AفA�z�A�dZA�A�A��A�
=A���Aغ^A؇+A�$�A��mAק�A�7LA��
A�ȴA�=qA���AԑhA�p�A�?}A�7LA�33A�1'A�+A�&�A�&�A�(�A�&�A��A�bA���A��
AӬAӍPA�|�A�x�A�n�A�`BA�M�A�-A��A�JA��AҸRAҍPA�`BA�=qA�oA��Aѝ�A�S�A�1A�ȴA�$�A���A�hsA�hsA�{AƩ�A�dZA�/A���A�ĜAőhA�VA�1A���AčPA�1A�-AA�/A��TA���A�M�A���A��jA�n�A�M�A�7LA�+A��A��A���A��;A�7LA���A��DA� �A���A���A�hsA��DA��9A�\)A�$�A��`A��-A��hA�p�A�Q�A�ƨA�M�A�A�A��hA�ZA�"�A�{A��TA��!A��uA��PA�~�A�l�A�/A�A��A���A�1'A��#A�bNA��;A�=qA�ƨA���A��\A�dZA�Q�A�1'A�VA��mA��9A���A��A�+A�A�`BA�1A�p�A��A�z�A�\)A�5?A��A��A�oA�  A��A��A��HA��HA��A���A���A�ȴA��jA��!A���A���A�z�A��A���A�|�A�oA���A�v�A� �A��A���A���A�x�A�S�A�33A�VA��A��
A�ĜA���A�;dA���A�oA�Q�A��A��A�hsA�9XA��A�x�A��A�A�A��yA���A�VA�VA���A�`BA��A���A���A��PA�S�A��mA�XA�p�A�ĜA�9XA��`A��FA��+A�t�A�{A��9A�K�A���A�33A��PA��A�bA�M�A�Q�A��DA�&�A���A�&�A��A�I�A��HA���A�x�A�I�A��A�hsA�ĜA���A���A���A��DA�~�A�n�A�E�A�bA���A�S�A�
=A��HA���A��\A�p�A�C�A��A�ȴA�XA�oA��jA���A���A�A�dZA�;dA�JA��-A�x�A�?}A�{A�  A��`A��HA���A�ĜA���A��RA��FA���A���A���A���A��+A�~�A�jA�E�A�{A�  A�ĜA�|�A�/A���A��9A�`BA��jA�+A��A�ƨA~ �A|��A{\)Ay"�Au�-AuO�At�yAtZAs�As��As33Ar�uAq�mAp��Ao�^Aot�AoG�An�Anz�Am��Amx�Al��AlbNAkƨAk/Aj(�Ah�Ag��Af�9AeK�Ad5?Ab��Aal�A_�7A^n�A\��A[�AZ��AZE�AYAY7LAX�AX�uAXv�AX^5AX5?AXbAW��AW�TAW��AW��AW`BAWoAV�/AV�AU��AU�AUO�AU�AT��AT�RAT�DATI�AT1'AS�mAS�^AS��AS��ASl�AS?}AS�AR�yARv�AR9XAQ�#AQ�APVAP{AO�TAOAO��AO
=ANbAMK�AL�AL�!AL~�ALn�ALjALbNAL=qAKt�AJ��AI�#AI%AH�DAH(�AH  AG�AG�AG��AGC�AF�AFĜAF�+AFVAE�TAE�
AE��AEƨAE��AES�AE?}AE+AE�AD�AC��AC�AB�RAB1AA�AAG�AA�A@�9A@~�A@M�A@5?A?��A?ƨA?��A?�A?`BA?O�A??}A?�A?%A>�A>�HA>��A>I�A>  A=�A=�mA=��A=S�A=�A<�A<��A<�A<��A<��A<��A<�+A<bNA<=qA<(�A<1A;ƨA;�7A;G�A:�A:�A:z�A:ZA:E�A: �A9�A9�
A9ƨA9��A9��A9XA9oA8�`A8�\A8ZA8�+A8A�A8JA8{A8bA81A8  A8  A7��A7��A81A81A8  A7��A7�A7�;A7�#A7��A7��A7��A7��A7x�A6ZA5��A5A6A5�mA5�;A5��A5�wA5��A5��A5�hA5l�A57LA5%A4�HA4��A4��A4�DA4Q�A4-A41G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         Aݺ^Aݧ�Aݥ�Aݝ�A�v�A�dZA�`BA�Q�A�G�A�E�A�E�A�?}A�=qA�9XA�9XA�9XA�5?A�33A�1'A�+A��A���A�A�K�A۾wA�9XAھwA�bNA�=qA�/A��A�JA���A��A��/AٸRAٗ�A�^5A؁A��A�I�A�$�AӰ!A�7LA�K�AЃAǟ�A�=qA²-A��RA�&�A�?}A���A�jA���A��9A�-A���A���A��A� �A���A�-A�?}A���A�A���A�z�A�r�A���A�x�A��A���A��DA�As�-An��Ah�jA^-AX�RAWO�AUVAS�AQƨAN��ALE�AHĜAF��AE��AC��A@�HA?��A>�/A=�hA<��A;��A:�uA9�A8�\A81A8  A7�
A6=qA5�-A4��A4{A3A3/A2jA0��A/K�A-�A,��A,M�A,ZA-;dA,�A+A*��A*ZA)�;A)�PA)O�A(ĜA'|�A&ȴA&~�A&5?A%��A$��A#C�A"��A"�DA!�A!�
A!��A!ƨA!A!��A!|�A!%A (�A�hA+A��A1AA��AO�A��A�/A��A��A9XA�^AhsAXA
=A5?AJA�A�yA=qAA�`A��A�!AbNA�;A?}A��A^5A��AA��AjA�AdZA�HAffA9XA|�A��A��A�A�A�A(�A7LA��A��AbNAA�AAK�A
��A
v�A
 �A	��A	
=A~�A  A�
AC�A��A��A��A  A�-A|�A�jA��AbA?}A��A��A��AXA/A �`A ��A I�A �@��@�33@�+@��!@�O�@���@��u@�1'@�|�@��P@�C�@���@�E�@�@���@���@�@��@�?}@�D@�ƨ@���@�V@��T@�O�@�Ĝ@� �@@홚@�G�@��`@�Q�@�z�@�I�@��m@�|�@�!@��@�p�@�bN@睲@�"�@��H@�n�@�~�@�{@�7@��`@�A�@㕁@�dZ@�@�=q@�%@���@�|�@�
=@�^5@�$�@�p�@�V@���@���@܃@�9X@�"�@ڏ\@�^5@��@ى7@���@�Z@�\)@�n�@պ^@Ձ@�Ĝ@���@�Q�@�l�@���@�o@җ�@���@϶F@��y@��#@��@�7L@��`@��m@�\)@�o@��H@�V@���@�%@�I�@�|�@��@�~�@�^5@�$�@��@őh@ģ�@��@î@�@�@��#@�?}@���@�Q�@��P@�o@���@�@���@�&�@�I�@�  @��
@��P@�"�@��@���@�E�@��^@�X@��@��@� �@�ƨ@�|�@��H@��!@�$�@�@��#@��7@�X@���@�r�@���@�|�@�dZ@�\)@�\)@�"�@��+@��7@��@��@� �@��
@��@�|�@�K�@��H@��!@�v�@�-@�@��T@��7@�X@�7L@�Ĝ@�ƨ@�33@�ȴ@�ff@�{@��^@��h@�X@�&�@��`@��9@��D@�9X@�j@� �@��@�dZ@�33@��y@�^5@��@�@�x�@�p�@���@��9@�r�@�  @��P@�\)@���@�M�@���@�G�@��`@�r�@��m@���@�dZ@�"�@�ȴ@��@��h@�V@��/@��u@�j@�9X@�ƨ@�S�@���@�v�@�n�@�M�@��@��T@���@�p�@�/@��@�(�@��@��@��!@���@��!@���@�v�@�@�p�@�p�@���@��h@�G�@���@���@�(�@��;@��w@���@��P@�l�@�o@�~�@�J@��-@�X@�O�@�7L@�V@��j@�I�@�b@��w@�C�@���@��@��@�@�hs@�&�@���@�Ĝ@�j@�I�@�I�@�A�@�1'@� �@���@���@�|�@�o@�
=@��H@���@�v�@�$�@�x�@�?}@��@���@��9@�r�@��@��
@��w@�C�@�o@��@���@�v�@�-@��@��7@�&�@���@�z�@�I�@�ƨ@�t�@�C�@���@��y@��@��R@�~�@�=q@��@�@�X@��@��`@���@��@�b@\)@~�y@~�@~�@~��@~5?@~$�@}�@}��@}�-@}?}@|I�@{��@z��@z��@z~�@z^5@zJ@yhs@w��@w+@v��@v��@v$�@u�-@up�@uO�@t��@t�j@t(�@s�
@sdZ@s"�@r��@q��@q��@q�@p��@pbN@p1'@p  @o;d@n�@n�R@n��@nv�@nV@n{@m�T@m�-@m�h@m`B@m?}@m/@l��@lz�@lI�@l1@kƨ@kC�@jn�@jJ@i��@i�#@i��@i��@ix�@iG�@i%@h��@h��@hbN@g�P@g+@fȴ@f5?@e�@e�h@ep�@eO�@e�@d��@dz�@dZ@cƨ@cC�@c@b�\@b�@ax�@`Ĝ@`bN@`Q�@` �@_�;@^�@^�+@^E�@^{@]�T@]�h@]O�@\�/@\z�@\z�@\(�@[ƨ@[�@[C�@[@ZM�@YX@Y%@X�`@XĜ@XQ�@W;d@Vv�@V@U��@T��@T�D@TI�@T�@S�F@S"�@R��@R^5@Q�@Q��@Q��@Qhs@Q%@P�9@P�u@P�u@PA�@O|�@O+@N��@N�@Nȴ@N��@NE�@N@M�-@M�h@MO�@L��@L(�@KdZ@KS�@Ko@J�\@I�@Ix�@IG�@I�@I%@H��@HĜ@Hr�@Hb@G�@G��@G�w@G�@G�P@Gl�@G�@F�+@Fff@FE�@E�T@E�h@E/@D��@D�/@D��@Dz�@D9X@Cƨ@C��@CdZ@C33@B�@B�\@Bn�@Bn�@B^5@Ahs@A�@@��@@�@@�@@r�@@r�@@1'@?�;@?��@?|�@?K�@?�@>�R@>E�@=�@=�@<��@<(�@;�
@:�@:=q@:J@9�@9�^@9hs@8��@8r�@8Q�@8  @7��@7|�@6��@6ȴ@6�+@6v�@65?@5�@5�-@5`B@4��@4�j@4I�@49X@4�@3��@3o@2��@1�#@1hs@1&�@0�`@0Ĝ@0Q�@0  @/�;@/�P@/|�@/\)@/�@.��@.�y@.ȴ@.ff@-�@-@-�h@-�@,�j@,Z@,1@+�
@+ƨ@+ƨ@+ƨ@+��@+t�@+33@*��@*�!@*��@*�\@*n�@*�@)�#@)hs@(�9@(r�@(1'@'�@'�P@'l�@'K�@';d@'+@'�@&�y@&�R@&v�@&ff@&V@&E�@&5?@&@%�T@%��@%�-@%�h@%�h@%�h@%�h@%O�@%�@$��@$�/@$�/@$��@$�j@$��@$j@$�@#��@#ƨ@#�@#S�@#@"��@"^5@"�@"J@!�@!��@!�7@!hs@!&�@ �9@ r�@  �@�;@�@��@|�@|�@K�@�@ȴ@�R@�+@E�@{@�T@@�-@�h@�@�j@��@z�@z�@z�@z�@z�@9X@(�@1@��@�
@��@dZ@C�@@~�@�@��@hs@hs@X@&�@%@��@��@�9@�u@bN@A�@1'@b@�@��@�w@��@�@�y@ȴ@��@v�@V@$�@�T@��@��@p�@?}@V@�@�j@�D@I�@(�@�@�m@�@C�@@��@�\@~�@n�@�@��@�7@hs@7L@�`@�`@��@�@bN@A�@b@��@�w@�w@�w@��@l�@;d@�@
=@�@�R@ff@V@E�@E�@5?@@�@@`B@?}@/@��@�/@��@��@��@��@��@��@�@�@�m@��@�@�@�@t�G�O�AݬAݺ^Aݺ^AݶFAݺ^AݸRAݩ�Aݡ�Aݛ�Aݣ�Aݥ�AݬAݛ�Aݡ�Aݡ�Aݏ\A݁A�t�A�jA�ffA�bNA�bNA�ffA�dZA�`BA�VA�^5A�S�A�K�A�K�A�K�A�E�A�C�A�G�A�G�A�C�A�C�A�G�A�C�A�;dA�A�A�A�A�=qA�;dA�=qA�?}A�=qA�;dA�9XA�=qA�=qA�9XA�9XA�;dA�;dA�7LA�;dA�=qA�;dA�9XA�9XA�=qA�;dA�7LA�9XA�=qA�;dA�7LA�7LA�;dA�9XA�5?A�7LA�7LA�1'A�/A�5?A�7LA�5?A�1'A�5?A�5?A�1'A�/A�33A�1'A�-A�/A�1'A�1'A�/A�-A�1'A�1'A�(�A�&�A�&�A�$�A��A��A��A��A�VA�VA�JA�1A�A���A��HA��#A��/A��/A���A�ƨAܺ^AܶFAܩ�A�~�A�v�A�`BA�I�A�-A�{A�A��`A���Aۺ^A۰!Aۛ�A�x�A�ffA�VA�=qA�+A��A���A��yA���AڸRAڰ!Aڡ�AڃA�x�A�bNA�`BA�S�A�M�A�G�A�G�A�C�A�9XA�7LA�7LA�33A�/A�+A�-A�1'A�/A�$�A�"�A��A�{A�oA�oA�{A�VA�1A�JA�JA�A���A���A���A���A��A��A��A��A��yA��mA��yA��`A��HA��;A��;A��#A���A���A�ƨAٸRA٬A٥�Aٟ�Aٝ�Aٝ�Aٙ�AّhAٕ�Aُ\Aى7AفA�z�A�dZA�A�A��A�
=A���Aغ^A؇+A�$�A��mAק�A�7LA��
A�ȴA�=qA���AԑhA�p�A�?}A�7LA�33A�1'A�+A�&�A�&�A�(�A�&�A��A�bA���A��
AӬAӍPA�|�A�x�A�n�A�`BA�M�A�-A��A�JA��AҸRAҍPA�`BA�=qA�oA��Aѝ�A�S�A�1A�ȴA�$�A���A�hsA�hsA�{AƩ�A�dZA�/A���A�ĜAőhA�VA�1A���AčPA�1A�-AA�/A��TA���A�M�A���A��jA�n�A�M�A�7LA�+A��A��A���A��;A�7LA���A��DA� �A���A���A�hsA��DA��9A�\)A�$�A��`A��-A��hA�p�A�Q�A�ƨA�M�A�A�A��hA�ZA�"�A�{A��TA��!A��uA��PA�~�A�l�A�/A�A��A���A�1'A��#A�bNA��;A�=qA�ƨA���A��\A�dZA�Q�A�1'A�VA��mA��9A���A��A�+A�A�`BA�1A�p�A��A�z�A�\)A�5?A��A��A�oA�  A��A��A��HA��HA��A���A���A�ȴA��jA��!A���A���A�z�A��A���A�|�A�oA���A�v�A� �A��A���A���A�x�A�S�A�33A�VA��A��
A�ĜA���A�;dA���A�oA�Q�A��A��A�hsA�9XA��A�x�A��A�A�A��yA���A�VA�VA���A�`BA��A���A���A��PA�S�A��mA�XA�p�A�ĜA�9XA��`A��FA��+A�t�A�{A��9A�K�A���A�33A��PA��A�bA�M�A�Q�A��DA�&�A���A�&�A��A�I�A��HA���A�x�A�I�A��A�hsA�ĜA���A���A���A��DA�~�A�n�A�E�A�bA���A�S�A�
=A��HA���A��\A�p�A�C�A��A�ȴA�XA�oA��jA���A���A�A�dZA�;dA�JA��-A�x�A�?}A�{A�  A��`A��HA���A�ĜA���A��RA��FA���A���A���A���A��+A�~�A�jA�E�A�{A�  A�ĜA�|�A�/A���A��9A�`BA��jA�+A��A�ƨA~ �A|��A{\)Ay"�Au�-AuO�At�yAtZAs�As��As33Ar�uAq�mAp��Ao�^Aot�AoG�An�Anz�Am��Amx�Al��AlbNAkƨAk/Aj(�Ah�Ag��Af�9AeK�Ad5?Ab��Aal�A_�7A^n�A\��A[�AZ��AZE�AYAY7LAX�AX�uAXv�AX^5AX5?AXbAW��AW�TAW��AW��AW`BAWoAV�/AV�AU��AU�AUO�AU�AT��AT�RAT�DATI�AT1'AS�mAS�^AS��AS��ASl�AS?}AS�AR�yARv�AR9XAQ�#AQ�APVAP{AO�TAOAO��AO
=ANbAMK�AL�AL�!AL~�ALn�ALjALbNAL=qAKt�AJ��AI�#AI%AH�DAH(�AH  AG�AG�AG��AGC�AF�AFĜAF�+AFVAE�TAE�
AE��AEƨAE��AES�AE?}AE+AE�AD�AC��AC�AB�RAB1AA�AAG�AA�A@�9A@~�A@M�A@5?A?��A?ƨA?��A?�A?`BA?O�A??}A?�A?%A>�A>�HA>��A>I�A>  A=�A=�mA=��A=S�A=�A<�A<��A<�A<��A<��A<��A<�+A<bNA<=qA<(�A<1A;ƨA;�7A;G�A:�A:�A:z�A:ZA:E�A: �A9�A9�
A9ƨA9��A9��A9XA9oA8�`A8�\A8ZA8�+A8A�A8JA8{A8bA81A8  A8  A7��A7��A81A81A8  A7��A7�A7�;A7�#A7��A7��A7��A7��A7x�A6ZA5��A5A6A5�mA5�;A5��A5�wA5��A5��A5�hA5l�A57LA5%A4�HA4��A4��A4�DA4Q�A4-A41G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
;�B
<�B
;0B
<�B
=B
;dB
<B
;�B
;dB
;dB
;�B
;�B
;�B
;�B
;�B
<B
<6B
<B
<6B
<B
<�B
=qB
=<B
=�B
8�B
0�B
'RB
 �B
CB
�B
�B
�B
�B
�B
�B
B
�B	��B	�B	�[B	y	B	q�B	l�B	`�B	S&B	@B	,=B	�B	#nB	J�B	�=B	��B
Q�B
kB
�nB
�B
��B
�yBH�B��B�aB�<B�qB�qBm]B�B
��B
�B
��B
�MB
h>B
1�B
$tB
�B	�B	��B	|PB	d�B	<�B	�B		�B	B��B�.B	�B	 �B	\B	�B	�B	~B	#B	&B	)�B	1[B	I�B	aHB	k�B	n�B	qB	��B	��B	�0B	��B	�B	�B	��B	�VB
�B
.B
	7B
GB	�B	��B	�B
�B
!-B
.}B
-�B
.}B
.�B
.�B
-�B
+�B
+B
"�B
�B
!�B
"hB
-�B
,=B
 \B
IB
�B
!�B
!�B
"�B
#�B
%FB
)�B
0!B
33B
49B
=B
@OB
?}B
@�B
C�B
FtB
F�B
E9B
F�B
IB
J�B
K�B
OB
MjB
L0B
L0B
K�B
L0B
M6B
MjB
J�B
K�B
K�B
M�B
MjB
M�B
MjB
N�B
LdB
L0B
I�B
EmB
I�B
H�B
GzB
J�B
G�B
E�B
J�B
H�B
J�B
Q�B
S�B
TaB
V�B
Q�B
M�B
MB
MB
L�B
L�B
M6B
K�B
K�B
K)B
K^B
G�B
FB
A�B
>B
=<B
<jB
:�B
=qB
>BB
;0B
8�B
9�B
6�B
7B
6B
.IB
.B
,qB
)�B
(�B
'�B
(�B
'�B
&�B
%FB
)*B
)*B
+kB
.B
/B
,B
+�B
,=B
*0B
*0B
/�B
/B
/B
-�B
.�B
-wB
,�B
+�B
+B
)_B
(�B
(�B
'�B
'RB
'B
%�B
%�B
$�B
!bB
VB
�B
�B
�B
 �B
 \B
 �B
 \B
B
�B
qB
�B
$B
�B
kB
CB
�B
�B
B
�B
xB
B
�B
�B
�B
B
�B
�B
SB
�B
�B
�B
�B
oB
 B
.B
�B
xB
�B

�B

=B
	B
�B
�B
�B
YB
�B
�B
1B
1B
�B
�B
�B
	�B
YB
%B
�B
�B
�B

	B
�B
�B
PB
�B
PB
"B
�B
B
�B
B
~B
�B
JB
B
�B
B
JB
�B
�B
~B
�B
�B
B
JB
~B
~B
JB
~B
B
�B
B
�B
B
B
JB
~B
~B
�B
�B
~B
~B
�B
~B
~B
�B
�B
�B
PB
"B
�B
�B
�B
PB
�B
�B
�B
�B
VB
"B
"B
�B
�B
�B
(B
�B
�B
(B
�B
�B
�B
�B
�B
�B
(B
�B
�B
�B
�B
�B
�B
�B
�B
VB
"B
VB
�B
�B
�B
�B
\B
(B
�B
�B
B
�B
$B
�B
�B
YB
_B
�B
�B
_B
+B
eB
1B
�B
�B
B
kB
�B
=B
�B
�B
�B
�B
�B
CB
xB
xB
�B
�B
�B
�B
IB
IB
~B
~B
~B
OB
�B
�B
�B
�B
�B
�B
VB
�B
VB
 \B
 �B
!�B
"hB
"hB
"�B
$�B
'B
'�B
'RB
'�B
'�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*eB
*�B
+B
,qB
,B
,B
.B
.�B
.�B
.�B
/OB
0UB
0UB
0�B
1'B
1�B
2�B
2�B
2�B
2�B
33B
3hB
3�B
4B
5B
4�B
4�B
4�B
5B
4�B
5B
4�B
4�B
5?B
5�B
5�B
6B
5�B
6zB
7�B
7B
7B
7LB
7�B
7�B
7�B
8�B
8�B
9$B
8�B
8�B
8�B
8�B
8�B
9$B
9�B
:^B
:^B
;dB
;dB
<6B
<�B
<�B
=B
<�B
<�B
<�B
=<B
=qB
=�B
=�B
?HB
>�B
?HB
?B
@OB
@�B
A�B
A�B
AUB
A B
A�B
A�B
A�B
A�B
A�B
A�B
B'B
B�B
C�B
C�B
C�B
C�B
CaB
CaB
D�B
F?B
E�B
E�B
F?B
FB
F�B
FtB
F�B
F�B
F�B
G�B
GzB
G�B
G�B
HB
H�B
H�B
IRB
H�B
I�B
IRB
I�B
J�B
J�B
J�B
J�B
J�B
K)B
K^B
K^B
K�B
K�B
K�B
K�B
K�B
K�B
L0B
L0B
LdB
LdB
MjB
NB
M�B
M�B
M�B
N<B
NB
NB
NpB
NpB
NpB
N�B
NpB
PB
OBB
PB
PB
PHB
P�B
P}B
P}B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
RTB
R�B
S[B
S&B
S&B
S&B
S&B
T�B
T,B
TaB
TaB
T�B
T�B
T�B
T�B
U2B
T�B
U2B
UgB
UgB
UgB
UgB
V9B
V�B
V�B
VmB
VmB
VmB
XB
WsB
WsB
W�B
XyB
XEB
XB
XEB
XEB
XEB
XB
X�B
X�B
X�B
X�B
YB
YB
YB
YB
YKB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[WB
[�B
[�B
[�B
\�B
]/B
^5B
^B
^�B
_;B
`BB
`B
`BB
`vB
`�B
`�B
aB
aB
aB
aHB
a|B
a|B
a|B
a|B
a�B
bNB
b�B
b�B
b�B
b�B
cTB
c�B
c�B
d&B
d&B
dZB
d�B
d�B
d�B
e,B
e`B
e�B
e�B
e�B
e�B
e`B
gB
f�B
gB
gB
f�B
f�B
f�B
g8B
g8B
gB
g�B
gmB
gmB
g�B
h
B
hsB
h�B
iDB
iDB
iDB
j�B
kB
j�B
kB
kB
k�B
k�B
k�B
k�B
lWB
l"B
l�B
l�B
l�B
m)B
m)B
m]B
m�B
m�B
m�B
m�B
ncB
n�B
n�B
n�B
o B
o5B
o�B
poB
poB
p�B
p�B
p�B
qAB
qvB
qvB
q�B
q�B
rB
rGB
rGB
rGB
rGB
r�B
sB
sB
sMB
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u%B
uZB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v�B
v�B
w2B
wfB
w�B
w�B
xB
xB
xB
xB
x8B
x8B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y>B
yrB
yrB
y>B
yrB
y�B
y�B
zB
zB
zB
zB
zB
zB
zDB
zxB
zxB
z�B
z�B
z�B
{B
{B
{�B
{�B
{�B
{�B
|B
|B
|B
|�B
|�B
|�B
}"B
}�B
}�B
}�B
}�B
}�B
}�B
~(B
}�B
}�B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
�B
�B
� B
� B
� B
�B
�B
�B
�4B
� B
�4B
�4B
�4B
��B
��B
��B
�;B
�;B
��B
��B
�uB
�uB
�uB
��B
��B
�B
�GB
�{B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�SB
��B
��B
��B
��B
�%B
�YB
��B
��B
��B
��B
��B
�_B
�_B
��B
��B
��B
��B
��B
��B
�fB
��B
��B
�B
�B
�B
�B
��B
�	B
��B
�	B
�=B
��B
�rB
�B
��B
�B
�B
�xB
��B
��B
��B
��B
�xB
�~B
�JB
�~B
�~B
��B
��B
�PB
�PB
�B
�B
�PB
��B
�PB
��B
��B
��B
�"B
�VB
��B
�VB
��B
�VB
�VB
�"B
�"B
�VB
�(B
�(B
��B
��B
��B
��B
��B
��B
:�B
9$B
;0B
=�B
<jB
=<B
>BB
:�B
<6B
<�B
:�B
:�B
?}B
9�B
;0B
B'B
8�B
D�B
>BB
;�B
<6B
;�B
:�B
;0B
=qB
;0B
9�B
<6B
<B
;�B
9�B
;dB
<B
;�B
:�B
<jB
;�B
:^B
;0B
<�B
;�B
:�B
;dB
<�B
<6B
:^B
:�B
<6B
<�B
;dB
:�B
<�B
<jB
;0B
;dB
=B
;�B
:�B
;�B
<�B
<B
:�B
;dB
<�B
<jB
:�B
;dB
<�B
<6B
:�B
<B
=<B
<�B
:�B
=<B
=B
<B
:�B
<B
=<B
;0B
;dB
<�B
<�B
;�B
;dB
=<B
=B
<B
:�B
<B
<�B
;�B
;0B
<jB
<6B
;dB
;�B
=B
<B
;dB
<�B
>wB
;�B
<�B
=<B
=�B
<�B
?�B
>B
<�B
;�B
>wB
?B
>�B
<B
=qB
C�B
<�B
>�B
=B
>B
>wB
;�B
;�B
;�B
7�B
6�B
6zB
7B
2�B
0�B
1'B
/OB
.�B
,=B
*0B
*�B
)_B
#:B
%�B
#�B
$B
#B
�B
B
�B
OB
=B
B
�B
B
	B
�B
B
7B
�B
+B
�B
qB
B
�B
�B
_B
_B
�B
�B
�B
�B
YB
YB
_B
$B
�B
MB
�B
�B
�B
B
�B
�B
uB
�B
uB
�B
�B
�B
.B
�B
�B
\B
�B
B
	lB
�B
B
�B
YB
B
�B
�B
oB
 iB	�JB	��B	�GB	�B	�/B	�B	�|B	خB	��B	��B	�B	��B	ǮB	��B	�PB	�uB	|�B	}�B	u%B	s�B	r�B	s�B	s�B	r�B	qvB	p;B	q�B	qvB	q�B	p�B	n�B	l�B	gmB	d�B	d�B	c B	b�B	e�B	\�B	[#B	\]B	Z�B	YB	U2B	NB	MjB	L0B	G�B	G�B	@OB	7�B	?�B	#�B	r|B	{�B	T,B	�B��B��B	�B	�B	�B	�B	B	�B		7B	qB	*�B	(XB	$@B	$�B	)*B	8�B	?�B	H�B	W�B	XEB	[WB	[�B	]/B	g�B	��B	�BB	��B	��B	��B	��B
  B
�B
B
C�B
]�B
`BB
]dB
d&B
h
B
f2B
e�B
g�B
{B
��B
�UB
��B
��B
��B
�$B
�'B
�'B
�*B
�$B
��B
�_B
��B
ϫB
��B
�9B
�B
�dB
�&B
רB
�vB
՛B
҉B
�mB
�mB
��B
�B
ٴB
�/B
��B
�KB
�B
��B
�B�B�BZB��B��B�dB��B��B��B�jB��B��B�B�BB�jB�BB�B�6B��B�6B��B��B�$B�qB��BÖB�0BҽBѷB�BԕB��B�<B�6B�jB��BʌB�XBɺB��BĜB�B˒B�KB��B��B�9B�B�B��B�B�nB��B��B�B�"B�DB�MB��B�4B�4Bt�Bv�Bq�Bm�Br|Bk�B^�BP}B2-B(�B�BB�B:B
�DB
��B
��B�B
�TB
��B
˒B
��B
�zB
�CB
��B
�kB
��B
��B
��B
��B
~�B
zDB
~�B
��B
~�B
��B
�CB
�SB
��B
�B
�MB
��B
��B
��B
�MB
��B
��B
�%B
��B
}�B
x�B
|PB
y	B
y�B
u�B
{�B
l�B
��B
��B
WsB
M�B
<�B
B[B
@�B
=qB
8RB
2aB
.�B
.}B
,=B
,qB
*�B
(�B
)_B
'�B
$�B
&B
%B
 �B
#�B
�B
 �B
 �B
%zB
�B
�B
B
hB
�B
	�B
�B
B	��B	�cB
GB	�cB	�aB	��B	��B	ޞB	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	� B	{�B	�4B	{JB	t�B	t�B	rB	m)B	k�B	i�B	k�B	lWB	[�B	b�B	S[B	PB	N�B	U�B	J�B	9XB	B�B	,�B	~B	B	�B	B	�B	\B	"B	"B	B	JB	
�B		B	�B	
=B		lB	�B	�B	1B	B	�B	B	 �B	B	 �B	AB	B��B	B��B��B�B��B��B��B�"B�cB��B��B		�B	�B�.B��B�VB��B	�B	�B	1B	�B	B�(B��B�PB�PB�]B	 B	
rB	�B	�B	�B	bB	�B	�B	DB	B	�B	�B	FB	@B	�B		B	�B	oB	:B	oB	�B	4B	bB	�B	�B	$tB		B	.B	�B	%FB	 'B	#B	$tB	%zB	#:B	"�B	+6B	%�B	$tB	$�B	&�B	%�B	%B	($B	'�B	'�B	'�B	(XB	<B	-wB	-wB	-CB	7B	8B	49B	4�B	6�B	A B	J�B	K�B	T�B	WsB	W?B	V�B	Z�B	d�B	j�B	jB	k�B	n/B	m]B	iyB	j�B	jB	m�B	l�B	l�B	m�B	p;B	o�B	s�B	i�B	p�B	m)B	l�B	kB	��B	�oB	.B	�DB	��B	�B	�oB	�B	��B	�SB	�bB	�zB	��B	�eB	�IB	�B	�*B	�B	�'B	�XB	ӏB	�,B	�<B	�B	�
B	�sB	�B	��B	�)B	��B	��B	��B	��B	�MB	�B	��B	�ZB	��B	��B	�2B	�`B	�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         B
<�B
=B
;�B
>B
=�B
;�B
<�B
;�B
;�B
;�B
;�B
;�B
;�B
;�B
;�B
<B
<GB
<B
<eB
<�B
=gB
>�B
?�B
@�B
;B
37B
)SB
!jB
�B
=B
`B
]B
�B
OB
hB
�B
SB
�B	�B	��B	z�B	t~B	o�B	f�B	`�B	m�B	;B	�B	.�B	TqB	��B
�B
Y�B
y;B
��B
�GB
��B
�1BM�B�YB�lBމB��B��B��B-�B
ͩB
�WB
��B
�AB
{�B
5dB
-�B
45B
'B	�6B	�LB	�B	N0B	hB	2B	,B	�B		wB	iB	�B	rB	tB	[B	'>B	'}B	(�B	.!B	4�B	L8B	fB	n�B	rXB	r�B	��B	��B	�]B	��B	�B	��B	�aB	�_B
gB
B
B
	IB	�;B	�B	��B	��B
"�B
1�B
0�B
0�B
0�B
0B
.�B
-�B
/;B
%"B
 "B
"�B
$�B
1B
1B
"B
yB
!�B
"+B
!�B
"�B
#�B
%�B
*�B
1�B
6*B
6nB
>[B
AIB
B�B
A�B
D�B
G�B
G�B
E�B
GB
I�B
LeB
M�B
PB
M�B
M~B
N�B
L�B
M�B
O�B
O�B
LxB
N�B
LhB
M�B
N�B
O�B
O�B
O�B
N�B
N�B
KoB
FKB
K,B
JB
I�B
L�B
I[B
FzB
MPB
H`B
K,B
RB
S�B
U�B
ZB
U;B
O{B
M�B
M�B
MnB
N�B
N�B
M>B
MQB
L}B
M[B
I�B
HB
C�B
>�B
?1B
=zB
:�B
>;B
@�B
<_B
9�B
<KB
7wB
9=B
8�B
0�B
0wB
-�B
*�B
)lB
(�B
)�B
(�B
'B
&9B
)�B
)XB
,xB
0�B
/�B
,�B
,lB
-bB
*<B
*�B
0�B
0$B
0B
/fB
1 B
/B
.�B
-!B
,SB
*�B
*:B
*B
(�B
(jB
(+B
'B
(xB
&lB
"B
 "B
�B
�B
 B
!�B
!FB
"lB
!vB
�B
�B
�B
�B
�B
�B
fB
B
�B
�B
ZB
�B
�B
�B
RB
�B
�B
B
�B
�B
�B
9B
7B
�B
0B
B
�B
B
�B
�B
yB
sB
[B

9B

�B
	�B
;B
�B
EB
B
	B
	�B
�B
hB
	�B
�B
�B
�B
�B
	!B
�B

�B
�B
�B
�B
&B
[B
7B
cB
�B
8B
#B
QB
�B
�B
�B
�B
�B
_B
�B
B
`B
MB
�B
�B
�B
�B
pB
CB
�B
�B
�B
�B
EB
tB
�B
B
B
�B
nB
�B
BB
SB
{B
?B
3B
TB
�B
�B
OB
kB
B
B
$B
B
�B
$B
XB
�B
nB
5B
�B
8B
�B
zB
 B
�B
UB
�B
ZB
,B
�B
ZB
B
�B
�B
B
�B
^B
B
�B
�B
�B
�B
B
�B
B
MB
fB
]B
sB
�B
�B
B
�B
�B
�B
�B
+B
�B
aB
=B
$B
OB
�B
B
�B
�B
�B
�B
zB
wB
�B
7B
�B
nB
�B
�B
GB
�B
B
FB
{B
�B
�B
B
�B
�B
�B
`B
nB
�B
BB
�B
�B
�B
�B
 /B
�B
 B
 WB
!dB
!�B
"�B
#+B
"�B
"�B
$�B
'�B
(�B
(dB
'�B
'aB
*3B
+`B
+pB
+vB
+�B
+,B
*�B
*�B
*�B
*�B
+�B
-�B
,�B
,�B
.�B
/ B
.�B
/AB
/�B
1/B
0�B
17B
2%B
3B
3�B
3'B
2�B
3{B
3�B
3�B
4HB
4�B
5JB
4�B
4�B
4�B
52B
4�B
5�B
59B
5�B
5`B
5�B
6PB
6�B
6�B
7�B
8#B
7_B
7sB
7�B
8	B
8.B
8mB
8�B
9�B
9�B
9
B
9MB
9'B
9B
9EB
9�B
:MB
:�B
;KB
;�B
<aB
<�B
=<B
=\B
=.B
<�B
<�B
=IB
=�B
>B
>FB
>�B
?�B
?QB
?yB
?�B
A0B
A?B
A�B
A�B
A`B
AfB
BB
A�B
A�B
BB
A�B
BB
CB
C�B
DTB
DB
C�B
C�B
C�B
DB
F*B
F�B
FB
F=B
F�B
F~B
G B
F�B
F�B
G*B
G7B
HB
G�B
H+B
HQB
H�B
IPB
IlB
I�B
IZB
I�B
I�B
JDB
J�B
J�B
J�B
KB
KB
KkB
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LsB
LgB
LvB
L�B
L�B
N5B
NhB
M�B
M�B
NB
NQB
N-B
N@B
N�B
N�B
N�B
N�B
OBB
PvB
O�B
P�B
P]B
P�B
P�B
P�B
P�B
QB
Q5B
QB
QFB
R9B
Q�B
R(B
RkB
R�B
S5B
S�B
SAB
S^B
SyB
TB
T�B
TtB
T�B
T�B
T�B
UB
UpB
U[B
U>B
UUB
U�B
U�B
U�B
U�B
V"B
WB
W)B
V�B
V�B
V�B
WyB
X�B
W�B
W�B
X�B
X�B
X�B
XLB
X�B
X�B
X�B
XxB
YB
Y#B
X�B
YB
YzB
Y�B
Y�B
Y�B
Y�B
Z�B
[B
[!B
[B
[B
[B
[QB
[�B
[�B
[�B
[�B
\rB
]B
]�B
^OB
^MB
_YB
_�B
`�B
`GB
`sB
`�B
`�B
`�B
aeB
atB
a;B
akB
a�B
a�B
a�B
a�B
bB
b�B
b�B
b�B
cB
c@B
c�B
c�B
c�B
deB
dPB
d�B
d�B
e B
e:B
ebB
e�B
e�B
e�B
e�B
e�B
fMB
gZB
g;B
g'B
g
B
f�B
f�B
gB
g�B
gTB
gYB
g�B
g�B
g�B
h!B
h�B
h�B
i(B
i�B
i�B
j'B
kaB
kSB
kB
kUB
k{B
l?B
l#B
lB
lAB
l�B
l}B
mB
l�B
m5B
mBB
mmB
m�B
m�B
m�B
n[B
nFB
n�B
n�B
n�B
oB
o�B
o�B
p]B
p�B
p�B
qB
p�B
qB
q�B
q�B
q�B
q�B
q�B
rTB
rjB
r^B
rqB
r�B
sTB
sPB
sTB
s�B
tQB
tRB
t�B
t�B
uB
t�B
t�B
uB
t�B
ulB
u�B
u�B
u�B
u�B
u�B
u�B
vAB
vtB
v�B
wAB
wEB
w|B
w�B
w�B
w�B
xB
xB
xB
x8B
xlB
xyB
x�B
x�B
x�B
x�B
x�B
x�B
yB
y-B
y`B
yvB
yvB
yIB
y�B
y�B
z B
z1B
zB
z$B
z&B
z6B
zHB
z�B
z�B
z�B
z�B
{B
{4B
{^B
{{B
{�B
{�B
{�B
{�B
|AB
|DB
|gB
|�B
}B
}@B
}gB
}�B
}�B
}�B
}�B
}�B
~,B
~=B
~B
~)B
~�B
~�B
~�B
B
~�B
#B
<B
�+B
�B
� B
�B
�B
�B
�B
�
B
�JB
�$B
�IB
�[B
�jB
��B
��B
��B
��B
��B
�WB
��B
�|B
��B
��B
� B
��B
�7B
�kB
��B
��B
�<B
��B
��B
��B
��B
��B
�B
�3B
��B
��B
��B
��B
�B
�YB
��B
�B
��B
��B
�*B
�*B
��B
��B
��B
��B
�B
��B
�4B
�`B
��B
��B
��B
�B
�B
�B
�\B
�B
� B
��B
�AB
��B
��B
��B
�2B
�B
�5B
�FB
��B
��B
��B
��B
��B
��B
��B
�mB
��B
��B
��B
�5B
�eB
�cB
�!B
�2B
��B
��B
��B
��B
�B
�B
�TB
�yB
��B
�ZB
��B
�ZB
�YB
�*B
�OB
��B
�^B
�kB
��B
��B
��B
��B
��G�O�B
:�B
9$B
;0B
=�B
<jB
=<B
>BB
:�B
<6B
<�B
:�B
:�B
?}B
9�B
;0B
B'B
8�B
D�B
>BB
;�B
<6B
;�B
:�B
;0B
=qB
;0B
9�B
<6B
<B
;�B
9�B
;dB
<B
;�B
:�B
<jB
;�B
:^B
;0B
<�B
;�B
:�B
;dB
<�B
<6B
:^B
:�B
<6B
<�B
;dB
:�B
<�B
<jB
;0B
;dB
=B
;�B
:�B
;�B
<�B
<B
:�B
;dB
<�B
<jB
:�B
;dB
<�B
<6B
:�B
<B
=<B
<�B
:�B
=<B
=B
<B
:�B
<B
=<B
;0B
;dB
<�B
<�B
;�B
;dB
=<B
=B
<B
:�B
<B
<�B
;�B
;0B
<jB
<6B
;dB
;�B
=B
<B
;dB
<�B
>wB
;�B
<�B
=<B
=�B
<�B
?�B
>B
<�B
;�B
>wB
?B
>�B
<B
=qB
C�B
<�B
>�B
=B
>B
>wB
;�B
;�B
;�B
7�B
6�B
6zB
7B
2�B
0�B
1'B
/OB
.�B
,=B
*0B
*�B
)_B
#:B
%�B
#�B
$B
#B
�B
B
�B
OB
=B
B
�B
B
	B
�B
B
7B
�B
+B
�B
qB
B
�B
�B
_B
_B
�B
�B
�B
�B
YB
YB
_B
$B
�B
MB
�B
�B
�B
B
�B
�B
uB
�B
uB
�B
�B
�B
.B
�B
�B
\B
�B
B
	lB
�B
B
�B
YB
B
�B
�B
oB
 iB	�JB	��B	�GB	�B	�/B	�B	�|B	خB	��B	��B	�B	��B	ǮB	��B	�PB	�uB	|�B	}�B	u%B	s�B	r�B	s�B	s�B	r�B	qvB	p;B	q�B	qvB	q�B	p�B	n�B	l�B	gmB	d�B	d�B	c B	b�B	e�B	\�B	[#B	\]B	Z�B	YB	U2B	NB	MjB	L0B	G�B	G�B	@OB	7�B	?�B	#�B	r|B	{�B	T,B	�B��B��B	�B	�B	�B	�B	B	�B		7B	qB	*�B	(XB	$@B	$�B	)*B	8�B	?�B	H�B	W�B	XEB	[WB	[�B	]/B	g�B	��B	�BB	��B	��B	��B	��B
  B
�B
B
C�B
]�B
`BB
]dB
d&B
h
B
f2B
e�B
g�B
{B
��B
�UB
��B
��B
��B
�$B
�'B
�'B
�*B
�$B
��B
�_B
��B
ϫB
��B
�9B
�B
�dB
�&B
רB
�vB
՛B
҉B
�mB
�mB
��B
�B
ٴB
�/B
��B
�KB
�B
��B
�B�B�BZB��B��B�dB��B��B��B�jB��B��B�B�BB�jB�BB�B�6B��B�6B��B��B�$B�qB��BÖB�0BҽBѷB�BԕB��B�<B�6B�jB��BʌB�XBɺB��BĜB�B˒B�KB��B��B�9B�B�B��B�B�nB��B��B�B�"B�DB�MB��B�4B�4Bt�Bv�Bq�Bm�Br|Bk�B^�BP}B2-B(�B�BB�B:B
�DB
��B
��B�B
�TB
��B
˒B
��B
�zB
�CB
��B
�kB
��B
��B
��B
��B
~�B
zDB
~�B
��B
~�B
��B
�CB
�SB
��B
�B
�MB
��B
��B
��B
�MB
��B
��B
�%B
��B
}�B
x�B
|PB
y	B
y�B
u�B
{�B
l�B
��B
��B
WsB
M�B
<�B
B[B
@�B
=qB
8RB
2aB
.�B
.}B
,=B
,qB
*�B
(�B
)_B
'�B
$�B
&B
%B
 �B
#�B
�B
 �B
 �B
%zB
�B
�B
B
hB
�B
	�B
�B
B	��B	�cB
GB	�cB	�aB	��B	��B	ޞB	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	� B	{�B	�4B	{JB	t�B	t�B	rB	m)B	k�B	i�B	k�B	lWB	[�B	b�B	S[B	PB	N�B	U�B	J�B	9XB	B�B	,�B	~B	B	�B	B	�B	\B	"B	"B	B	JB	
�B		B	�B	
=B		lB	�B	�B	1B	B	�B	B	 �B	B	 �B	AB	B��B	B��B��B�B��B��B��B�"B�cB��B��B		�B	�B�.B��B�VB��B	�B	�B	1B	�B	B�(B��B�PB�PB�]B	 B	
rB	�B	�B	�B	bB	�B	�B	DB	B	�B	�B	FB	@B	�B		B	�B	oB	:B	oB	�B	4B	bB	�B	�B	$tB		B	.B	�B	%FB	 'B	#B	$tB	%zB	#:B	"�B	+6B	%�B	$tB	$�B	&�B	%�B	%B	($B	'�B	'�B	'�B	(XB	<B	-wB	-wB	-CB	7B	8B	49B	4�B	6�B	A B	J�B	K�B	T�B	WsB	W?B	V�B	Z�B	d�B	j�B	jB	k�B	n/B	m]B	iyB	j�B	jB	m�B	l�B	l�B	m�B	p;B	o�B	s�B	i�B	p�B	m)B	l�B	kB	��B	�oB	.B	�DB	��B	�B	�oB	�B	��B	�SB	�bB	�zB	��B	�eB	�IB	�B	�*B	�B	�'B	�XB	ӏB	�,B	�<B	�B	�
B	�sB	�B	��B	�)B	��B	��B	��B	��B	�MB	�B	��B	�ZB	��B	��B	�2B	�`B	�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<\�E<%��<#�
<#�
<#�
<#�
<e�=8<<v�a<{t�<@n2<+�X<��<c�<#�
<k�l<#�
<1C�<���<V[�<#�
<#�
<z/�<��E<��y<��]<���=T�<��<�P<ZǠ<���<�g�<#�
<#�
=��=c�<�}�<�h�=k"<���<#�
<#�
<#�
<#�
<0��<#�
<?��<#�
<#�
<#�
<(ݲ<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2020080322511920200803225119IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020081322004620200813220046QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020081322004620200813220046QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020082411445520200824114455IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                