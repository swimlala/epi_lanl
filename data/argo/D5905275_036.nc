CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-01-06T18:16:41Z creation; 2023-04-26T19:14:27Z DMQC;      
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
_FillValue        G�O�     p  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     p  dT   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     p  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     p  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʀ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     p  �\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     p  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     p    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     p @d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     p g�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190106181641  20230426191427  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               $   $AA  AOAO7316_008644_036                 7316_008644_036                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @؝�O�;@؝�O�;11  @؝��y��@؝��y��@*��,�d�@*��,�d��c�9���c�9��11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@@  @�  @�  @��R@�  A   A\)A\)A+�A@  A`��A�  A��A��A�Q�A�  AϮA�  A�A��B  B(�B  B   B((�B/�
B7�B?�
BH  BP  BX  B_�
Bg�Bo�
Bw�
B�  B�{B�  B�{B�Q�B�
=B��
B��B�  B�{B�  B�  B��B��B�  B�  B�  B�  B�  B�  B��
B��B�{B�{B�  B��
B��
B�  B�{B�  B��B��B��C��C��C��C��C
  C  C�C��C  C�C��C��C��C��C
=C �C"  C#��C&  C(  C*
=C,
=C.  C0  C2
=C4
=C6  C7��C:  C;��C=��C@
=CB
=CD  CF  CH
=CJ
=CL  CN  CP
=CR
=CT{CV{CX  CZ  C\
=C]��C_��Cb  Cc��Ce��Cg��Cj  Cl
=Cn  Cp  Cr  Ct  Cu��Cw��Cy��C|  C~  C�  C�C�C�C�  C���C�  C�  C�C�  C�  C�C�C�  C�  C�  C�  C�C�  C�  C���C�  C�  C���C���C���C�  C�C�  C���C���C�  C�  C���C�  C�  C���C�  C�
=C�C�C�C�C�C�C�  C�  C�  C�  C�  C�  C�  C���C�C�
=C�C�  C�  C���C�  C�  C���C���C���C���C�  C�  C���C���C���C���C�  C�C�
=C�C�  C�  C���C�  C�  C�  C�  C�C�  C���C�  C�C�  C�  C�  C�  C�
=C�
=C�  C���C�  C�
=C�
=C�C�C�C�C���C���C���C�  C�
=C�C���C���C���C�  C�  C�C�C�C�  C�  C���C���C�  C�
=C�C�C�C�  C�  C���D   D }qD  D��D�qDz�D�qD� D�D�D�D}qD  D��D�qD}qD  D� D�qD	� D
  D
� D
�qD� D�D� D�qDz�D��D� DD�DD��D  D}qD�qD}qD�qD}qD�qD� D  D� D�D��D�D��DD� D�qD}qD  D��D�D�D  D}qD  D��DD��D�qD� D �D ��D!�D!� D!�qD"}qD#  D#� D$  D$�D%�D%��D&  D&� D&�qD'� D(  D(� D)  D)z�D)�qD*}qD*�qD+� D+��D,z�D-�D-��D.�D.��D.�qD/� D0  D0� D1�D1� D2  D2��D3  D3� D4�D4� D4�qD5� D6�D6� D6�qD7��D8D8� D8�qD9� D:�D:� D:�qD;}qD<  D<��D=  D=��D>D>��D?  D?� D@�D@}qD@��DAz�DB  DB�DC�DC� DD  DD}qDD��DE� DF  DF}qDG  DG}qDH  DH�DI�DI��DI�qDJ��DK  DK}qDL�DL� DL�qDMz�DN  DN�DO�DO}qDO�qDP� DQ�DQ� DR  DR� DS  DS� DS�qDT}qDU  DU��DV  DV� DW  DW}qDW�qDX}qDY  DY��DZ�DZ��D[  D[}qD\  D\��D]  D]}qD]�qD^� D_  D_��D`D`� D`�qDa� Db  Db��Dc�Dc��Dd  Dd}qDd�qDe� Df  Df��Dg�Dg� Dg�qDh}qDi  Di��Dj  Dj}qDk  Dk� Dl  Dl}qDm  Dm� Dm�qDn}qDn�qDo}qDp  Dp� Dp�qDq}qDr  Dr��Ds  Ds� Dt�Dt��Du  Du}qDv  Dv� Dv�qDw��Dx  Dx� Dy  Dy� Dz  Dz}qDz�qD{��D|D|� D|�qD}� D~�D~� D  D}qD�  D�AHD��HD���D���D�AHD�~�D�� D�HD�AHD��HD�� D�  D�@ D�~�D���D���D�@ D��HD�D�HD�@ D��HD�� D���D�@ D��HD�� D���D�@ D�~�D�� D�HD�@ D�� D�� D�HD�@ D�~�D���D���D�>�D�}qD�� D�  D�@ D�� D�� D�HD�AHD�~�D�� D�  D�>�D�~�D���D��qD�>�D�� D��HD�  D�@ D�� D���D���D�AHD�� D���D�  D�AHD�� D�� D���D�AHD��HD��qD���D�@ D�� D��HD�HD�@ D�� D���D���D�>�D�~�D��HD��D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D��qD���D�AHD���D��HD�HD�B�D��HD��HD�  D�@ D�~�D���D�HD�B�D�� D���D���D�@ D�� D��HD�  D�>�D�~�D�� D�HD�@ D��HD�D�HD�@ D�~�D��HD�  D�>�D��HD�� D�  D�AHD��HD��HD�HD�AHD�� D��qD���D�@ D�~�D��qD���D�@ D�� D�� D���D�>�D�� D�� D�  D�B�D��HD�� D���D�@ D�� D���D��qD�>�D��HD�� D�  D�>�D�~�D���D���D�=qD�� D�� D�HD�AHD�~�D�� D�  D�>�D�~�D���D���D�>�D�~�D�� D�HD�@ D�~�D��HD�HD�B�D���D���D���D�>�D�~�D���D���D�>�D�� D�� D�  D�=qD�~�D�� D���D�=qD�~�D��HD��D�AHD�� D�� D���D�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D�� D�� D�  D�>�D�~�D���D���D�@ D�� D�� D�  D�@ D�~�D�� D�  D�AHD�� D�� D�HD�@ D��HD��HD�HD�AHD��HD��HD���D�>�D�� D�� D���D�@ D��HD�� D���D�@ D��HD��HD�  D�AHD�~�D�� D�  D�>�DÀ D�� D���D�>�DĀ D�� D���D�>�DŁHD�D�HD�@ DƁHD�D�HD�@ Dǀ D�� D���D�>�D�~�D�� D�HD�B�DɁHD��HD�HD�@ Dʀ Dʾ�D��qD�=qDˀ D˾�D�  D�@ D�~�D̾�D���D�>�D́HD��HD�HD�AHD�~�D�� D�  D�AHDρHD��HD���D�>�DЀ D�� D�  D�@ D�~�DѾ�D�  D�AHDҁHD�� D���D�@ DӀ D��HD�HD�AHDԀ DԾ�D�  D�@ D�~�D�� D�  D�@ D�~�D־�D�  D�@ D�~�D׾�D���D�>�D�~�Dؾ�D�HD�AHDفHDپ�D�  D�AHDځHD��HD�  D�AHDۀ D�� D�HD�B�D܁HD��HD�HD�>�D�~�D�� D�HD�AHD�~�D�� D��D�@ D߀ D߾�D�  D�@ D�� D�� D�  D�@ D�}qD�� D�  D�@ D₏D��HD�HD�AHD� D�� D�  D�>�D� D��HD�HD�@ D�HD��HD�HD�AHD�~�D�� D�HD�@ D� D��HD�HD�@ D� D�� D�  D�>�D�~�D��HD��D�@ D�HD꾸D���D�@ D�~�D뾸D�  D�AHD�HD��HD�HD�AHD�HD���D���D�>�D�HD�� D���D�AHD�HD��HD�  D�>�D�� D�� D�  D�@ D� D�� D�HD�AHD�HD�� D��D�AHD�HD�� D���D�>�D� D�� D���D�>�D�~�D���D��qD�>�D�~�D���D���D�@ D��HD��HD�HD�@ D�� D���D�HD�@ D�~�D��HD���?��?W
=?k�?��R?�Q�?�
=?��H@��@��@.{@@  @Q�@aG�@p��@��\@�=q@��@�(�@�ff@��@��@��H@��
@�=q@У�@ٙ�@�\@�=q@��@���A�AAQ�A��A�\A
=A=qA   A%A+�A0��A5A;�A@��AEAJ=qAN�RAU�AY��A^{Ab�\AhQ�Al��Ap  Atz�Ay��A~{A�Q�A��\A���A��RA�Q�A�=qA�z�A�{A��A��A�(�A�{A�  A�=qA���A�
=A�G�A��
A�ffA���A��\A��A��A�=qA�(�A�ffA���A�33A��A�\)A��A��
A�A�Q�Aʏ\A�z�A�ffA���A�33A��A�\)Aٙ�A�(�A�{A�Q�A��HA�p�A�A�G�A�(�A�RA���A�33A�p�A�Q�A��\A�z�A�\)B ��BB
=BQ�Bp�BffB\)B��B	�B
�HB�
B��B=qB33B  BG�B�\B\)BQ�Bp�B�\B�Bz�Bp�B�RB�
B��B{B33B Q�B!G�B"�\B$  B$��B&{B'\)B(��B*{B+
=B,z�B-B/
=B0(�B1p�B2�RB4(�B5G�B6=qB7�B8��B:=qB;33B<Q�B=B?
=B@(�BAG�BBffBC�
BE�BF=qBG\)BH��BI�BK33BLQ�BMp�BN�RBP  BQG�BRffBS�BT��BV=qBW\)BXz�BYB[
=B\z�B]B^�HB`(�Bap�Bb�HBd(�Be�BfffBg�
Bi�Bj=qBk\)Blz�Bn{Bo\)BpQ�Bq��Bs
=BtQ�Bu��Bv�RBx  Byp�Bz�RB|  B}G�B~�\B�
B��RB�\)B��B���B�p�B�(�B���B�p�B�(�B��HB���B�=qB���B�p�B�(�B���B�\)B��B�z�B��B��B�{B�z�B���B�\)B���B�B�  B�Q�B�ffB�z�B��RB���B��HB�
=B�
=B��B�G�B�p�B��B��B��B��B��
B��B�  B�{B�  B�(�B�=qB�=qB�=qB�=qB�Q�B�ffB�ffB�ffB�Q�B�ffB�z�B�z�B�ffB�Q�B�ffB�z�B�ffB�Q�B�=qB�=qB�Q�B�=qB�(�B�  B�  B�  B�  B��B�B��B�B��B��B�p�B�\)B�G�B�G�B�33B�
=B���B��HB��HB���B��\B��\B�z�B�z�B�=qB�(�B�{B�(�B�{B��B�B�B��B���B�p�B�\)B�p�B�p�B�G�B�33B��B�33B�33B��B�
=B�
=B�
=B��B��B�
=B�
=B��B�33B�33B�33B�33B�\)B�p�B��B��B��B��B��
B�  B�{B�(�B�=qB�ffB���B��HB���B�33B�\)B��B�B�{B�Q�B��\B��RB��HB�
=B�\)B��B��
B�  B�(�B�Q�B��\B���B�
=B�G�B�p�B���B�B��B�(�B�ffB���B���B�33B�\)B���B�B�(�B�z�B���B��B�p�B��B�{B�ffB���B�33B��B�  B�z�B��HB�\)B��B�{B�ffB��HB�G�B�B�(�B��\B���B�\)B�B�{B�z�B���B�G�B��B�(�B�z�B��HB�33B���B��B�=qB���B���B�\)B��B�(�B��\B���B�\)B�B�(�B��\B���B�\)B�B�=qB��RB�33B���B�{B��\B�
=B�p�B��
B�Q�B���B�G�B�B�=qB��RB�33B�B�Q�B��HB�\)B��B�ffB���B�p�B�  B�z�B���B��B�  B�z�B��HB�p�B�  B�z�B�
=B���B�{B���B�33B��B�(�B���B�33B��B�(�B£�B��BîB�(�Bģ�B��Bř�B�{Bƣ�B��BǙ�B�{Bȏ\B�
=BɅB�{Bʏ\B�
=B˙�B�{B̏\B��B͙�B�=qBθRB�G�B�B�Q�B��HB�p�B�  Bҏ\B��BӮB�=qB���B�\)B��B�z�B�
=Bי�B�(�BظRB�G�B��B�ffB���Bۙ�B�(�BܸRB�\)B��Bޏ\B��B߮B�=qB��HB�p�B�  B�\B�33B�B�ffB���B噚B�=qB���B�B�(�B���B�p�B�{B�RB�\)B�  B��B�G�B��B�\B�G�B��B��\B�G�B��
B�\B�33B��B�z�B�33B�B�Q�B���B���B�=qB��HB��B�(�B���B�p�B�{B���B��B�  B��RB�\)B��C =qC �\C �
C(�Cz�C��C�Cp�CC{Cp�CC{CffC�RC  CQ�C��C��CG�C��C��C=qC�\C�HC(�Cz�C�RC	
=C	Q�C	��C	�C
33C
�C
�
C(�Cz�C��C�CffC�RC  CQ�C��C�C33C�C�
C(�Cz�C��C(�Cp�CC
=C\)C��C��CQ�C��C��C=qC�\C�HC33Cz�CC{Cp�CC�Cz�CC{CffC�C
=C\)C�RC
=CQ�C��C�C=qC�\C�HC33C�C�
C(�Cp�CC
=C\)C�C
=C\)C�C��C=qC�C�
C �C \)C �\C C �C!�C!G�C!z�C!�C!C!�HC"  C"�C"=qC"ffC"�\C"�RC"�
C#  C#(�C#G�C#\)C#z�C#��C#��C#��C$�C$G�C$p�C$�\C$�C$�
C$��C%�C%G�C%p�C%�\C%C%�C&{C&=qC&\)C&z�C&��C&��C&�C'{C'G�C'p�C'��C'�RC'�
C'��C(�C(G�C(z�C(��C(��C(�C)�C)=qC)\)C)�\C)�RC)�HC*{C*G�C*ffC*�C*�C*�
C+
=C+=qC+ffC+�\C+�C+�
C,  C,(�C,\)C,�C,�RC,�HC-  C-(�C-Q�C-�C-�RC-�HC.
=C.(�C.Q�C.z�C.�C.�HC/
=C/=qC/\)C/�C/�C/�
C0
=C033C0ffC0�\C0C0��C1�C1G�C1p�C1��C1��C2  C233C2ffC2��C2��C2��C3�C3G�C3z�C3�C3�
C4{C4G�C4z�C4��C4��C5  C533C5p�C5��C5�
C6
=C6=qC6ffC6�\C6��C7
=C7=qC7z�C7��C7�
C8
=C8G�C8�C8C8��C933C9ffC9��C9��C:  C:G�C:�C:C;  C;33C;ffC;��C;�
C<{C<Q�C<�\C<��C=  C==qC=z�C=�C=�HC>{C>\)C>�\C>�
C?{C?\)C?��C?C@  C@=qC@z�C@�RCA  CA=qCAz�CA�RCA��CB33CBp�CB�CB��CC=qCCz�CC�RCC�CD(�CDffCD�CD�CE33CEz�CE�RCE�CF(�CFffCF��CF�CG33CGp�CG�CG�CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                            ?�  ?��H@@  @�  @�  @��R@�  A   A\)A\)A+�A@  A`��A�  A��A��A�Q�A�  AϮA�  A�A��B  B(�B  B   B((�B/�
B7�B?�
BH  BP  BX  B_�
Bg�Bo�
Bw�
B�  B�{B�  B�{B�Q�B�
=B��
B��B�  B�{B�  B�  B��B��B�  B�  B�  B�  B�  B�  B��
B��B�{B�{B�  B��
B��
B�  B�{B�  B��B��B��C��C��C��C��C
  C  C�C��C  C�C��C��C��C��C
=C �C"  C#��C&  C(  C*
=C,
=C.  C0  C2
=C4
=C6  C7��C:  C;��C=��C@
=CB
=CD  CF  CH
=CJ
=CL  CN  CP
=CR
=CT{CV{CX  CZ  C\
=C]��C_��Cb  Cc��Ce��Cg��Cj  Cl
=Cn  Cp  Cr  Ct  Cu��Cw��Cy��C|  C~  C�  C�C�C�C�  C���C�  C�  C�C�  C�  C�C�C�  C�  C�  C�  C�C�  C�  C���C�  C�  C���C���C���C�  C�C�  C���C���C�  C�  C���C�  C�  C���C�  C�
=C�C�C�C�C�C�C�  C�  C�  C�  C�  C�  C�  C���C�C�
=C�C�  C�  C���C�  C�  C���C���C���C���C�  C�  C���C���C���C���C�  C�C�
=C�C�  C�  C���C�  C�  C�  C�  C�C�  C���C�  C�C�  C�  C�  C�  C�
=C�
=C�  C���C�  C�
=C�
=C�C�C�C�C���C���C���C�  C�
=C�C���C���C���C�  C�  C�C�C�C�  C�  C���C���C�  C�
=C�C�C�C�  C�  C���D   D }qD  D��D�qDz�D�qD� D�D�D�D}qD  D��D�qD}qD  D� D�qD	� D
  D
� D
�qD� D�D� D�qDz�D��D� DD�DD��D  D}qD�qD}qD�qD}qD�qD� D  D� D�D��D�D��DD� D�qD}qD  D��D�D�D  D}qD  D��DD��D�qD� D �D ��D!�D!� D!�qD"}qD#  D#� D$  D$�D%�D%��D&  D&� D&�qD'� D(  D(� D)  D)z�D)�qD*}qD*�qD+� D+��D,z�D-�D-��D.�D.��D.�qD/� D0  D0� D1�D1� D2  D2��D3  D3� D4�D4� D4�qD5� D6�D6� D6�qD7��D8D8� D8�qD9� D:�D:� D:�qD;}qD<  D<��D=  D=��D>D>��D?  D?� D@�D@}qD@��DAz�DB  DB�DC�DC� DD  DD}qDD��DE� DF  DF}qDG  DG}qDH  DH�DI�DI��DI�qDJ��DK  DK}qDL�DL� DL�qDMz�DN  DN�DO�DO}qDO�qDP� DQ�DQ� DR  DR� DS  DS� DS�qDT}qDU  DU��DV  DV� DW  DW}qDW�qDX}qDY  DY��DZ�DZ��D[  D[}qD\  D\��D]  D]}qD]�qD^� D_  D_��D`D`� D`�qDa� Db  Db��Dc�Dc��Dd  Dd}qDd�qDe� Df  Df��Dg�Dg� Dg�qDh}qDi  Di��Dj  Dj}qDk  Dk� Dl  Dl}qDm  Dm� Dm�qDn}qDn�qDo}qDp  Dp� Dp�qDq}qDr  Dr��Ds  Ds� Dt�Dt��Du  Du}qDv  Dv� Dv�qDw��Dx  Dx� Dy  Dy� Dz  Dz}qDz�qD{��D|D|� D|�qD}� D~�D~� D  D}qD�  D�AHD��HD���D���D�AHD�~�D�� D�HD�AHD��HD�� D�  D�@ D�~�D���D���D�@ D��HD�D�HD�@ D��HD�� D���D�@ D��HD�� D���D�@ D�~�D�� D�HD�@ D�� D�� D�HD�@ D�~�D���D���D�>�D�}qD�� D�  D�@ D�� D�� D�HD�AHD�~�D�� D�  D�>�D�~�D���D��qD�>�D�� D��HD�  D�@ D�� D���D���D�AHD�� D���D�  D�AHD�� D�� D���D�AHD��HD��qD���D�@ D�� D��HD�HD�@ D�� D���D���D�>�D�~�D��HD��D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D��qD���D�AHD���D��HD�HD�B�D��HD��HD�  D�@ D�~�D���D�HD�B�D�� D���D���D�@ D�� D��HD�  D�>�D�~�D�� D�HD�@ D��HD�D�HD�@ D�~�D��HD�  D�>�D��HD�� D�  D�AHD��HD��HD�HD�AHD�� D��qD���D�@ D�~�D��qD���D�@ D�� D�� D���D�>�D�� D�� D�  D�B�D��HD�� D���D�@ D�� D���D��qD�>�D��HD�� D�  D�>�D�~�D���D���D�=qD�� D�� D�HD�AHD�~�D�� D�  D�>�D�~�D���D���D�>�D�~�D�� D�HD�@ D�~�D��HD�HD�B�D���D���D���D�>�D�~�D���D���D�>�D�� D�� D�  D�=qD�~�D�� D���D�=qD�~�D��HD��D�AHD�� D�� D���D�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D�� D�� D�  D�>�D�~�D���D���D�@ D�� D�� D�  D�@ D�~�D�� D�  D�AHD�� D�� D�HD�@ D��HD��HD�HD�AHD��HD��HD���D�>�D�� D�� D���D�@ D��HD�� D���D�@ D��HD��HD�  D�AHD�~�D�� D�  D�>�DÀ D�� D���D�>�DĀ D�� D���D�>�DŁHD�D�HD�@ DƁHD�D�HD�@ Dǀ D�� D���D�>�D�~�D�� D�HD�B�DɁHD��HD�HD�@ Dʀ Dʾ�D��qD�=qDˀ D˾�D�  D�@ D�~�D̾�D���D�>�D́HD��HD�HD�AHD�~�D�� D�  D�AHDρHD��HD���D�>�DЀ D�� D�  D�@ D�~�DѾ�D�  D�AHDҁHD�� D���D�@ DӀ D��HD�HD�AHDԀ DԾ�D�  D�@ D�~�D�� D�  D�@ D�~�D־�D�  D�@ D�~�D׾�D���D�>�D�~�Dؾ�D�HD�AHDفHDپ�D�  D�AHDځHD��HD�  D�AHDۀ D�� D�HD�B�D܁HD��HD�HD�>�D�~�D�� D�HD�AHD�~�D�� D��D�@ D߀ D߾�D�  D�@ D�� D�� D�  D�@ D�}qD�� D�  D�@ D₏D��HD�HD�AHD� D�� D�  D�>�D� D��HD�HD�@ D�HD��HD�HD�AHD�~�D�� D�HD�@ D� D��HD�HD�@ D� D�� D�  D�>�D�~�D��HD��D�@ D�HD꾸D���D�@ D�~�D뾸D�  D�AHD�HD��HD�HD�AHD�HD���D���D�>�D�HD�� D���D�AHD�HD��HD�  D�>�D�� D�� D�  D�@ D� D�� D�HD�AHD�HD�� D��D�AHD�HD�� D���D�>�D� D�� D���D�>�D�~�D���D��qD�>�D�~�D���D���D�@ D��HD��HD�HD�@ D�� D���D�HD�@ D�~�D��HG�O�?��?W
=?k�?��R?�Q�?�
=?��H@��@��@.{@@  @Q�@aG�@p��@��\@�=q@��@�(�@�ff@��@��@��H@��
@�=q@У�@ٙ�@�\@�=q@��@���A�AAQ�A��A�\A
=A=qA   A%A+�A0��A5A;�A@��AEAJ=qAN�RAU�AY��A^{Ab�\AhQ�Al��Ap  Atz�Ay��A~{A�Q�A��\A���A��RA�Q�A�=qA�z�A�{A��A��A�(�A�{A�  A�=qA���A�
=A�G�A��
A�ffA���A��\A��A��A�=qA�(�A�ffA���A�33A��A�\)A��A��
A�A�Q�Aʏ\A�z�A�ffA���A�33A��A�\)Aٙ�A�(�A�{A�Q�A��HA�p�A�A�G�A�(�A�RA���A�33A�p�A�Q�A��\A�z�A�\)B ��BB
=BQ�Bp�BffB\)B��B	�B
�HB�
B��B=qB33B  BG�B�\B\)BQ�Bp�B�\B�Bz�Bp�B�RB�
B��B{B33B Q�B!G�B"�\B$  B$��B&{B'\)B(��B*{B+
=B,z�B-B/
=B0(�B1p�B2�RB4(�B5G�B6=qB7�B8��B:=qB;33B<Q�B=B?
=B@(�BAG�BBffBC�
BE�BF=qBG\)BH��BI�BK33BLQ�BMp�BN�RBP  BQG�BRffBS�BT��BV=qBW\)BXz�BYB[
=B\z�B]B^�HB`(�Bap�Bb�HBd(�Be�BfffBg�
Bi�Bj=qBk\)Blz�Bn{Bo\)BpQ�Bq��Bs
=BtQ�Bu��Bv�RBx  Byp�Bz�RB|  B}G�B~�\B�
B��RB�\)B��B���B�p�B�(�B���B�p�B�(�B��HB���B�=qB���B�p�B�(�B���B�\)B��B�z�B��B��B�{B�z�B���B�\)B���B�B�  B�Q�B�ffB�z�B��RB���B��HB�
=B�
=B��B�G�B�p�B��B��B��B��B��
B��B�  B�{B�  B�(�B�=qB�=qB�=qB�=qB�Q�B�ffB�ffB�ffB�Q�B�ffB�z�B�z�B�ffB�Q�B�ffB�z�B�ffB�Q�B�=qB�=qB�Q�B�=qB�(�B�  B�  B�  B�  B��B�B��B�B��B��B�p�B�\)B�G�B�G�B�33B�
=B���B��HB��HB���B��\B��\B�z�B�z�B�=qB�(�B�{B�(�B�{B��B�B�B��B���B�p�B�\)B�p�B�p�B�G�B�33B��B�33B�33B��B�
=B�
=B�
=B��B��B�
=B�
=B��B�33B�33B�33B�33B�\)B�p�B��B��B��B��B��
B�  B�{B�(�B�=qB�ffB���B��HB���B�33B�\)B��B�B�{B�Q�B��\B��RB��HB�
=B�\)B��B��
B�  B�(�B�Q�B��\B���B�
=B�G�B�p�B���B�B��B�(�B�ffB���B���B�33B�\)B���B�B�(�B�z�B���B��B�p�B��B�{B�ffB���B�33B��B�  B�z�B��HB�\)B��B�{B�ffB��HB�G�B�B�(�B��\B���B�\)B�B�{B�z�B���B�G�B��B�(�B�z�B��HB�33B���B��B�=qB���B���B�\)B��B�(�B��\B���B�\)B�B�(�B��\B���B�\)B�B�=qB��RB�33B���B�{B��\B�
=B�p�B��
B�Q�B���B�G�B�B�=qB��RB�33B�B�Q�B��HB�\)B��B�ffB���B�p�B�  B�z�B���B��B�  B�z�B��HB�p�B�  B�z�B�
=B���B�{B���B�33B��B�(�B���B�33B��B�(�B£�B��BîB�(�Bģ�B��Bř�B�{Bƣ�B��BǙ�B�{Bȏ\B�
=BɅB�{Bʏ\B�
=B˙�B�{B̏\B��B͙�B�=qBθRB�G�B�B�Q�B��HB�p�B�  Bҏ\B��BӮB�=qB���B�\)B��B�z�B�
=Bי�B�(�BظRB�G�B��B�ffB���Bۙ�B�(�BܸRB�\)B��Bޏ\B��B߮B�=qB��HB�p�B�  B�\B�33B�B�ffB���B噚B�=qB���B�B�(�B���B�p�B�{B�RB�\)B�  B��B�G�B��B�\B�G�B��B��\B�G�B��
B�\B�33B��B�z�B�33B�B�Q�B���B���B�=qB��HB��B�(�B���B�p�B�{B���B��B�  B��RB�\)B��C =qC �\C �
C(�Cz�C��C�Cp�CC{Cp�CC{CffC�RC  CQ�C��C��CG�C��C��C=qC�\C�HC(�Cz�C�RC	
=C	Q�C	��C	�C
33C
�C
�
C(�Cz�C��C�CffC�RC  CQ�C��C�C33C�C�
C(�Cz�C��C(�Cp�CC
=C\)C��C��CQ�C��C��C=qC�\C�HC33Cz�CC{Cp�CC�Cz�CC{CffC�C
=C\)C�RC
=CQ�C��C�C=qC�\C�HC33C�C�
C(�Cp�CC
=C\)C�C
=C\)C�C��C=qC�C�
C �C \)C �\C C �C!�C!G�C!z�C!�C!C!�HC"  C"�C"=qC"ffC"�\C"�RC"�
C#  C#(�C#G�C#\)C#z�C#��C#��C#��C$�C$G�C$p�C$�\C$�C$�
C$��C%�C%G�C%p�C%�\C%C%�C&{C&=qC&\)C&z�C&��C&��C&�C'{C'G�C'p�C'��C'�RC'�
C'��C(�C(G�C(z�C(��C(��C(�C)�C)=qC)\)C)�\C)�RC)�HC*{C*G�C*ffC*�C*�C*�
C+
=C+=qC+ffC+�\C+�C+�
C,  C,(�C,\)C,�C,�RC,�HC-  C-(�C-Q�C-�C-�RC-�HC.
=C.(�C.Q�C.z�C.�C.�HC/
=C/=qC/\)C/�C/�C/�
C0
=C033C0ffC0�\C0C0��C1�C1G�C1p�C1��C1��C2  C233C2ffC2��C2��C2��C3�C3G�C3z�C3�C3�
C4{C4G�C4z�C4��C4��C5  C533C5p�C5��C5�
C6
=C6=qC6ffC6�\C6��C7
=C7=qC7z�C7��C7�
C8
=C8G�C8�C8C8��C933C9ffC9��C9��C:  C:G�C:�C:C;  C;33C;ffC;��C;�
C<{C<Q�C<�\C<��C=  C==qC=z�C=�C=�HC>{C>\)C>�\C>�
C?{C?\)C?��C?C@  C@=qC@z�C@�RCA  CA=qCAz�CA�RCA��CB33CBp�CB�CB��CC=qCCz�CC�RCC�CD(�CDffCD�CD�CE33CEz�CE�RCE�CF(�CFffCF��CF�CG33CGp�CG�CG�CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                            @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�-A�/A�5?A�9XA�?}A�?}A�;dA�;dA�;dA�=qA�E�A�E�A�G�A�G�A�C�A�I�A�G�A�I�A�K�A�K�A�S�A�S�A�XA�\)A�ZA�ZA�ZA�ZA�\)A�^5A�^5A�`BA�`BA�bNA�bNA�bNA�dZA�ffA�^5A�;dA�/A�v�A��A�?}A��TA��A�=qA���A��mA�5?A���A���A��jA��9A�
=A���A�I�A�n�A�jA�1A�VA���A�K�A���A�G�A�VA��A��A��A�XA���AzE�Avn�Atr�Ap �Aj�jAhz�Acp�A_dZA\�HAR�APn�AP�ANbNAI�ACXAA�hA@  A;��A:5?A8=qA6�A69XA5�wA4��A4A2ȴA1p�A1%A0bNA/�#A.��A,�A+hsA++A+��A+ƨA+�#A)|�A)O�A(�jA)�^A'�A%\)A%�A%��A$ �A#��A#��A#�PA#C�A"��A"�A"�yA"��A"��A!ƨA!dZA �HA �`A �A M�A (�A �A�;A�A|�Ar�AZAI�A�A�A�AffA��AO�A~�Ax�A�jAjA�^A��A�jA�DA=qA1A��A�A�Ap�A�A�AĜA~�A(�AƨAXAVA�+AZA�At�A�A�A�!A�\A~�AbNA9XAƨAS�A�yA��An�AQ�A{A�7AhsAXA��AM�A�A��A�TA��AdZA
��A
{A	?}A	A�HA�\AZA-A��AO�A"�A�AoA��A�`AVAA�HA��A=qA{A�A��A��AG�A%A�\AQ�A(�A�Ax�AA ��A =qA -@���@�
=@�n�@���@��/@���@��u@�Z@��@���@�V@�9X@�C�@�o@��H@���@���@�hs@�Ĝ@�@�Q�@� �@���@�ƨ@�
=@�p�@�Z@�@�"�@�-@�7L@�Ĝ@�A�@�ƨ@�\)@�
=@���@��@�v�@�{@�X@�V@�9@�u@�D@� �@畁@旍@�`B@�z�@��@�\@ᙚ@�p�@���@�ƨ@���@ޏ\@�$�@ݙ�@���@��@���@�5?@�@���@���@�b@�S�@��H@�V@���@���@�j@�1@ӕ�@�;d@ҏ\@�5?@�p�@У�@�(�@Ͼw@��@��@�ȴ@Ο�@�ff@�J@Ͳ-@�?}@�%@̓u@�A�@�^5@�p�@��@�bN@�b@���@ǍP@�
=@ư!@�=q@�@�7L@���@��@���@�Q�@Ý�@�K�@��@���@\@�-@��T@���@���@�p�@�7L@��@���@���@�I�@�1@��P@�+@��@��y@��@��7@�X@��@���@�I�@�ƨ@�C�@���@���@�@���@��@�X@�7L@��@�%@���@��/@��@�z�@�Z@�1@��y@��+@�E�@���@��^@���@���@��@�p�@�V@��@�dZ@�"�@�@��y@��!@�5?@���@��-@�hs@�O�@���@�I�@��@��F@�\)@�~�@��@���@��@�(�@��
@��@�l�@�33@��@�~�@���@�`B@�7L@��`@��u@��@�S�@�v�@���@��h@�`B@�O�@�?}@���@���@�r�@���@���@�l�@�C�@�+@��H@���@���@��+@�ff@�=q@��7@��`@�z�@�A�@��
@�|�@�33@���@�V@�5?@���@��-@�`B@�V@���@�Q�@���@��m@��w@��@���@�E�@��@�x�@�V@���@�r�@�j@�I�@�(�@���@��
@�dZ@�"�@�
=@���@�V@�E�@�-@�J@��@�x�@�/@���@�Ĝ@���@�z�@�A�@�b@�t�@�33@��@��y@�ȴ@���@�ff@�{@��#@�@��h@�X@�O�@�7L@��`@��@�9X@��;@���@�"�@��y@��+@��@��h@�p�@�/@���@�I�@���@���@��@�C�@�o@�@��y@��!@�v�@�^5@�@��@�7L@�%@���@�bN@��@��
@�ƨ@��@��@�l�@�S�@�K�@�C�@�+@��y@���@�@��T@��#@�@���@��@�`B@�X@�?}@�V@���@��@�A�@��@���@�l�@�@���@��\@�ff@�V@�=q@�J@�@���@�`B@�?}@��@��@�z�@�1@��@K�@~�@~�+@~5?@~{@}�T@}�-@}`B@}V@|��@|�@|j@{��@z��@z^5@z=q@zJ@y�7@x��@xA�@w�;@w|�@v�+@u�@u��@u?}@t�D@s��@r�@r�@q�7@pĜ@pbN@p �@p  @o��@nȴ@nv�@m��@m/@l��@l�@k��@k�F@k�@kt�@ko@j~�@jM�@jJ@i7L@h��@hA�@hb@g��@g+@f�@fv�@f$�@e�-@e/@dj@c�m@ct�@c"�@b�@bn�@b-@a�#@ahs@aX@a�@a%@`Ĝ@`r�@`1'@_�@_�P@^��@]�@]@]��@]�h@]`B@\��@\�@\Z@\1@[�m@[��@[S�@[33@[@Z��@Z^5@Z-@Y�^@YX@Y%@X�@XQ�@X  @W�P@W\)@V�y@V�+@Vff@VV@V5?@V{@V{@V{@V@U��@T�@T�@TZ@S��@SC�@So@S@R�@R�@R��@Rn�@Q�#@Q�7@P�`@Pr�@Pb@O�@O+@N�R@M�-@MV@L��@L�D@Lz�@L�@K�F@KS�@Jn�@J�@I�@I��@Ihs@IX@I7L@I�@H��@HbN@H �@G�w@GK�@G
=@F�R@Fv�@F$�@E�-@Ep�@EO�@E?}@E/@E�@D�/@D�@D1@Co@B�\@B=q@BJ@A�#@A��@A&�@@��@@Ĝ@@Ĝ@@�@@bN@@  @?��@?�@?|�@?
=@>��@>ff@>$�@=��@=`B@<z�@<9X@;��@;��@;t�@;C�@:�@:��@:M�@9�#@9�^@9��@97L@8 �@7�;@7��@7\)@7�@6�y@6��@6�+@6v�@6V@6V@5��@5O�@4��@4(�@4�@3��@3ƨ@3��@3t�@3dZ@3o@2��@2^5@1�@1hs@1X@1G�@17L@1%@0��@0b@/l�@.�@.�+@.ff@.$�@-��@-`B@,��@,�@+�@+C�@+33@+@*�@*��@*n�@*J@)��@)x�@)x�@)hs@)7L@)&�@(�`@(bN@'�w@'�P@'+@&��@&�@&ȴ@&�R@&�R@&�R@&��@&v�@&V@&$�@%@%�@$�@$�j@$Z@$�@#��@#C�@"��@"=q@!�#@!�7@!x�@!hs@!X@!X@!X@!7L@!7L@!&�@!�@ Ĝ@ �u@ b@�@��@l�@�y@�+@V@5?@@/@��@�/@I�@��@�m@��@dZ@S�@C�@S�@C�@C�@@��@n�@��@��@x�@&�@��@Ĝ@�u@Q�@b@�@�;@�w@�P@|�@|�@l�@\)@\)@\)@K�@K�@��@�y@ȴ@v�@V@E�@E�@E�@$�@{@��@O�@?}@V@�@�/@�j@��@I�@1@ƨ@�F@��@t�@t�@C�@33@33@"�@o@�@��@n�@n�@^5@M�@-@�@��@��@7L@��@��@��@Q�@�@�w@��@�P@K�@+@+@�@��@�+@V@{@�@@�h@�h@`B@/@��@��@I�@9X@�@��@�m@�
@�F@��@�@t�@S�@S�@S�@S�@C�A�(�A�/A�-A�+A�-A�-A�/A�-A�7LA�9XA�5?A�1'A�7LA�5?A�?}A�=qA�=qA�A�A�A�A�=qA�;dA�A�A�C�A�=qA�9XA�?}A�=qA�9XA�;dA�=qA�9XA�9XA�=qA�?}A�;dA�5?A�5?A�?}A�E�A�E�A�G�A�A�A�E�A�G�A�E�A�E�A�I�A�K�A�C�A�C�A�I�A�K�A�I�A�E�A�K�A�K�A�C�A�E�A�I�A�G�A�C�A�C�A�E�A�C�A�?}A�C�A�G�A�E�A�A�A�K�A�K�A�K�A�E�A�I�A�I�A�I�A�E�A�E�A�G�A�G�A�G�A�G�A�I�A�K�A�I�A�G�A�I�A�G�A�E�A�I�A�M�A�K�A�I�A�K�A�O�A�M�A�I�A�I�A�K�A�K�A�G�A�I�A�M�A�K�A�I�A�M�A�M�A�VA�Q�A�XA�ZA�\)A�Q�A�S�A�S�A�O�A�O�A�VA�\)A�XA�O�A�Q�A�\)A�`BA�\)A�\)A�`BA�`BA�ZA�\)A�^5A�\)A�XA�XA�\)A�\)A�ZA�VA�ZA�\)A�VA�XA�\)A�ZA�XA�ZA�\)A�ZA�XA�ZA�\)A�\)A�ZA�XA�\)A�^5A�ZA�XA�ZA�^5A�\)A�ZA�XA�^5A�^5A�ZA�ZA�\)A�^5A�`BA�^5A�\)A�\)A�`BA�^5A�\)A�\)A�^5A�`BA�`BA�^5A�^5A�`BA�dZA�bNA�^5A�`BA�bNA�bNA�^5A�`BA�bNA�dZA�dZA�`BA�^5A�bNA�dZA�bNA�bNA�^5A�bNA�dZA�dZA�`BA�`BA�dZA�ffA�bNA�bNA�bNA�bNA�bNA�bNA�`BA�ffA�ffA�bNA�bNA�bNA�dZA�hsA�hsA�dZA�dZA�hsA�dZA�\)A�S�A�VA�O�A�VA�7LA�$�A�(�A�7LA�9XA��A�$�A�$�A�9XA�5?A�;dA�1'A��A��A���AғuA�bNAѣ�A�`BA�O�A�VA�hsA�^5A�Q�A�Q�A�VA�ZA�`BA�dZA�jA�jA�jA�t�AѰ!AѮAѾwA���A��A���A��A�JA�{A��A�$�A�+A�-A�-A�5?A�A�A�C�A�E�A�E�A�I�A�XA�bNA�ffA�bNA�`BA�ffA�hsA�hsA�dZA�hsA�n�A�t�A�v�A�t�A�n�A�x�AҁA�~�A�|�A�|�A�z�A�jA�`BA�I�A�A�A�A�A�A�A�?}A�;dA�1'A�$�A�AѺ^A�ffA�=qA� �A�{A�1A�A��HA��#A�ĜAв-AЬAЛ�A�v�A�l�A�9XA�;dA�A�ȴAϗ�A�z�A�bNA�S�A�1'A�$�A�A���A���A���A���A��A�ĜA�ƨAζFAΰ!AΕ�A΋DA�jA�S�A�9XA�33A��A�^5A�E�A�/A��A���A�ĜA�v�A�?}A�bA��mA˕�A��#AʑhA���Aɛ�A� �A�7LA���AǓuA�bNA�C�A�5?A�(�A�
=A��A��/A��
A�ȴAƺ^AƬAƝ�AƉ7A�\)A�7LA��A���A��/AőhAŝ�A�I�A�^5A�ffA� �A��;A��A�^5A��A��FA��-A�-A�A�~�A�M�A��A�\)A�ZA��A�S�A���A���A���A��DA�\)A�+A�A��#A���A�7LA�/A�&�A��A���A��A��HA��
A���A���A��9A�G�A���A��9A���A�l�A��mA�A�A�&�A�%A��7A��A��#A���A�5?A��A���A�^5A��A���A�\)A�K�A�
=A��
A��uA���A���A��`A�ƨA���A��PA�x�A�jA�VA�A�A�bA��TA��!A�jA�C�A�9XA�5?A�1'A�-A�
=A��A�z�A�ZA�G�A�9XA�/A�bA��A��-A�n�A�E�A�+A��A��;A�A���A�+A��-A� �A��RA�jA�+A�bA�  A���A��DA�\)A�K�A�+A���A��uA�x�A�G�A��A���A���A�G�A�{A�  A��
A�G�A���A�dZA��RA�x�A�O�A�Q�A��-A���A���A���A��A��+A�hsA�/A��
A�XA���A��A�dZA� �A��A�ĜA��-A��!A��uA�-A�1'A�C�A�hsA�I�A��FA�I�A��A���A�=qA�  A�  A�`BA�K�A���A�XA�bA���A�;dA�  A�ƨA�M�A��A��A�{A��jA��+A�ZA�+A�A��mA���A��!A��7A�ffA�M�A�33A�(�A� �A��A�oA�JA�A���A��A��mA��A�ƨA��-A���A��\A�z�A�ZA�?}A�$�A�  A��A��^A�hsA�"�A�ȴA�VA�
A|��Az9XAyVAx�Aw��Aw��AwdZAv��Av��Av9XAu\)At�`At�At�Atn�AtffAtbNAtI�As�As�Aq|�ApjAop�An$�AlZAk�
Ak�AkAj��AjZAi�-Ai�Ah��Ah��AhjAhQ�Ah5?Ah �Ag��Af{Ac;dAa33AaA`��A`�!A`jA_�A_\)A^��A^n�A^1'A]�A]��A]�wA]l�A\M�AXM�AUK�AR��AQXAP��AP��AP�9AP��AP�uAPv�AP^5APM�API�AP=qAP1'AP$�AP�APbAPAO�#AO��AOl�ANȴANbAM�7AL��ALv�AK�FAJĜAJ�AIO�AGp�AE��ADz�AD(�AC�^AC`BAC%AB�AB^5AB$�AB  AA�TAA��AA�^AA��AA��AA�7AA|�AAt�AAp�AAhsAAK�AA+AAoA@�yA@��A@��A@�+A@r�A@bNA@I�A@-A@1A?��A?�A?
=A>I�A=�A=��A=�PA<��A<M�A;��A;G�A:�/A:��A:��A:�\A:�DA:�DA:�+A:�+A:�+A:�+A:�DA:�A:~�A:v�A:VA: �A:  A9�TA9�FA9;dA9/A9
=A8�yA8��A8��A8�\A8E�A7��A7�;A7�A7dZA7;dA7VA7�A7VA6��A6��A6��A6�HA6�HA6�HA6�/A6�/A6�A6ȴA6��A6��A6^5A6-A6$�A6$�A6�A6JA6A5��A5�A5�mA5�;A5�
A5��A5ƨA5A5�^A5�FA5�A5��A5��A5��A5��A5�A5`BA4�A4��A4n�A4bNA4bNA4bNA4bNA4^5A4^5A4VA4-A4{A4JA3��A3�TA3�TA3�;A3��A3��A3dZA3C�A3VA2��A2�RA2�uA2n�A2bNA2ZA2$�A1�mA1�-A1�A1/A0��A1&�A1S�A1|�A1x�A133A1%A0��A1A1�A1�A0��A0�yA0ĜA0��A0�+A0n�A0^5A0^5A0ZA0E�A0$�A0�A0JA0A/��A/A/A/�wA/�^A/ƨA/A/|�A/O�A/"�A.��A.�A-�#A-�7A-"�A,��A,��A,��A-A,��A,��A,�DA,Q�A,bA+�#A+��A+t�A+oA*��A*�A*�A*�A*�A+
=A+7LA+?}A+?}A+XA+p�A+�A+�7A+�hA+��A+��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                            A�-A�/A�5?A�9XA�?}A�?}A�;dA�;dA�;dA�=qA�E�A�E�A�G�A�G�A�C�A�I�A�G�A�I�A�K�A�K�A�S�A�S�A�XA�\)A�ZA�ZA�ZA�ZA�\)A�^5A�^5A�`BA�`BA�bNA�bNA�bNA�dZA�ffA�^5A�;dA�/A�v�A��A�?}A��TA��A�=qA���A��mA�5?A���A���A��jA��9A�
=A���A�I�A�n�A�jA�1A�VA���A�K�A���A�G�A�VA��A��A��A�XA���AzE�Avn�Atr�Ap �Aj�jAhz�Acp�A_dZA\�HAR�APn�AP�ANbNAI�ACXAA�hA@  A;��A:5?A8=qA6�A69XA5�wA4��A4A2ȴA1p�A1%A0bNA/�#A.��A,�A+hsA++A+��A+ƨA+�#A)|�A)O�A(�jA)�^A'�A%\)A%�A%��A$ �A#��A#��A#�PA#C�A"��A"�A"�yA"��A"��A!ƨA!dZA �HA �`A �A M�A (�A �A�;A�A|�Ar�AZAI�A�A�A�AffA��AO�A~�Ax�A�jAjA�^A��A�jA�DA=qA1A��A�A�Ap�A�A�AĜA~�A(�AƨAXAVA�+AZA�At�A�A�A�!A�\A~�AbNA9XAƨAS�A�yA��An�AQ�A{A�7AhsAXA��AM�A�A��A�TA��AdZA
��A
{A	?}A	A�HA�\AZA-A��AO�A"�A�AoA��A�`AVAA�HA��A=qA{A�A��A��AG�A%A�\AQ�A(�A�Ax�AA ��A =qA -@���@�
=@�n�@���@��/@���@��u@�Z@��@���@�V@�9X@�C�@�o@��H@���@���@�hs@�Ĝ@�@�Q�@� �@���@�ƨ@�
=@�p�@�Z@�@�"�@�-@�7L@�Ĝ@�A�@�ƨ@�\)@�
=@���@��@�v�@�{@�X@�V@�9@�u@�D@� �@畁@旍@�`B@�z�@��@�\@ᙚ@�p�@���@�ƨ@���@ޏ\@�$�@ݙ�@���@��@���@�5?@�@���@���@�b@�S�@��H@�V@���@���@�j@�1@ӕ�@�;d@ҏ\@�5?@�p�@У�@�(�@Ͼw@��@��@�ȴ@Ο�@�ff@�J@Ͳ-@�?}@�%@̓u@�A�@�^5@�p�@��@�bN@�b@���@ǍP@�
=@ư!@�=q@�@�7L@���@��@���@�Q�@Ý�@�K�@��@���@\@�-@��T@���@���@�p�@�7L@��@���@���@�I�@�1@��P@�+@��@��y@��@��7@�X@��@���@�I�@�ƨ@�C�@���@���@�@���@��@�X@�7L@��@�%@���@��/@��@�z�@�Z@�1@��y@��+@�E�@���@��^@���@���@��@�p�@�V@��@�dZ@�"�@�@��y@��!@�5?@���@��-@�hs@�O�@���@�I�@��@��F@�\)@�~�@��@���@��@�(�@��
@��@�l�@�33@��@�~�@���@�`B@�7L@��`@��u@��@�S�@�v�@���@��h@�`B@�O�@�?}@���@���@�r�@���@���@�l�@�C�@�+@��H@���@���@��+@�ff@�=q@��7@��`@�z�@�A�@��
@�|�@�33@���@�V@�5?@���@��-@�`B@�V@���@�Q�@���@��m@��w@��@���@�E�@��@�x�@�V@���@�r�@�j@�I�@�(�@���@��
@�dZ@�"�@�
=@���@�V@�E�@�-@�J@��@�x�@�/@���@�Ĝ@���@�z�@�A�@�b@�t�@�33@��@��y@�ȴ@���@�ff@�{@��#@�@��h@�X@�O�@�7L@��`@��@�9X@��;@���@�"�@��y@��+@��@��h@�p�@�/@���@�I�@���@���@��@�C�@�o@�@��y@��!@�v�@�^5@�@��@�7L@�%@���@�bN@��@��
@�ƨ@��@��@�l�@�S�@�K�@�C�@�+@��y@���@�@��T@��#@�@���@��@�`B@�X@�?}@�V@���@��@�A�@��@���@�l�@�@���@��\@�ff@�V@�=q@�J@�@���@�`B@�?}@��@��@�z�@�1@��@K�@~�@~�+@~5?@~{@}�T@}�-@}`B@}V@|��@|�@|j@{��@z��@z^5@z=q@zJ@y�7@x��@xA�@w�;@w|�@v�+@u�@u��@u?}@t�D@s��@r�@r�@q�7@pĜ@pbN@p �@p  @o��@nȴ@nv�@m��@m/@l��@l�@k��@k�F@k�@kt�@ko@j~�@jM�@jJ@i7L@h��@hA�@hb@g��@g+@f�@fv�@f$�@e�-@e/@dj@c�m@ct�@c"�@b�@bn�@b-@a�#@ahs@aX@a�@a%@`Ĝ@`r�@`1'@_�@_�P@^��@]�@]@]��@]�h@]`B@\��@\�@\Z@\1@[�m@[��@[S�@[33@[@Z��@Z^5@Z-@Y�^@YX@Y%@X�@XQ�@X  @W�P@W\)@V�y@V�+@Vff@VV@V5?@V{@V{@V{@V@U��@T�@T�@TZ@S��@SC�@So@S@R�@R�@R��@Rn�@Q�#@Q�7@P�`@Pr�@Pb@O�@O+@N�R@M�-@MV@L��@L�D@Lz�@L�@K�F@KS�@Jn�@J�@I�@I��@Ihs@IX@I7L@I�@H��@HbN@H �@G�w@GK�@G
=@F�R@Fv�@F$�@E�-@Ep�@EO�@E?}@E/@E�@D�/@D�@D1@Co@B�\@B=q@BJ@A�#@A��@A&�@@��@@Ĝ@@Ĝ@@�@@bN@@  @?��@?�@?|�@?
=@>��@>ff@>$�@=��@=`B@<z�@<9X@;��@;��@;t�@;C�@:�@:��@:M�@9�#@9�^@9��@97L@8 �@7�;@7��@7\)@7�@6�y@6��@6�+@6v�@6V@6V@5��@5O�@4��@4(�@4�@3��@3ƨ@3��@3t�@3dZ@3o@2��@2^5@1�@1hs@1X@1G�@17L@1%@0��@0b@/l�@.�@.�+@.ff@.$�@-��@-`B@,��@,�@+�@+C�@+33@+@*�@*��@*n�@*J@)��@)x�@)x�@)hs@)7L@)&�@(�`@(bN@'�w@'�P@'+@&��@&�@&ȴ@&�R@&�R@&�R@&��@&v�@&V@&$�@%@%�@$�@$�j@$Z@$�@#��@#C�@"��@"=q@!�#@!�7@!x�@!hs@!X@!X@!X@!7L@!7L@!&�@!�@ Ĝ@ �u@ b@�@��@l�@�y@�+@V@5?@@/@��@�/@I�@��@�m@��@dZ@S�@C�@S�@C�@C�@@��@n�@��@��@x�@&�@��@Ĝ@�u@Q�@b@�@�;@�w@�P@|�@|�@l�@\)@\)@\)@K�@K�@��@�y@ȴ@v�@V@E�@E�@E�@$�@{@��@O�@?}@V@�@�/@�j@��@I�@1@ƨ@�F@��@t�@t�@C�@33@33@"�@o@�@��@n�@n�@^5@M�@-@�@��@��@7L@��@��@��@Q�@�@�w@��@�P@K�@+@+@�@��@�+@V@{@�@@�h@�h@`B@/@��@��@I�@9X@�@��@�m@�
@�F@��@�@t�@S�@S�@S�@S�G�O�A�(�A�/A�-A�+A�-A�-A�/A�-A�7LA�9XA�5?A�1'A�7LA�5?A�?}A�=qA�=qA�A�A�A�A�=qA�;dA�A�A�C�A�=qA�9XA�?}A�=qA�9XA�;dA�=qA�9XA�9XA�=qA�?}A�;dA�5?A�5?A�?}A�E�A�E�A�G�A�A�A�E�A�G�A�E�A�E�A�I�A�K�A�C�A�C�A�I�A�K�A�I�A�E�A�K�A�K�A�C�A�E�A�I�A�G�A�C�A�C�A�E�A�C�A�?}A�C�A�G�A�E�A�A�A�K�A�K�A�K�A�E�A�I�A�I�A�I�A�E�A�E�A�G�A�G�A�G�A�G�A�I�A�K�A�I�A�G�A�I�A�G�A�E�A�I�A�M�A�K�A�I�A�K�A�O�A�M�A�I�A�I�A�K�A�K�A�G�A�I�A�M�A�K�A�I�A�M�A�M�A�VA�Q�A�XA�ZA�\)A�Q�A�S�A�S�A�O�A�O�A�VA�\)A�XA�O�A�Q�A�\)A�`BA�\)A�\)A�`BA�`BA�ZA�\)A�^5A�\)A�XA�XA�\)A�\)A�ZA�VA�ZA�\)A�VA�XA�\)A�ZA�XA�ZA�\)A�ZA�XA�ZA�\)A�\)A�ZA�XA�\)A�^5A�ZA�XA�ZA�^5A�\)A�ZA�XA�^5A�^5A�ZA�ZA�\)A�^5A�`BA�^5A�\)A�\)A�`BA�^5A�\)A�\)A�^5A�`BA�`BA�^5A�^5A�`BA�dZA�bNA�^5A�`BA�bNA�bNA�^5A�`BA�bNA�dZA�dZA�`BA�^5A�bNA�dZA�bNA�bNA�^5A�bNA�dZA�dZA�`BA�`BA�dZA�ffA�bNA�bNA�bNA�bNA�bNA�bNA�`BA�ffA�ffA�bNA�bNA�bNA�dZA�hsA�hsA�dZA�dZA�hsA�dZA�\)A�S�A�VA�O�A�VA�7LA�$�A�(�A�7LA�9XA��A�$�A�$�A�9XA�5?A�;dA�1'A��A��A���AғuA�bNAѣ�A�`BA�O�A�VA�hsA�^5A�Q�A�Q�A�VA�ZA�`BA�dZA�jA�jA�jA�t�AѰ!AѮAѾwA���A��A���A��A�JA�{A��A�$�A�+A�-A�-A�5?A�A�A�C�A�E�A�E�A�I�A�XA�bNA�ffA�bNA�`BA�ffA�hsA�hsA�dZA�hsA�n�A�t�A�v�A�t�A�n�A�x�AҁA�~�A�|�A�|�A�z�A�jA�`BA�I�A�A�A�A�A�A�A�?}A�;dA�1'A�$�A�AѺ^A�ffA�=qA� �A�{A�1A�A��HA��#A�ĜAв-AЬAЛ�A�v�A�l�A�9XA�;dA�A�ȴAϗ�A�z�A�bNA�S�A�1'A�$�A�A���A���A���A���A��A�ĜA�ƨAζFAΰ!AΕ�A΋DA�jA�S�A�9XA�33A��A�^5A�E�A�/A��A���A�ĜA�v�A�?}A�bA��mA˕�A��#AʑhA���Aɛ�A� �A�7LA���AǓuA�bNA�C�A�5?A�(�A�
=A��A��/A��
A�ȴAƺ^AƬAƝ�AƉ7A�\)A�7LA��A���A��/AőhAŝ�A�I�A�^5A�ffA� �A��;A��A�^5A��A��FA��-A�-A�A�~�A�M�A��A�\)A�ZA��A�S�A���A���A���A��DA�\)A�+A�A��#A���A�7LA�/A�&�A��A���A��A��HA��
A���A���A��9A�G�A���A��9A���A�l�A��mA�A�A�&�A�%A��7A��A��#A���A�5?A��A���A�^5A��A���A�\)A�K�A�
=A��
A��uA���A���A��`A�ƨA���A��PA�x�A�jA�VA�A�A�bA��TA��!A�jA�C�A�9XA�5?A�1'A�-A�
=A��A�z�A�ZA�G�A�9XA�/A�bA��A��-A�n�A�E�A�+A��A��;A�A���A�+A��-A� �A��RA�jA�+A�bA�  A���A��DA�\)A�K�A�+A���A��uA�x�A�G�A��A���A���A�G�A�{A�  A��
A�G�A���A�dZA��RA�x�A�O�A�Q�A��-A���A���A���A��A��+A�hsA�/A��
A�XA���A��A�dZA� �A��A�ĜA��-A��!A��uA�-A�1'A�C�A�hsA�I�A��FA�I�A��A���A�=qA�  A�  A�`BA�K�A���A�XA�bA���A�;dA�  A�ƨA�M�A��A��A�{A��jA��+A�ZA�+A�A��mA���A��!A��7A�ffA�M�A�33A�(�A� �A��A�oA�JA�A���A��A��mA��A�ƨA��-A���A��\A�z�A�ZA�?}A�$�A�  A��A��^A�hsA�"�A�ȴA�VA�
A|��Az9XAyVAx�Aw��Aw��AwdZAv��Av��Av9XAu\)At�`At�At�Atn�AtffAtbNAtI�As�As�Aq|�ApjAop�An$�AlZAk�
Ak�AkAj��AjZAi�-Ai�Ah��Ah��AhjAhQ�Ah5?Ah �Ag��Af{Ac;dAa33AaA`��A`�!A`jA_�A_\)A^��A^n�A^1'A]�A]��A]�wA]l�A\M�AXM�AUK�AR��AQXAP��AP��AP�9AP��AP�uAPv�AP^5APM�API�AP=qAP1'AP$�AP�APbAPAO�#AO��AOl�ANȴANbAM�7AL��ALv�AK�FAJĜAJ�AIO�AGp�AE��ADz�AD(�AC�^AC`BAC%AB�AB^5AB$�AB  AA�TAA��AA�^AA��AA��AA�7AA|�AAt�AAp�AAhsAAK�AA+AAoA@�yA@��A@��A@�+A@r�A@bNA@I�A@-A@1A?��A?�A?
=A>I�A=�A=��A=�PA<��A<M�A;��A;G�A:�/A:��A:��A:�\A:�DA:�DA:�+A:�+A:�+A:�+A:�DA:�A:~�A:v�A:VA: �A:  A9�TA9�FA9;dA9/A9
=A8�yA8��A8��A8�\A8E�A7��A7�;A7�A7dZA7;dA7VA7�A7VA6��A6��A6��A6�HA6�HA6�HA6�/A6�/A6�A6ȴA6��A6��A6^5A6-A6$�A6$�A6�A6JA6A5��A5�A5�mA5�;A5�
A5��A5ƨA5A5�^A5�FA5�A5��A5��A5��A5��A5�A5`BA4�A4��A4n�A4bNA4bNA4bNA4bNA4^5A4^5A4VA4-A4{A4JA3��A3�TA3�TA3�;A3��A3��A3dZA3C�A3VA2��A2�RA2�uA2n�A2bNA2ZA2$�A1�mA1�-A1�A1/A0��A1&�A1S�A1|�A1x�A133A1%A0��A1A1�A1�A0��A0�yA0ĜA0��A0�+A0n�A0^5A0^5A0ZA0E�A0$�A0�A0JA0A/��A/A/A/�wA/�^A/ƨA/A/|�A/O�A/"�A.��A.�A-�#A-�7A-"�A,��A,��A,��A-A,��A,��A,�DA,Q�A,bA+�#A+��A+t�A+oA*��A*�A*�A*�A*�A+
=A+7LA+?}A+?}A+XA+p�A+�A+�7A+�hA+��A+��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�HB��B�B�B�B�B��B�B�HB��B�pB�vB�BB�BB�B�vB�vB�vB�B�B��B�HB��B�B�HB�vB�B�B�vB�vB��B�B�|B�B�|B�B�B�|B�B�B��B�iB�
B	MB	��B
�B
$�B
)�B
GB
NB
OBB
OBB
MjB
J�B
GzB
8B
2�B
*�B
4nB
B
	�B
 B	��B	��B	ܒB	�}B	�B	��B	�B	�B	��B	��B	v�B	gB	d�B	=�B	+kB	($B	%B	;B�%B��B�BیB��B��BԕB��B�B�B�)B�B�9B�9B��B�BܒB�B	qB	0!B	3hB	?}B	<B	8RB	@�B	V�B	^jB	k�B	qvB	�MB	��B	��B	��B	��B	�kB	бB	ߤB	�B
 iB
�B
qB
!B
 �B
"hB
'RB
,�B
*0B
*eB
*�B
.�B
-wB
,�B
.�B
8B
<�B
B�B
I�B
C-B
K)B
MjB
L�B
PB
M6B
L�B
N�B
NB
NpB
JXB
E�B
EB
CaB
A B
AUB
CaB
J#B
MjB
M6B
MjB
NpB
Q�B
PB
O�B
O�B
O�B
O�B
O�B
MjB
LdB
J�B
K)B
L�B
JXB
J�B
J�B
L0B
K�B
LdB
L0B
J�B
K^B
J�B
H�B
GzB
F?B
E�B
E9B
D�B
CaB
C-B
D3B
C�B
C-B
B�B
A�B
AUB
@�B
B'B
A�B
?�B
?HB
?HB
?}B
>�B
>BB
>BB
<�B
<�B
<6B
<B
;�B
;dB
;�B
9�B
8�B
8�B
8B
7�B
7B
5�B
5B
4nB
2�B
2�B
0�B
0!B
/B
/�B
.}B
.B
,qB
,=B
+�B
+B
+B
*�B
(�B
(�B
(�B
)�B
)_B
&LB
%B
%�B
%FB
$tB
#�B
$@B
%B
$@B
$@B
#�B
#nB
#:B
"�B
!�B
!�B
"4B
�B
�B
~B
OB
CB
�B
OB
�B
B
�B
~B
B
B
�B
�B
qB
�B
	B
�B
=B
�B
�B
�B
+B
�B
+B
SB
�B
�B
{B
�B
B
�B
�B
@B
B
oB
:B
4B
hB
hB
hB
 B
bB
bB
.B
�B
�B
�B
�B
(B
�B
\B
 B
�B
�B
�B
\B
�B
�B
�B
�B
"B
�B
�B
�B
PB
~B
�B
~B
�B
�B
�B
~B
PB
B
�B
�B
~B
~B
JB
�B
�B
"B
~B
PB
~B
~B
�B
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
�B
"B
PB
�B
�B
�B
�B
�B
�B
�B
�B
VB
VB
�B
�B
\B
�B
�B
(B
�B
�B
�B
(B
(B
�B
(B
�B
�B
.B
�B
.B
�B
�B
�B
�B
�B
(B
�B
4B
�B
�B
hB
4B
�B
�B
�B
�B
�B
:B
B
uB
uB
@B
�B
�B
FB
FB
�B
�B
�B
�B
�B
B
MB
�B
YB
�B
�B
$B
$B
�B
�B
�B
�B
�B
B
B
�B
B
7B
�B
	B
qB
qB
�B
qB
�B
�B
�B
�B
�B
�B
�B
B
B
B
�B
�B
B
IB
B
B
~B
IB
�B
~B
~B
�B
VB
VB
�B
 �B
 \B
�B
�B
�B
 'B
 �B
 �B
 �B
 �B
!bB
!�B
!�B
"hB
"hB
!�B
#:B
#nB
#nB
$B
$@B
$tB
&B
&B
&B
&LB
&LB
&LB
&B
&LB
%�B
%�B
&B
&�B
&�B
'�B
'�B
($B
($B
(�B
)�B
)�B
)�B
)�B
*0B
*0B
*�B
*�B
+B
+kB
+kB
,B
,=B
,=B
,=B
,�B
-�B
.IB
.�B
.�B
/OB
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
1�B
1[B
2aB
2aB
2aB
2aB
2�B
2�B
2�B
2�B
2aB
2�B
2�B
3hB
4B
4B
4B
49B
4B
4nB
4nB
49B
4�B
4�B
4nB
3�B
49B
49B
4B
5B
5B
4�B
5�B
6zB
6�B
7B
7�B
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
8RB
8�B
9$B
9XB
9�B
9�B
9�B
9�B
:*B
:�B
:�B
;dB
;�B
;dB
;0B
;�B
<B
<jB
<�B
<�B
<�B
=�B
=�B
=qB
=qB
=<B
>wB
>�B
?B
?�B
?�B
?�B
?�B
?HB
?}B
@�B
@OB
AUB
AUB
AUB
A�B
B�B
B�B
B[B
B'B
B�B
C-B
B�B
C-B
C�B
DgB
D�B
D�B
D�B
D�B
EB
EmB
EB
EmB
E�B
FB
FB
F?B
F�B
F�B
GB
GB
GEB
GzB
GzB
G�B
GzB
HB
G�B
HB
G�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
JXB
J�B
J�B
J�B
J�B
K^B
K^B
K�B
K�B
K�B
K�B
K�B
LdB
LdB
MB
MB
L�B
MjB
MjB
M�B
NB
NpB
NB
N<B
NpB
N<B
NB
M�B
M�B
N�B
OB
OBB
OBB
O�B
PB
PB
O�B
O�B
O�B
O�B
O�B
PB
PHB
P�B
QB
Q�B
Q�B
Q�B
R B
S&B
S[B
S�B
S[B
S�B
S�B
S�B
T�B
T�B
U2B
U2B
UgB
UgB
UgB
UgB
UgB
U�B
VB
U�B
V�B
VmB
V�B
V�B
W
B
W?B
W�B
XB
W�B
W�B
W�B
WsB
W�B
W�B
X�B
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[WB
[�B
[�B
[�B
[�B
[�B
\]B
\)B
\�B
\�B
]�B
^5B
^�B
^jB
^�B
^jB
^jB
^�B
_pB
_�B
_�B
_pB
_�B
`�B
`vB
`vB
aHB
aHB
a|B
a�B
a�B
a�B
a�B
a|B
bNB
bNB
b�B
cTB
cTB
cTB
c�B
c�B
c�B
c�B
c�B
c�B
dZB
d�B
e`B
e,B
e,B
e,B
e`B
e`B
ffB
f�B
g8B
g�B
gmB
g�B
g�B
h
B
h
B
iDB
iyB
iyB
i�B
i�B
i�B
jB
jKB
j�B
j�B
kB
kQB
kQB
k�B
k�B
k�B
l�B
m)B
m�B
ncB
n�B
n�B
o B
n�B
n�B
n�B
n�B
o5B
o B
o5B
pB
p�B
qB
qB
qAB
qAB
q�B
q�B
r�B
sB
r�B
s�B
sMB
s�B
sMB
s�B
sMB
s�B
sMB
sMB
s�B
s�B
sMB
tTB
s�B
tB
t�B
u%B
u%B
u%B
t�B
u�B
v+B
u�B
v�B
v�B
wfB
v�B
w�B
wfB
w�B
xB
w�B
x8B
wfB
xlB
x�B
y	B
y>B
y>B
y�B
y�B
zxB
zDB
zB
zxB
z�B
{JB
{B
z�B
{B
{B
{B
{B
{�B
{B
{�B
|B
{B
|B
{�B
{�B
|B
|PB
|�B
|�B
|�B
}"B
}VB
}VB
}�B
}VB
~(B
~]B
}�B
~]B
}�B
~]B
~�B
~�B
~�B
~�B
�B
.B
�4B
�B
�B
�iB
�B
�4B
�iB
�iB
��B
�iB
�B
��B
�AB
��B
��B
��B
��B
��B
�GB
�{B
��B
�MB
�MB
�MB
�MB
��B
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
��B
��B
�%B
��B
��B
��B
��B
��B
��B
�+B
��B
�+B
��B
��B
��B
�1B
�fB
��B
��B�B�BB�B�NB�HB�B�|B��BߤB�BB�B�B�B�BޞB��B��B�vB�BB�B�B�5B�vB��B�BߤB�BB�|B�|B�pB��B�HB�HB�BB�B��B�B�;B�B��B�HB�B�B�pB�BB�|B�vB�;B�NB�HB�pB�jB�B�NB�5B�B��B�HB�;B�;B��B�B��B��B��B�vBߤB��B�B��B�B�;B�HB�B�BߤB�B��B��B�pB�B�HB�B�;B�B�B�vB�vB�vB��BߤB�B�HB�HB�B��B�B��BߤB�pB�HB�|B��B�BB�B�B�vB�B�BB�NB�vB�vB�&B�BߤB�B�TB�BB��B�B� B�BߤB�HB��B�B�HB�HB� B�NB��B�BB�B�BߤB�|B��B�B�BB�B�|B�B�;B�BB��B��B�pB�BB�HB�B�B��B�HB��B��B�B�HB�|B��BߤB��B�B�B��B�pB��B�|B�BߤBߤB�vB�HB�HB�B�BB�|B�B�BB�;B�|B�B�NB�B�B�|B�B�B�vB�B�NB�NB�NB��B�HB�B� B�B�B�B�HB�B�HB�B�B�B�B�B�vB��B�B��B�B�HB�B�NB�B�HB�|B�NB�B�B�B�vB�NB�B��B�HB�B�ZB�NB�,B�B�)B��B�B�B�B�GB��B�B�B�]B�"B�5B�fB�B	lB�B �BU�Ba|Bc�Bb�B[�B_;BbBbNBb�BbNB`�B_�B^�B`�Ba�B_�BNBTaBPBK�BC-BA�BDgB<6B:�B8�B6zB6FB6�B6zB4nB0�B0�B0!B0�B/OB(�B%zB$�B&�B&�B$tB$B$@B%zB$@B#B �B �B"4B#�B�B�B�B�B�B \B$�B%zB,qB.IB.}B-�B-CB.}B1�B5?B=�BR�Bc�BjBjKBk�Bm�BncBt�BuZBzDB~�B~�B�B�MB��B��B�VB��B��B��B�}B�aB��B�dB�dB��B�3B��B�UB�UB�-B��BɺB�6B�dB�&B�aBخB�WB��B��B�B:B�B
	B�BuBxB-�B3�B4B:�BJ�Bp�BqvB��B��B�?B�EB�mBޞB�>B�
B�>B�sB�cB�B�B�]B�iB�B�vB�5B�B�	B�	B��B�xB	 4B	_B�B	%B	2�B	^B	P�B	g8B	�\B	��B	�}B	�<B
�B	��B	�5B	�B	�B	�AB	�/B
"�B
eB
:B
B
�B
$B
�B
OB
�B
�B
$tB
&�B
/B
!bB
#�B
%FB
$B
$�B
$�B
"�B
"�B
 �B
%zB
1'B
(�B
#:B
 �B
#B
1'B
>B
NpB
P}B
:^B
K^B
@OB
@�B
N�B
@�B
K)B
F�B
P}B
K�B
HKB
E�B
M�B
J�B
L�B
tB
T,B
MjB
N�B
NB
K�B
K�B
JXB
K)B
L�B
OvB
MjB
U2B
QB
MB
LdB
K)B
K^B
JXB
P}B
R�B
N�B
LdB
JXB
H�B
GEB
K�B
HKB
NpB
H�B
J�B
FtB
K�B
C�B
C�B
FB
M�B
V�B
N�B
=qB
EB
=<B
5?B
3hB
<jB
9$B
5tB
2-B
4�B
>�B
4�B
-wB
3�B
.�B
2aB
.�B
,qB
$�B
"hB
'�B
*�B
xB
8�B
b�B
D�B
0�B
3�B
OB
($B
&�B
"hB
�B
\B
B
B
�B
%�B
(B
B
	�B
1B
	�B

=B
�B	��B
 �B
VB
~B
�B
�B
7B
AB	�B	�B	��B	�B
�B	��B
�B	��B	�KB	��B	�B	��B	�dB	�WB	�yB	�KB	خB	�`B	�,B	�KB	�TB	͟B	бB	ϫB	�#B	��B	�)B	ȴB	�zB	ĜB	��B	��B	��B	��B	�'B	�wB	��B	�}B	�B	�B	�B	��B	�wB	�dB	�^B	�XB	�RB	��B	�tB	��B	��B	��B	��B	��B	�6B	��B	уB	�4B	��B	��B	�GB	|�B	v+B	v�B	cB	oiB	w�B	yrB	n/B	jB	j�B	gB	d�B	b�B	b�B	e�B	jB	�B	[�B	`�B	YKB	n�B	?�B	DgB	?�B	9XB	8�B	HB	8B	.�B	*�B	*�B	&�B	&B	$tB	($B	K�B	V�B	"4B	�B	
=B	�B		�B	�B	
�B	�B	GB�rB��B�GB��B�B	;B	IB	B	�B�]B�B�B�5B��B��B�jB��B��BیBچB��B�KB�yB�EB�EB��BخB֡B�`B�jB�
B�5BٴB�vB�BܒB��B	YB�QB�B��B��B��BیB�QBٴBרB�
B��B�B�B��B��B��B�[B�&B��B��B��B��B� B�gB�&B�2B�2B�,B��B�aBԕB��B��B��B�B�BбB�B�2BܒB�5B��B�QB�jB�sB�yB�sBרB��B�EB�EB�yBרB֡B��B�mB�sBچB��B�QBٴBںB�fBخB��BٴBݘB�5B�]B��B�pB�KB��B�B�QB�B�B֡B��B�2B��B�gB�mB�9B��B՛B�9B��B�2B֡B�#B�?B�2B�2B��B՛B�B��B�9B֡B֡B��B�9B�9B�mB֡B�
B�
B֡B��B�2B�gB��B��B�B�B�BںB��B��B�B��B�B�B�B��B�B��B�B�EB�yB�QB��B�jB��B�;B�5BیBܒB�)B�B��B�;BݘBݘB��B��BݘB�,B�B	�B	B	�B	 B	SB	4B		B	!-B	#�B	%�B	)_B	-�B	.}B	0!B	/OB	/OB	2aB	2�B	2aB	1'B	1�B	1�B	9�B	2aB	3hB	3hB	4nB	1�B	7�B	:�B	:�B	?B	=qB	D�B	J�B	C�B	B'B	9$B	7�B	9XB	9�B	<6B	@B	<�B	>BB	=qB	8�B	6B	9�B	9$B	4�B	4B	5B	5?B	4�B	3�B	AUB	J�B	MB	J#B	P�B	T�B	T�B	T�B	V9B	XyG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                            B�B�B�vB�vB�B��B��B�B�BߊB�pB�\B�BB�\B��B�vB�vB�vB�B�vB��B�-B��B�B�HB�vB�B�B�vB�vB��B�B�|B�B�|B�B�B�B�B�"B�.B�sBUB	7LB	�B
!HB
/B
E�B
WsB
\�B
U2B
T�B
TB
T�B
T�B
B'B
A;B
R�B
H1B
�B
�B
,�B
�B	��B	�sB	�B	��B	��B	�B	�aB	�1B	�*B	~]B	u?B	utB	F�B	;�B	5?B	�B	!HB��B��B�BB�QB�RB�B�#B��B�OB�B��BخB�+BٚBܬBބB�HB��B	dB	2-B	7�B	EmB	@�B	9XB	?�B	U�B	_B	r�B	r�B	��B	�B	��B	��B	�B	��B	�MB	�-B	�AB
 �B
�B
]B
;B
 �B
"�B
(>B
/iB
+�B
,"B
*�B
/�B
.�B
-CB
/OB
8�B
=VB
C�B
L�B
C�B
KxB
N�B
M�B
R�B
N�B
N�B
P�B
QB
RB
L�B
GEB
GzB
E�B
B'B
B'B
D�B
J�B
M�B
MPB
M�B
P.B
SB
P�B
P}B
P�B
P�B
QNB
QhB
N�B
N<B
K�B
L�B
N<B
K�B
K�B
K^B
L�B
LJB
L�B
L�B
L�B
MB
K�B
I�B
H1B
F�B
F�B
GB
EB
C�B
D�B
FtB
DgB
C�B
B�B
B�B
B[B
B�B
D�B
DgB
@�B
?�B
@iB
@OB
?}B
@OB
?HB
=qB
<�B
<jB
<PB
<PB
=qB
=�B
<�B
:B
9�B
8�B
8B
7�B
6�B
6FB
5tB
49B
3�B
1[B
1B
0�B
1vB
0B
/OB
,�B
-�B
,�B
,=B
,qB
,"B
)yB
)B
)_B
,B
+�B
'�B
&�B
'�B
%�B
$�B
$ZB
%zB
&2B
%`B
$�B
$@B
#�B
#�B
# B
#�B
$�B
$&B
 �B
�B
VB
 B
/B
�B
;B
VB
�B
OB
�B
~B
�B
B
~B
B
�B
=B
qB
]B
�B
�B
=B
�B
�B
�B
�B

B
B
B
�B
�B
�B
MB
�B
9B
uB
�B
�B
B
&B
�B
�B
hB
hB
�B
�B
�B
bB
}B
bB
}B
�B
oB
�B
bB
�B
�B
�B
B
B
vB
�B
\B
pB
�B
"B
�B
4B
PB
<B
"B
B
PB
<B
�B
�B
�B
�B
�B
~B
0B
�B
\B
6B
�B
�B
B
jB
B
�B
6B
PB
PB
�B
6B
PB
�B
B
�B
�B
�B
jB
�B
vB
�B
<B
VB
�B
�B
BB
�B
�B
�B
�B
BB
B
vB
(B
(B
(B
\B
�B
\B
vB
�B
�B
�B
.B
 B
.B
.B
�B
�B
�B
�B
TB
�B
 B
�B
�B
�B
�B
[B
�B
[B
&B
�B
FB
,B
�B
B
gB
�B
�B
�B
YB
�B
B
mB
B
�B
B
B
�B
YB
�B
�B
+B
KB
7B
�B
QB
7B
B
7B
QB
kB
�B
�B
�B
�B
�B
�B
�B
)B
B
)B
)B
CB
�B
�B
�B
�B
�B
�B
~B
B
�B
dB
�B
B
�B
�B
B
�B
�B
�B
�B
 �B
!-B
!bB
 �B
 �B
 �B
 �B
!HB
!B
!B
!HB
!�B
"B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$ZB
$�B
%`B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'mB
&fB
&B
&�B
&�B
'8B
(
B
(XB
(�B
(XB
)_B
)�B
)�B
)�B
*0B
*�B
*�B
+QB
+6B
+�B
+�B
,=B
-)B
,�B
,�B
,�B
-�B
.}B
.�B
/5B
/OB
/�B
/�B
/�B
/�B
/�B
0UB
0!B
0;B
0�B
1[B
0�B
1�B
2GB
1�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3MB
3hB
4�B
4TB
4B
49B
4nB
4TB
4�B
4�B
4nB
5B
5%B
5B
4TB
4�B
4�B
4�B
5�B
5tB
5ZB
5�B
6�B
7B
7�B
88B
8B
88B
8B
7�B
8B
8�B
8�B
88B
88B
8�B
8�B
9	B
9XB
9�B
9�B
9�B
9�B
:B
:^B
:�B
;�B
<6B
<6B
;�B
;dB
<B
<�B
<�B
=B
=B
=�B
>]B
=�B
=�B
>(B
>(B
?B
?}B
?�B
@iB
@OB
?�B
?�B
?�B
@iB
A B
A B
A�B
A�B
A�B
B�B
B�B
B�B
BuB
B�B
CGB
CaB
CGB
C�B
D�B
D�B
D�B
EB
E9B
EB
EmB
E�B
E�B
E�B
FYB
F�B
FtB
F�B
F�B
G+B
G_B
G_B
G�B
G�B
G�B
G�B
G�B
HfB
H1B
HfB
HKB
IlB
J#B
I�B
I�B
J	B
I�B
J#B
J�B
J�B
J�B
K)B
KDB
K�B
K�B
K�B
K�B
LJB
L0B
LdB
L�B
L�B
M�B
M6B
M6B
M�B
M�B
NB
NpB
N�B
N"B
NVB
N�B
N<B
NB
M�B
N<B
O\B
O\B
O�B
O�B
P}B
PHB
P.B
O�B
O�B
O�B
P.B
PbB
P}B
Q B
QNB
Q�B
Q�B
R:B
RB
S&B
S�B
S�B
S�B
SuB
S�B
TaB
TFB
UgB
UMB
UgB
U�B
U�B
U�B
U�B
U�B
U�B
VB
VSB
V9B
W
B
V�B
W$B
W$B
WYB
W�B
W�B
X+B
W�B
W�B
W�B
W�B
XB
X�B
Y�B
Y�B
ZB
Y�B
Y�B
Y�B
ZkB
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
\)B
\]B
\CB
\�B
\xB
]B
]�B
]�B
^�B
^�B
^�B
^�B
^�B
^�B
_!B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
a�B
a|B
a�B
a�B
a�B
a�B
a�B
a�B
b�B
b�B
cTB
cnB
cnB
c�B
c�B
c�B
c�B
c�B
d@B
dZB
d�B
eFB
ezB
eFB
eFB
e`B
e�B
e�B
gB
gmB
g�B
g�B
g�B
g�B
h>B
hsB
h�B
i�B
i�B
i�B
i�B
i�B
i�B
jKB
j�B
kB
k6B
kB
kkB
k�B
k�B
k�B
l=B
m]B
m]B
m�B
n�B
n�B
n�B
oB
n�B
n�B
n�B
n�B
oOB
o5B
o�B
p�B
qB
qAB
qvB
q�B
q�B
rGB
rGB
sMB
s�B
s3B
s�B
shB
s�B
sMB
s�B
shB
s�B
shB
shB
s�B
s�B
s�B
tnB
tB
t�B
uB
u�B
uZB
uZB
utB
vzB
v`B
v+B
wB
v�B
w�B
wLB
xB
w�B
w�B
xB
w�B
x8B
w�B
x�B
y>B
y�B
yrB
yrB
y�B
zB
z�B
zxB
z^B
z�B
z�B
{dB
{0B
{B
{0B
{B
{0B
{0B
{�B
{B
{�B
|B
{�B
|6B
|B
|B
|6B
|jB
|�B
|�B
|�B
}<B
}�B
}�B
}�B
}�B
~BB
~wB
}�B
~wB
~BB
~�B
~�B
B
B
~�B
�B
cB
�OB
�B
�B
��B
� B
�iB
��B
�iB
��B
��B
� B
��B
�[B
�B
�B
�B
��B
��B
��B
��B
�B
�gB
�gB
��B
�gB
��B
��B
�SB
��B
��B
�mB
��B
�SB
��B
��B
��B
��B
��B
��B
�B
��B
�B
�B
��B
��B
�EB
��B
�EB
��B
�B
��B
�1B
�fB
��G�O�B�B�BB�B�NB�HB�B�|B��BߤB�BB�B�B�B�BޞB��B��B�vB�BB�B�B�5B�vB��B�BߤB�BB�|B�|B�pB��B�HB�HB�BB�B��B�B�;B�B��B�HB�B�B�pB�BB�|B�vB�;B�NB�HB�pB�jB�B�NB�5B�B��B�HB�;B�;B��B�B��B��B��B�vBߤB��B�B��B�B�;B�HB�B�BߤB�B��B��B�pB�B�HB�B�;B�B�B�vB�vB�vB��BߤB�B�HB�HB�B��B�B��BߤB�pB�HB�|B��B�BB�B�B�vB�B�BB�NB�vB�vB�&B�BߤB�B�TB�BB��B�B� B�BߤB�HB��B�B�HB�HB� B�NB��B�BB�B�BߤB�|B��B�B�BB�B�|B�B�;B�BB��B��B�pB�BB�HB�B�B��B�HB��B��B�B�HB�|B��BߤB��B�B�B��B�pB��B�|B�BߤBߤB�vB�HB�HB�B�BB�|B�B�BB�;B�|B�B�NB�B�B�|B�B�B�vB�B�NB�NB�NB��B�HB�B� B�B�B�B�HB�B�HB�B�B�B�B�B�vB��B�B��B�B�HB�B�NB�B�HB�|B�NB�B�B�B�vB�NB�B��B�HB�B�ZB�NB�,B�B�)B��B�B�B�B�GB��B�B�B�]B�"B�5B�fB�B	lB�B �BU�Ba|Bc�Bb�B[�B_;BbBbNBb�BbNB`�B_�B^�B`�Ba�B_�BNBTaBPBK�BC-BA�BDgB<6B:�B8�B6zB6FB6�B6zB4nB0�B0�B0!B0�B/OB(�B%zB$�B&�B&�B$tB$B$@B%zB$@B#B �B �B"4B#�B�B�B�B�B�B \B$�B%zB,qB.IB.}B-�B-CB.}B1�B5?B=�BR�Bc�BjBjKBk�Bm�BncBt�BuZBzDB~�B~�B�B�MB��B��B�VB��B��B��B�}B�aB��B�dB�dB��B�3B��B�UB�UB�-B��BɺB�6B�dB�&B�aBخB�WB��B��B�B:B�B
	B�BuBxB-�B3�B4B:�BJ�Bp�BqvB��B��B�?B�EB�mBޞB�>B�
B�>B�sB�cB�B�B�]B�iB�B�vB�5B�B�	B�	B��B�xB	 4B	_B�B	%B	2�B	^B	P�B	g8B	�\B	��B	�}B	�<B
�B	��B	�5B	�B	�B	�AB	�/B
"�B
eB
:B
B
�B
$B
�B
OB
�B
�B
$tB
&�B
/B
!bB
#�B
%FB
$B
$�B
$�B
"�B
"�B
 �B
%zB
1'B
(�B
#:B
 �B
#B
1'B
>B
NpB
P}B
:^B
K^B
@OB
@�B
N�B
@�B
K)B
F�B
P}B
K�B
HKB
E�B
M�B
J�B
L�B
tB
T,B
MjB
N�B
NB
K�B
K�B
JXB
K)B
L�B
OvB
MjB
U2B
QB
MB
LdB
K)B
K^B
JXB
P}B
R�B
N�B
LdB
JXB
H�B
GEB
K�B
HKB
NpB
H�B
J�B
FtB
K�B
C�B
C�B
FB
M�B
V�B
N�B
=qB
EB
=<B
5?B
3hB
<jB
9$B
5tB
2-B
4�B
>�B
4�B
-wB
3�B
.�B
2aB
.�B
,qB
$�B
"hB
'�B
*�B
xB
8�B
b�B
D�B
0�B
3�B
OB
($B
&�B
"hB
�B
\B
B
B
�B
%�B
(B
B
	�B
1B
	�B

=B
�B	��B
 �B
VB
~B
�B
�B
7B
AB	�B	�B	��B	�B
�B	��B
�B	��B	�KB	��B	�B	��B	�dB	�WB	�yB	�KB	خB	�`B	�,B	�KB	�TB	͟B	бB	ϫB	�#B	��B	�)B	ȴB	�zB	ĜB	��B	��B	��B	��B	�'B	�wB	��B	�}B	�B	�B	�B	��B	�wB	�dB	�^B	�XB	�RB	��B	�tB	��B	��B	��B	��B	��B	�6B	��B	уB	�4B	��B	��B	�GB	|�B	v+B	v�B	cB	oiB	w�B	yrB	n/B	jB	j�B	gB	d�B	b�B	b�B	e�B	jB	�B	[�B	`�B	YKB	n�B	?�B	DgB	?�B	9XB	8�B	HB	8B	.�B	*�B	*�B	&�B	&B	$tB	($B	K�B	V�B	"4B	�B	
=B	�B		�B	�B	
�B	�B	GB�rB��B�GB��B�B	;B	IB	B	�B�]B�B�B�5B��B��B�jB��B��BیBچB��B�KB�yB�EB�EB��BخB֡B�`B�jB�
B�5BٴB�vB�BܒB��B	YB�QB�B��B��B��BیB�QBٴBרB�
B��B�B�B��B��B��B�[B�&B��B��B��B��B� B�gB�&B�2B�2B�,B��B�aBԕB��B��B��B�B�BбB�B�2BܒB�5B��B�QB�jB�sB�yB�sBרB��B�EB�EB�yBרB֡B��B�mB�sBچB��B�QBٴBںB�fBخB��BٴBݘB�5B�]B��B�pB�KB��B�B�QB�B�B֡B��B�2B��B�gB�mB�9B��B՛B�9B��B�2B֡B�#B�?B�2B�2B��B՛B�B��B�9B֡B֡B��B�9B�9B�mB֡B�
B�
B֡B��B�2B�gB��B��B�B�B�BںB��B��B�B��B�B�B�B��B�B��B�B�EB�yB�QB��B�jB��B�;B�5BیBܒB�)B�B��B�;BݘBݘB��B��BݘB�,B�B	�B	B	�B	 B	SB	4B		B	!-B	#�B	%�B	)_B	-�B	.}B	0!B	/OB	/OB	2aB	2�B	2aB	1'B	1�B	1�B	9�B	2aB	3hB	3hB	4nB	1�B	7�B	:�B	:�B	?B	=qB	D�B	J�B	C�B	B'B	9$B	7�B	9XB	9�B	<6B	@B	<�B	>BB	=qB	8�B	6B	9�B	9$B	4�B	4B	5B	5?B	4�B	3�B	AUB	J�B	MB	J#B	P�B	T�B	T�B	T�B	V9B	XyG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�)O<��c=*�=��<е�<1WX</�<�,q<�)O<od9<#�
<#�
<#�
<.1�<X�c<)|?<p��=κ<��T<_a�<��<�[�<ݻ"<�#<E��<#�
<#�
<#�
<#�
<<c3=�<L?�<#�
<j�L<��<#�
<��<X�c<DN�=38<#�
<#�
<#�
<�U<���<#�
<#�
<h�=<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2019010618164120190106181641IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019011618004420190116180044QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019011618004420190116180044QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551320190521075513IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                