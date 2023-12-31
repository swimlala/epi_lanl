CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-11-20T03:43:16Z creation; 2023-04-26T19:14:28Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191120034316  20230426191428  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               D   DAA  AOAO7316_008644_068                 7316_008644_068                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @��G �D@��G �D11  @��GU�=@��GU�=@)]�O�B�@)]�O�B��c�پL�I�c�پL�I11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @=p�@�  @�  @�G�@�G�@��RA\)A   A+�A@  A`��A�  A�  A�Q�A�Q�A�Q�A�  A�  A�  A��B  B  B  B�
B'�
B/�
B7�
B?�BG�
BP(�BXQ�B`(�Bg�
Bp  Bx(�B�{B�{B�{B�{B��B�  B�  B�{B�{B�{B�{B�z�B��
B�Q�B�G�B��
B�{B�(�B�{B�  B�  B��B��B�{B�{B��B��
B��B�{B�(�B�{B�{C   C��C��C��C  C	��C��C  C  C��C  C  C
=C
=C  C  C 
=C"
=C$  C&
=C(
=C*
=C,  C.  C0  C2  C4  C6  C7��C:  C<
=C>
=C@
=CB
=CC��CE��CG��CI��CL  CN  CO��CQ��CS��CV
=CX  CY��C\
=C^
=C_��Cb  Cd
=Cf  Cg��Cj
=Cl{Cn
=Cp  Cq��Cs�HCu�Cw��Cz
=C|  C}��C�  C�  C�C�  C���C���C�  C�C�  C���C�  C�C�  C�  C�C�  C�C�
=C�C�  C�C�  C�  C�C�C�  C�  C�  C�  C�  C�C�C���C�  C�  C�  C�  C���C�  C�C���C�  C�  C�C�
=C�C�  C���C�C�
=C�  C�
=C�\C�C���C�  C�C�  C�C�
=C�  C���C���C���C���C�  C�  C�  C�C�
=C�  C�  C�C�C�C���C���C���C���C�  C�  C���C�  C�C�  C���C�  C�C�  C���C���C���C�C�  C�C�
=C�C�  C�
=C�\C�
=C�C�  C�  C���C�  C�  C�C�
=C�  C�C�
=C�  C�  C�  C���C���C���C���C���C�  C�C�  C���C���C�  C���C�  D �D � D �qD� D�D�DD��D�qD� D�D}qD��D}qD  D}qD  D��D	D	��D
�D
� D  D}qD�qD� DD�D�D� D  D}qD  D� D  D� D  D��D�qD� DD�D�D��D  D��D�qDz�D�qD}qD��D� D  D}qD�qD}qD��D}qD�D�D  D��DD� D��D }qD!  D!}qD"�D"��D#�D#� D#��D$� D%D%��D&  D&� D&�qD'� D(D(��D)�D)��D*  D*}qD+�D+��D,D,�D-�D-� D.  D.}qD.�qD/}qD/��D0��D1�D1� D2  D2z�D2��D3z�D4  D4� D5  D5�D6D6�D7�D7��D8�D8� D9  D9� D:  D:� D:�qD;}qD;�qD<� D=  D=}qD>  D>��D?  D?� D@  D@� DA�DA��DA�qDB� DB�qDC}qDC�qDD� DE�DE��DF  DF� DF�qDG}qDH  DH��DI�DI� DI�qDJ}qDJ�qDK� DLDL� DL�qDM� DN  DN}qDO  DO� DP  DP��DQ  DQ��DR  DR}qDS  DS��DT  DT� DU�DU� DV�DV� DW  DW��DX�DX� DX�qDY}qDZ�DZ�D[  D[}qD\  D\��D]�D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Db�qDc}qDd  Dd��De  Dez�De�qDf}qDg  Dg� Dg�qDh}qDh�qDi� Di�qDj}qDk  Dk��Dl  Dl� Dm�Dm�DnDn��Do  Do}qDp  Dp� Dq  Dq}qDq��Dr}qDs  Ds� Dt�Dt��Du�Du}qDu�qDv}qDv�qDw� Dx  Dx� Dx��Dy}qDz  Dz� D{�D{� D|  D|}qD}  D}��D~  D~� D  D}qD�qD�>�D��HD��HD���D�@ D��HD��HD�  D�=qD�� D�� D��qD�=qD�� D��HD�  D�AHD��HD�� D���D�>�D�~�D�� D�  D�>�D�� D���D�  D�>�D�~�D��HD��D�AHD�� D���D��qD�@ D��HD��HD�  D�>�D�}qD�� D�  D�@ D�~�D�� D��D�@ D�}qD���D���D�>�D�~�D�� D�  D�AHD��HD�D�  D�>�D�� D�� D�HD�AHD��HD�� D�HD�@ D�~�D��HD�  D�@ D�~�D���D�  D�B�D��HD��qD���D�>�D�~�D��HD�HD�AHD��HD���D���D�@ D�� D��HD���D�>�D��HD��HD���D�=qD�~�D�� D�  D�>�D�� D��HD�  D�>�D�� D�D��D�B�D��HD�� D�  D�@ D�� D��HD�HD�AHD�~�D���D�  D�=qD�~�D�� D�HD�B�D��HD���D��qD�@ D���D��HD�  D�>�D�� D���D��qD�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD���D�� D�HD�B�D��HD��HD��D�@ D�}qD���D���D�=qD�� D��HD�HD�@ D��HD���D��D�AHD�� D���D���D�AHD���D�D��D�B�D��HD��HD�HD�AHD�� D�� D�  D�@ D�~�D���D�  D�@ D�� D��HD�HD�B�D�� D��)D�  D�@ D�}qD��qD��qD�=qD�}qD���D���D�>�D�~�D���D��qD�=qD�}qD��qD���D�>�D�~�D�� D�  D�@ D�� D�� D���D�=qD�}qD��qD���D�@ D�~�D�� D��D�>�D�~�D�� D���D�@ D��HD�� D���D�>�D�~�D��qD���D�AHD�� D���D��qD�>�D�� D���D��D�B�D��HD�� D�  D�@ D�� D��HD�  D�>�D�~�D���D���D�@ D�� D���D���D�@ D�� D��HD�  D�@ D D��HD�HD�@ D�~�D�� D�  D�@ DāHD��HD�  D�@ DŁHD�� D�HD�AHDƁHD��HD�  D�@ Dǀ D��HD�  D�@ DȁHD�� D�  D�@ Dɀ D��HD�  D�AHDʁHD��HD�HD�@ Dˀ D˾�D���D�>�D�~�D�� D�HD�@ D̀ D;�D���D�>�D΁HD��HD���D�@ Dπ D�� D�HD�@ DЀ D��HD�  D�>�Dр D��HD�  D�AHDҁHDҾ�D��qD�@ DӀ DӾ�D�  D�AHDԁHD�� D���D�>�D�~�Dվ�D���D�>�Dր D��HD�  D�@ DׁHD��HD�  D�@ D؀ Dؾ�D�  D�AHDفHD�� D�  D�@ Dڂ�D��HD�  D�@ D�~�D۾�D�  D�AHD܀ Dܾ�D�HD�@ D�}qD�� D�  D�=qDހ D޾�D���D�AHD߀ D߾�D���D�AHD���D�� D�HD�AHD� D�� D���D�@ D�~�D⾸D�  D�>�D�~�D�� D�  D�@ D�~�D侸D�  D�AHD�~�D徸D�HD�@ D�~�D�� D�  D�@ D�HD羸D���D�=qD�~�D�� D�  D�AHD�HD�� D�  D�AHD� D꾸D�  D�@ D�~�D뾸D�  D�@ D�}qD쾸D�  D�AHD�HD�� D���D�@ D�~�DD�HD�AHD� D�� D�  D�>�D�� D��HD�  D�>�D� D�� D�  D�@ D� D�� D�  D�>�D�~�D�D�  D�@ D� D���D�  D�=qD�}qD�� D�HD�AHD�� D�� D���D�>�D�� D�� D���D�=qD�~�D�� D�HD�@ D�~�D�� D�  D�@ D�l�D��\>�?.{?aG�?��?��
?�p�?�G�?�@��@z�@&ff@333@E�@Q�@aG�@k�@}p�@��@�=q@�z�@���@��\@���@��@���@�  @�=q@�\)@ٙ�@�G�@���@�33@�Q�A�A�A
�HA{A33A�A�A   A#33A(��A+�A1G�A5�A:�HA>�RAC33AHQ�AL(�AQG�AU�AZ=qA^{Ac33Ag
=Al(�Ao\)As�
Aw�A{�A�Q�A��A�z�A�{A���A�=qA���A�ffA���A��HA�z�A�
=A���A��HA�p�A�
=A���A�33A�A��A��A�(�A�{A�Q�A�=qA���A�ffA���A��HA��A�\)A�G�AÅA�p�A�  Aə�A��
A�{AϮA�=qA��
A�{A�Q�A�=qA�z�A�{A�Q�A��HA�(�A�RA���A��HA��A�\)A���A�A��A�\)A��A��A�p�A��B ��B{B�HB(�B��B=qB
=BQ�B	�B
=qB�BQ�Bp�B�\B�B��Bp�B�HB�B��B�B�RB  B��B{B33B  BG�B=qB\)B z�B!p�B"�RB#�B$��B%B&�RB(  B(��B*{B+
=B,  B-G�B.{B/\)B0Q�B1G�B2�\B3�B4z�B5B6�\B7�B8��B9B;
=B<  B<��B>=qB?
=B@Q�BAG�BBffBC�BDz�BEBFffBG�BH��BIBJ�HBK�BL��BM�BN�HBP  BP��BR{BR�HBS�
BT��BUBW
=BW�
BX��BY�BZ�\B[�B\��B]G�B^ffB_33B`Q�BaG�Bb=qBc\)BdQ�BeG�BfffBg33Bhz�BiG�BjffBk�BlQ�Bm��BnffBo�
Bp��Bq�Bs
=Bt  Bu�BvffBw�Bx��By�B{
=B|Q�B}��B~�\B�
B�z�B���B�B�Q�B���B���B�{B���B�\)B��B�ffB��B��B�(�B���B�p�B�  B���B��B�B�ffB���B�p�B�(�B���B�\)B�{B��RB�p�B�  B��RB��B�{B���B���B�=qB��HB���B�ffB���B�B��\B��B��
B���B�G�B��B���B�p�B�(�B���B���B�Q�B���B��B�z�B�
=B��
B���B�G�B��
B���B�\)B��
B��\B�33B��B�  B�z�B���B�33B�p�B��B��B�{B�(�B�Q�B�z�B�z�B��\B��RB���B���B�
=B��B��B�G�B�p�B��B���B��B��B�{B�{B�Q�B�ffB�z�B��\B��HB���B�
=B�33B�p�B��B���B��B�  B�{B�=qB��\B���B��RB���B��B��B�\)B��B���B��B��B�{B�{B�=qB�ffB��\B��\B��RB��HB��HB���B��B�G�B�G�B�\)B��B���B���B��B��
B��
B��
B��B�{B�  B�{B�(�B�=qB�(�B�=qB�ffB�Q�B�Q�B�ffB�z�B�z�B�z�B�z�B���B��\B�z�B���B��RB��RB���B��RB���B��HB���B���B��HB���B���B���B�
=B�33B�G�B�33B�G�B�p�B���B��B��B��
B�  B�=qB�=qB�z�B���B���B��HB�
=B�\)B��B���B��
B�{B�Q�B�ffB��\B��HB�
=B�G�B�p�B���B��
B�=qB�z�B���B��HB��B��B��B�(�B�ffB��RB��B�p�B�B�  B�=qB���B��B�p�B��B�  B�ffB���B�33B�p�B�B�(�Bģ�B���B�\)Bř�B�  B�ffB��HB�33BǙ�B��B�=qBȸRB�33Bə�B�  B�Q�B���B�G�B�B�(�B�z�B���B�p�B��
B�ffB��HB�\)B�B�(�BУ�B�33B�B�=qBң�B��Bә�B�  B�z�B�
=Bՙ�B�{B�z�B��HB�\)B��
B�ffB��HB�\)B��
B�Q�BڸRB�33Bۙ�B�(�Bܣ�B�33Bݙ�B�  B�ffB���B�p�B��B�ffB�RB��BᙚB�  B�\B�
=B�p�B��
B�(�B��B��B噚B�  B�ffB��HB�33B�B�{B�\B�
=B�B�  B�ffB��HB�\)B��
B�=qB�RB�33B��B�=qB�RB�33B�B�(�B��B�
=B�B��B�ffB���B�\)B��
B�ffB��HB�\)B��
B�=qB��RB�33B���B�{B�z�B���B�\)B��
B�Q�B���B�G�B��
B�=qB��RB�33B��B�=qB��RB�G�B��C {C Q�C �\C ��C  C=qCz�C�RC��C(�CffC�C�C(�CffC�C��C33Cz�C�RC  C=qC�C��C{CQ�C��C�HC(�CffC�RC��C=qC�CC	
=C	Q�C	��C	�HC
�C
p�C
�RC
=CG�C��C�HC(�Cz�CC
=CQ�C��C�
C(�Cp�C�C��C=qC�C�
C�Cp�C�RC
=CQ�C��C�HC�Cp�C�RC  CG�C��C�C=qC�C�
C�Cp�C�RC  CQ�C��C�C33C�C�
C(�Cz�C�
C�Cz�C��C�Cp�CC{C\)C�RC
=CffC�RC
=CffC�RC
=C\)C�C  CQ�C��C��C G�C ��C �C!G�C!��C!��C"G�C"��C"��C#Q�C#��C#��C$Q�C$��C$��C%G�C%��C%�C&G�C&��C&�C'=qC'��C'�C(=qC(�\C(�C)=qC)��C)�C*G�C*��C*�C+=qC+�\C+�C,=qC,�\C,�HC-33C-�C-�
C.33C.z�C.��C/(�C/p�C/��C0{C0ffC0�RC1{C1ffC1�RC2  C2\)C2�C3  C3Q�C3��C3��C4=qC4�\C4�HC5(�C5�C5�
C6�C6p�C6�RC7
=C7Q�C7��C7�C8=qC8�\C8�
C9(�C9z�C9C:
=C:Q�C:��C:��C;33C;z�C;��C<{C<\)C<��C<�C==qC=�C=��C>
=C>\)C>��C>��C?=qC?�C?��C@{C@ffC@�C@��CAG�CA�\CA�
CB(�CBp�CB�RCC
=CCQ�CC��CC�CD=qCD�CD�
CE�CEp�CE�RCF  CFQ�CF��CF�CG33CGz�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                   ?��@   @=p�@�  @�  @�G�@�G�@��RA\)A   A+�A@  A`��A�  A�  A�Q�A�Q�A�Q�A�  A�  A�  A��B  B  B  B�
B'�
B/�
B7�
B?�BG�
BP(�BXQ�B`(�Bg�
Bp  Bx(�B�{B�{B�{B�{B��B�  B�  B�{B�{B�{B�{B�z�B��
B�Q�B�G�B��
B�{B�(�B�{B�  B�  B��B��B�{B�{B��B��
B��B�{B�(�B�{B�{C   C��C��C��C  C	��C��C  C  C��C  C  C
=C
=C  C  C 
=C"
=C$  C&
=C(
=C*
=C,  C.  C0  C2  C4  C6  C7��C:  C<
=C>
=C@
=CB
=CC��CE��CG��CI��CL  CN  CO��CQ��CS��CV
=CX  CY��C\
=C^
=C_��Cb  Cd
=Cf  Cg��Cj
=Cl{Cn
=Cp  Cq��Cs�HCu�Cw��Cz
=C|  C}��C�  C�  C�C�  C���C���C�  C�C�  C���C�  C�C�  C�  C�C�  C�C�
=C�C�  C�C�  C�  C�C�C�  C�  C�  C�  C�  C�C�C���C�  C�  C�  C�  C���C�  C�C���C�  C�  C�C�
=C�C�  C���C�C�
=C�  C�
=C�\C�C���C�  C�C�  C�C�
=C�  C���C���C���C���C�  C�  C�  C�C�
=C�  C�  C�C�C�C���C���C���C���C�  C�  C���C�  C�C�  C���C�  C�C�  C���C���C���C�C�  C�C�
=C�C�  C�
=C�\C�
=C�C�  C�  C���C�  C�  C�C�
=C�  C�C�
=C�  C�  C�  C���C���C���C���C���C�  C�C�  C���C���C�  C���C�  D �D � D �qD� D�D�DD��D�qD� D�D}qD��D}qD  D}qD  D��D	D	��D
�D
� D  D}qD�qD� DD�D�D� D  D}qD  D� D  D� D  D��D�qD� DD�D�D��D  D��D�qDz�D�qD}qD��D� D  D}qD�qD}qD��D}qD�D�D  D��DD� D��D }qD!  D!}qD"�D"��D#�D#� D#��D$� D%D%��D&  D&� D&�qD'� D(D(��D)�D)��D*  D*}qD+�D+��D,D,�D-�D-� D.  D.}qD.�qD/}qD/��D0��D1�D1� D2  D2z�D2��D3z�D4  D4� D5  D5�D6D6�D7�D7��D8�D8� D9  D9� D:  D:� D:�qD;}qD;�qD<� D=  D=}qD>  D>��D?  D?� D@  D@� DA�DA��DA�qDB� DB�qDC}qDC�qDD� DE�DE��DF  DF� DF�qDG}qDH  DH��DI�DI� DI�qDJ}qDJ�qDK� DLDL� DL�qDM� DN  DN}qDO  DO� DP  DP��DQ  DQ��DR  DR}qDS  DS��DT  DT� DU�DU� DV�DV� DW  DW��DX�DX� DX�qDY}qDZ�DZ�D[  D[}qD\  D\��D]�D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Db�qDc}qDd  Dd��De  Dez�De�qDf}qDg  Dg� Dg�qDh}qDh�qDi� Di�qDj}qDk  Dk��Dl  Dl� Dm�Dm�DnDn��Do  Do}qDp  Dp� Dq  Dq}qDq��Dr}qDs  Ds� Dt�Dt��Du�Du}qDu�qDv}qDv�qDw� Dx  Dx� Dx��Dy}qDz  Dz� D{�D{� D|  D|}qD}  D}��D~  D~� D  D}qD�qD�>�D��HD��HD���D�@ D��HD��HD�  D�=qD�� D�� D��qD�=qD�� D��HD�  D�AHD��HD�� D���D�>�D�~�D�� D�  D�>�D�� D���D�  D�>�D�~�D��HD��D�AHD�� D���D��qD�@ D��HD��HD�  D�>�D�}qD�� D�  D�@ D�~�D�� D��D�@ D�}qD���D���D�>�D�~�D�� D�  D�AHD��HD�D�  D�>�D�� D�� D�HD�AHD��HD�� D�HD�@ D�~�D��HD�  D�@ D�~�D���D�  D�B�D��HD��qD���D�>�D�~�D��HD�HD�AHD��HD���D���D�@ D�� D��HD���D�>�D��HD��HD���D�=qD�~�D�� D�  D�>�D�� D��HD�  D�>�D�� D�D��D�B�D��HD�� D�  D�@ D�� D��HD�HD�AHD�~�D���D�  D�=qD�~�D�� D�HD�B�D��HD���D��qD�@ D���D��HD�  D�>�D�� D���D��qD�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD���D�� D�HD�B�D��HD��HD��D�@ D�}qD���D���D�=qD�� D��HD�HD�@ D��HD���D��D�AHD�� D���D���D�AHD���D�D��D�B�D��HD��HD�HD�AHD�� D�� D�  D�@ D�~�D���D�  D�@ D�� D��HD�HD�B�D�� D��)D�  D�@ D�}qD��qD��qD�=qD�}qD���D���D�>�D�~�D���D��qD�=qD�}qD��qD���D�>�D�~�D�� D�  D�@ D�� D�� D���D�=qD�}qD��qD���D�@ D�~�D�� D��D�>�D�~�D�� D���D�@ D��HD�� D���D�>�D�~�D��qD���D�AHD�� D���D��qD�>�D�� D���D��D�B�D��HD�� D�  D�@ D�� D��HD�  D�>�D�~�D���D���D�@ D�� D���D���D�@ D�� D��HD�  D�@ D D��HD�HD�@ D�~�D�� D�  D�@ DāHD��HD�  D�@ DŁHD�� D�HD�AHDƁHD��HD�  D�@ Dǀ D��HD�  D�@ DȁHD�� D�  D�@ Dɀ D��HD�  D�AHDʁHD��HD�HD�@ Dˀ D˾�D���D�>�D�~�D�� D�HD�@ D̀ D;�D���D�>�D΁HD��HD���D�@ Dπ D�� D�HD�@ DЀ D��HD�  D�>�Dр D��HD�  D�AHDҁHDҾ�D��qD�@ DӀ DӾ�D�  D�AHDԁHD�� D���D�>�D�~�Dվ�D���D�>�Dր D��HD�  D�@ DׁHD��HD�  D�@ D؀ Dؾ�D�  D�AHDفHD�� D�  D�@ Dڂ�D��HD�  D�@ D�~�D۾�D�  D�AHD܀ Dܾ�D�HD�@ D�}qD�� D�  D�=qDހ D޾�D���D�AHD߀ D߾�D���D�AHD���D�� D�HD�AHD� D�� D���D�@ D�~�D⾸D�  D�>�D�~�D�� D�  D�@ D�~�D侸D�  D�AHD�~�D徸D�HD�@ D�~�D�� D�  D�@ D�HD羸D���D�=qD�~�D�� D�  D�AHD�HD�� D�  D�AHD� D꾸D�  D�@ D�~�D뾸D�  D�@ D�}qD쾸D�  D�AHD�HD�� D���D�@ D�~�DD�HD�AHD� D�� D�  D�>�D�� D��HD�  D�>�D� D�� D�  D�@ D� D�� D�  D�>�D�~�D�D�  D�@ D� D���D�  D�=qD�}qD�� D�HD�AHD�� D�� D���D�>�D�� D�� D���D�=qD�~�D�� D�HD�@ D�~�D�� D�  D�@ D�l�G�O�>�?.{?aG�?��?��
?�p�?�G�?�@��@z�@&ff@333@E�@Q�@aG�@k�@}p�@��@�=q@�z�@���@��\@���@��@���@�  @�=q@�\)@ٙ�@�G�@���@�33@�Q�A�A�A
�HA{A33A�A�A   A#33A(��A+�A1G�A5�A:�HA>�RAC33AHQ�AL(�AQG�AU�AZ=qA^{Ac33Ag
=Al(�Ao\)As�
Aw�A{�A�Q�A��A�z�A�{A���A�=qA���A�ffA���A��HA�z�A�
=A���A��HA�p�A�
=A���A�33A�A��A��A�(�A�{A�Q�A�=qA���A�ffA���A��HA��A�\)A�G�AÅA�p�A�  Aə�A��
A�{AϮA�=qA��
A�{A�Q�A�=qA�z�A�{A�Q�A��HA�(�A�RA���A��HA��A�\)A���A�A��A�\)A��A��A�p�A��B ��B{B�HB(�B��B=qB
=BQ�B	�B
=qB�BQ�Bp�B�\B�B��Bp�B�HB�B��B�B�RB  B��B{B33B  BG�B=qB\)B z�B!p�B"�RB#�B$��B%B&�RB(  B(��B*{B+
=B,  B-G�B.{B/\)B0Q�B1G�B2�\B3�B4z�B5B6�\B7�B8��B9B;
=B<  B<��B>=qB?
=B@Q�BAG�BBffBC�BDz�BEBFffBG�BH��BIBJ�HBK�BL��BM�BN�HBP  BP��BR{BR�HBS�
BT��BUBW
=BW�
BX��BY�BZ�\B[�B\��B]G�B^ffB_33B`Q�BaG�Bb=qBc\)BdQ�BeG�BfffBg33Bhz�BiG�BjffBk�BlQ�Bm��BnffBo�
Bp��Bq�Bs
=Bt  Bu�BvffBw�Bx��By�B{
=B|Q�B}��B~�\B�
B�z�B���B�B�Q�B���B���B�{B���B�\)B��B�ffB��B��B�(�B���B�p�B�  B���B��B�B�ffB���B�p�B�(�B���B�\)B�{B��RB�p�B�  B��RB��B�{B���B���B�=qB��HB���B�ffB���B�B��\B��B��
B���B�G�B��B���B�p�B�(�B���B���B�Q�B���B��B�z�B�
=B��
B���B�G�B��
B���B�\)B��
B��\B�33B��B�  B�z�B���B�33B�p�B��B��B�{B�(�B�Q�B�z�B�z�B��\B��RB���B���B�
=B��B��B�G�B�p�B��B���B��B��B�{B�{B�Q�B�ffB�z�B��\B��HB���B�
=B�33B�p�B��B���B��B�  B�{B�=qB��\B���B��RB���B��B��B�\)B��B���B��B��B�{B�{B�=qB�ffB��\B��\B��RB��HB��HB���B��B�G�B�G�B�\)B��B���B���B��B��
B��
B��
B��B�{B�  B�{B�(�B�=qB�(�B�=qB�ffB�Q�B�Q�B�ffB�z�B�z�B�z�B�z�B���B��\B�z�B���B��RB��RB���B��RB���B��HB���B���B��HB���B���B���B�
=B�33B�G�B�33B�G�B�p�B���B��B��B��
B�  B�=qB�=qB�z�B���B���B��HB�
=B�\)B��B���B��
B�{B�Q�B�ffB��\B��HB�
=B�G�B�p�B���B��
B�=qB�z�B���B��HB��B��B��B�(�B�ffB��RB��B�p�B�B�  B�=qB���B��B�p�B��B�  B�ffB���B�33B�p�B�B�(�Bģ�B���B�\)Bř�B�  B�ffB��HB�33BǙ�B��B�=qBȸRB�33Bə�B�  B�Q�B���B�G�B�B�(�B�z�B���B�p�B��
B�ffB��HB�\)B�B�(�BУ�B�33B�B�=qBң�B��Bә�B�  B�z�B�
=Bՙ�B�{B�z�B��HB�\)B��
B�ffB��HB�\)B��
B�Q�BڸRB�33Bۙ�B�(�Bܣ�B�33Bݙ�B�  B�ffB���B�p�B��B�ffB�RB��BᙚB�  B�\B�
=B�p�B��
B�(�B��B��B噚B�  B�ffB��HB�33B�B�{B�\B�
=B�B�  B�ffB��HB�\)B��
B�=qB�RB�33B��B�=qB�RB�33B�B�(�B��B�
=B�B��B�ffB���B�\)B��
B�ffB��HB�\)B��
B�=qB��RB�33B���B�{B�z�B���B�\)B��
B�Q�B���B�G�B��
B�=qB��RB�33B��B�=qB��RB�G�B��C {C Q�C �\C ��C  C=qCz�C�RC��C(�CffC�C�C(�CffC�C��C33Cz�C�RC  C=qC�C��C{CQ�C��C�HC(�CffC�RC��C=qC�CC	
=C	Q�C	��C	�HC
�C
p�C
�RC
=CG�C��C�HC(�Cz�CC
=CQ�C��C�
C(�Cp�C�C��C=qC�C�
C�Cp�C�RC
=CQ�C��C�HC�Cp�C�RC  CG�C��C�C=qC�C�
C�Cp�C�RC  CQ�C��C�C33C�C�
C(�Cz�C�
C�Cz�C��C�Cp�CC{C\)C�RC
=CffC�RC
=CffC�RC
=C\)C�C  CQ�C��C��C G�C ��C �C!G�C!��C!��C"G�C"��C"��C#Q�C#��C#��C$Q�C$��C$��C%G�C%��C%�C&G�C&��C&�C'=qC'��C'�C(=qC(�\C(�C)=qC)��C)�C*G�C*��C*�C+=qC+�\C+�C,=qC,�\C,�HC-33C-�C-�
C.33C.z�C.��C/(�C/p�C/��C0{C0ffC0�RC1{C1ffC1�RC2  C2\)C2�C3  C3Q�C3��C3��C4=qC4�\C4�HC5(�C5�C5�
C6�C6p�C6�RC7
=C7Q�C7��C7�C8=qC8�\C8�
C9(�C9z�C9C:
=C:Q�C:��C:��C;33C;z�C;��C<{C<\)C<��C<�C==qC=�C=��C>
=C>\)C>��C>��C?=qC?�C?��C@{C@ffC@�C@��CAG�CA�\CA�
CB(�CBp�CB�RCC
=CCQ�CC��CC�CD=qCD�CD�
CE�CEp�CE�RCF  CFQ�CF��CF�CG33CGz�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�o@�vG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7A�+A�A�A�7A�7A�PA�l�A�ZA�\)A�VA�M�A�S�A�ZA�O�A��A�"�A�A���A���A��A��A��A��mA��mA��`A��TA��HA��`A��TA��TA��TA��/A�
=A�C�A��yA��TA�ƨA���AҬA�VA�ȴA�n�A�O�A�Aŗ�A�9XA£�A���A�x�A�C�A���A���A�&�A���A���A�dZA�`BA�VA���A���A�O�A��!A��+A�%A|ȴAu�Aq��Am`BAg�PAd��A^9XAYƨAU��ASx�ARVAP�`AN��AMoAHJAF��ADbNAAoA?7LA>=qA<jA85?A5�wA2A//A.�/A.v�A.bNA.A-�A.jA/VA/�A/oA.�/A/XA/�#A0-A.��A*�9A)�-A)XA(r�A&ffA&z�A'��A'�A'XA'A'�A'��A'��A&��A%l�A$n�A#�A#�-A"�A!�-A!\)A!
=A ffA�TA+AVA��A�AhsA�uA��AVA�RA9XA��A�^A\)A�A��AJA��Ax�AA�A5?AƨAl�A?}AVA��A��A�A1Ap�A%A�9Av�A-A�A�FA�PA+A�Av�A$�A�
A��At�A+A�HA�uAv�AffA�A�A�PA�AdZAS�A�HAI�A  A�wAdZAA
n�A
{A	A	t�A	dZA	O�A	oA�yA��A~�An�AffAn�AM�At�A?}AO�A�AQ�A(�A1A�A�Ap�Ap�Ap�A33A�9A=qA�A�#AXA�A�`AjA�A��AhsAO�A ��A 9XA {@���@���@�5?@���@�p�@�z�@��P@�@���@���@�~�@�J@��^@�G�@�%@��9@�ƨ@��!@�{@�@�`B@��@���@��@�j@�\)@�^5@�@���@�hs@�?}@���@�l�@�S�@�@홚@�j@�b@���@�P@�+@�~�@��@�x�@�&�@��`@�u@�9X@��@�^5@��@���@���@�-@�?}@��@�Q�@�b@�@�V@��@�?}@��@�1'@��;@�S�@ޗ�@�{@ݲ-@�/@��`@��`@�Ĝ@�A�@�ƨ@�t�@�dZ@���@ٺ^@���@�j@�Q�@�9X@��@׮@�S�@��@�ff@ՙ�@�  @�C�@���@���@��@щ7@��@�r�@�9X@���@�;d@�"�@�o@��y@Χ�@��@���@̬@�z�@�j@�Z@�1'@��@�v�@�J@�@�x�@�?}@��@�Ĝ@�  @ǅ@�S�@�o@���@��@�x�@�/@���@���@�r�@þw@���@�ff@�J@��@���@�x�@�`B@���@��j@��@�9X@�S�@��@��\@�v�@�ff@�V@�E�@��@��-@�G�@���@���@�z�@�Q�@�Q�@�A�@�9X@�b@���@�+@�v�@��@��-@���@��m@�S�@�ȴ@��R@���@��+@�^5@�=q@��h@�Ĝ@��D@� �@��@��w@�t�@�;d@���@�X@�G�@��`@�bN@�A�@�  @���@��F@��@�33@��H@���@�5?@�@�x�@�/@���@�  @�dZ@��@�5?@��@�{@�@���@�O�@�/@���@��u@��D@��@�Q�@��@�ȴ@�^5@�{@���@���@��#@��7@���@��u@�Z@��@�t�@�33@��y@��!@�~�@���@���@�I�@�b@��w@�t�@��@�@��@���@���@���@�v�@�M�@���@�?}@���@�j@�  @�
=@��\@�5?@�-@�-@��@��T@��-@�hs@��`@�r�@�b@���@�S�@�@���@�ff@�=q@�$�@�@��@���@���@�O�@�V@��`@���@�j@��m@�;d@�v�@�-@���@��7@�hs@�`B@�`B@�G�@��D@��@��w@�dZ@�"�@���@�n�@���@�O�@��@�%@���@���@��
@�S�@���@���@���@��+@�v�@�V@��@��^@��@�X@�%@��j@�A�@��@�ƨ@�l�@��@��y@���@��R@���@�~�@�$�@��@���@���@�x�@�X@���@��@�A�@��@��
@���@�+@���@��y@���@���@�V@�=q@��@��^@�x�@�`B@�O�@�7L@��@�%@���@��@��`@���@��D@�Z@�9X@�b@�;@�w@~�y@}��@}p�@|��@|�/@|�j@|z�@{��@z��@y�^@y�7@y7L@x�u@xQ�@xA�@x �@w��@w|�@vff@u�@u�-@up�@u?}@t�D@t(�@sƨ@sC�@r~�@rJ@p��@o��@n��@n�y@n�@nff@n{@mO�@l�/@l��@l�j@l�D@l(�@k�m@k��@k��@k��@kC�@j�!@jn�@j=q@jJ@i�@i��@i%@hbN@g�@g|�@g;d@g
=@f�@f�R@f��@f5?@e@e��@e/@dz�@c�@co@b��@bJ@a�@`Ĝ@`A�@`b@_�@_��@_�w@_�@_;d@^E�@^5?@^{@]�T@]��@]��@]p�@]/@\�/@\j@[��@[C�@Z�!@Z^5@ZM�@Z-@Y�7@Y%@X��@XĜ@XĜ@X�u@Xr�@XQ�@W�P@WK�@W�@V�R@Vv�@VV@V5?@U�h@U�@T��@T�j@T�D@Tj@Tj@T(�@Sƨ@SS�@R�@R�!@R�\@R~�@RM�@R�@Q�@Q�^@Q�7@Q7L@PĜ@PQ�@Pb@O�@O�w@O|�@OK�@O�@N�y@N�@N��@N��@Nff@M�-@Mp�@M`B@L��@L��@L�j@L�j@Lj@K�m@Kƨ@Kt�@K@J�H@J��@J��@J�\@JM�@I��@I%@I%@H��@H��@HĜ@Hr�@HA�@G�;@G�P@F�@Fv�@F$�@E@E�@Ep�@E`B@E�@D9X@C��@C�F@Ct�@CC�@B��@A��@A�^@A&�@@r�@?�@?�P@?\)@?;d@?+@>��@>v�@=�T@=�@=/@<�j@<�@<�D@;�
@;dZ@;"�@:��@:�!@:��@:=q@:�@:�@9��@9�@8��@8Q�@7�;@7�w@7�P@7�P@7\)@7+@6��@6�R@65?@5�@5��@5p�@5?}@5�@4�/@4�j@4j@3��@3C�@2�!@2~�@2~�@2^5@2-@1�#@1��@1X@1%@0Ĝ@0A�@/�w@/��@/|�@/K�@/+@/�@.ȴ@.��@.v�@.v�@.E�@.@-�@-�h@-?}@-�@-V@,�@,�j@,��@,I�@+�
@+S�@*�@*��@*��@*�!@*��@*��@*�\@*�\@*n�@*^5@*M�@*�@)��@)hs@)X@)G�@(bN@'��@'��@'�w@'�@'l�@';d@';d@'+@'
=@&��@&��@%�@%O�@%�@$�@$�j@$�D@$9X@#�m@#�F@#�F@#dZ@#o@"�@"�\@!�@!��@!�7@!7L@!%@ �`@ ��@ 1'@ 1'@�@�w@K�@�y@��@��@v�@V@@�@j@Z@Z@I�@(�@1@�
@�F@��@t�@33@@�!@J@�7@&�@%@Ĝ@��@�u@r�@bN@A�@b@  @�;@�@l�@K�@
=@�+@5?@$�@�@��@�j@�j@�j@�@��@�D@Z@�m@��@�@S�@C�@�@��@�\@~�@^5@-@�#@��@hs@hs@7L@��@Ĝ@�9@��@��@��@��@�9@�u@�@�@bN@b@  @  @�@�@�@��@;d@�y@��@v�@E�@E�@5?@{@A�7A�+A�+A�PA�+A�DA�7A�+A�+A�A�7A�A�DA�~�A�+A�A�7A�A�+A�+A�7A�+A�+A�7A�A�PA�DA�7A�PA�7A�hA�7A�~�A�`BA�O�A�\)A�Q�A�XA�bNA�\)A�XA�ffA�\)A�Q�A�XA�Q�A�\)A�S�A�K�A�A�A�C�A�E�A�O�A�S�A�Q�A�VA�VA�ZA�S�A�S�A�ZA�^5A�^5A�XA�bNA�^5A�VA�O�A�^5A�M�A�S�A�Q�A�S�A�7LA�bA��A�JA�A��A��A�(�A�"�A�(�A�$�A�+A� �A� �A� �A� �A�"�A�oA���A���A�  A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��mA��A��`A��yA��yA��mA��A��`A��A��mA��`A��mA��TA��yA��TA��mA��mA��HA��yA��TA��yA��mA��`A��mA��HA��mA��HA��HA��`A��;A��HA��;A��/A��TA��;A��TA��mA��HA��`A��`A��HA��`A��`A��TA��mA��HA��mA��HA��TA��`A��HA��mA��HA��`A��`A��HA��`A��HA��`A��TA��TA��`A��HA��`A��TA��HA��`A��HA��`A��;A��;A��/A��A��/A��#A��#A��/A���A��
A���A�x�A�O�A��A⟾A�33A�A�Aߴ9A޼jA���Aݲ-A�p�A�\)A�/A�JA�ĜAܕ�A�9XA���AۑhAڋDAٝ�A��A�C�AׁAփA�9XA��AԮA���A���A�ĜAө�A�|�A�p�A�=qA�&�A�1A��A��A��
A�ȴA���AҴ9Aҧ�Aң�Aҧ�Aң�Aҧ�Aҥ�Aҙ�Aҗ�AҋDA�-Aџ�A�S�A��Aϝ�AΡ�A�^5A�7LA��TA�1Aʏ\A�?}A���Aɕ�A�\)A��;A�r�A�"�AǶFA�XA��A���A��Aƴ9A�r�A� �A���A���Aŝ�A�ffA�7LA���Aě�A�Q�A��A���AÑhA�dZA� �A��HA�VA��A��-A�n�A���A�VA�v�A�+A���A���A��A��A��A��`A��`A��mA��HA��/A��HA��;A��A���A���A�ȴA��jA��A���A���A��hA��PA��7A��A�z�A�v�A�t�A�p�A�n�A�bNA�C�A�&�A�{A���A��wA��7A�
=A��A��9A���A�z�A�\)A��A�  A��TA��jA�`BA���A�S�A��PA�A���A�I�A�(�A��A�JA���A���A��TA���A���A���A���A�ȴA���A���A��jA��^A��^A��RA��FA��FA��RA��FA���A��A��!A���A��A���A���A���A���A���A���A��\A��PA��DA��+A�~�A�z�A��A�z�A�r�A�n�A�p�A�VA��A���A�|�A�;dA�A��TA���A�ƨA��FA���A�v�A�ffA�Q�A�?}A�$�A���A���A��PA��A���A�O�A�-A���A��/A���A���A�I�A�ȴA�bA���A��jA�"�A��^A�5?A�{A��`A�ĜA��A�-A��FA�$�A� �A�dZA�{A��A���A�
=A�1A�hsA�A��A���A�p�A�?}A�M�A�G�A�O�A�  A���A���A�ffA�C�A��A��A��#A���A��FA���A��7A�r�A�M�A�-A�
=A��A��/A�ȴA��FA��hA�dZA��A��mA��
A���A�ĜA���A�?}A���A��!A�&�A��jA�G�A��9A�Q�A���A��A���A���A��\A��A��hA��A�bNA�7LA��A�A��A���A��wA���A��hA��7A��7A�x�A�dZA�33A��RA�jA�JA��wA�M�A�  A��#A�K�A��FA�VA��FA�K�A��yA���A��hA�p�A�Q�A�=qA��A��HA���A���A�bNA�7LA���A���A�G�A�ĜA�hsA���A���A�A�A�S�A��`A��7A�^5A��A��`A��A��+A���A�ƨA���A�r�A�AXA}��A|jA{�Az�uAzbAx�DAwG�Avz�AvJAu�-AuXAt��At9XAt$�AsƨAs\)Ar�HAr-Aq|�Aq�Ap9XAo�#Ao�PAo?}An�HAnv�AmAm&�Al��Al1Ak?}Ai�wAh�9Ag��AgdZAgVAf�yAf��Af�RAfz�Af5?AeƨAe&�Ad�Ad�Ac�7Ac�Ab�\Aa��A` �A^�RA]�A\ĜA\VA\JA[�A[�PAZ��AZVAY�wAYVAX^5AW�AW"�AV�DAU�mAUdZAU&�AT�AT�!ATjAS��AS�^ASl�AS;dASAR�HAR��AR�AR��ARz�ARM�AR�AQ��AQ�AQXAQ/AP��AP�jAP��APr�APVAOAN��AN�AN��ANȴAN��ANI�AN�ANJAMƨAM�AK�AK+AJ{AH��AG�TAGƨAG�AG��AG�7AGp�AG33AF�AF��AFr�AFffAF �AE��AE33AD��AD=qAC|�AC%ABz�AA�;AA�A@�HA@��A@  A?t�A?XA?K�A??}A?�A>��A>��A>��A>n�A>E�A>�A=�mA=dZA="�A=
=A<��A<��A;XA:��A:9XA9K�A8�A7G�A6��A6ZA6A�A6$�A5�
A5�7A5O�A5VA4��A3�wA2�A0ĜA/�A/�A/C�A/33A/+A/+A/&�A/�A/A.��A.�yA.�A.��A.��A.~�A.n�A.z�A.�A.z�A.z�A.�A.�A.z�A.M�A.�A.jA.ffA.M�A.�A-�#A-��A-��A-��A-A-�
A-�A-�A.JA.=qA.M�A.M�A.Q�A.ZA.�DA.�RA.�/A/%A/oA/�A/�A/�A/�A/oA/�A/�A/�A/�A/�A/"�A/"�A/"�A/�A/VA/A.�A.�`A.�A.��A.��A.��A.�yA/
=A/+A/7LA/K�A/XA/hsA/x�A/�7A/��A/��A/�wA/�mA01A0{A0�A0�A0 �A0-A01'A09XA0=qA0=qA01A/�^A/`BA.E�A-33A,ĜA,$�A+?}A*��A*ZA*{A)�TA)��A)��A)��A)�FG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                   A�7A�+A�A�A�7A�7A�PA�l�A�ZA�\)A�VA�M�A�S�A�ZA�O�A��A�"�A�A���A���A��A��A��A��mA��mA��`A��TA��HA��`A��TA��TA��TA��/A�
=A�C�A��yA��TA�ƨA���AҬA�VA�ȴA�n�A�O�A�Aŗ�A�9XA£�A���A�x�A�C�A���A���A�&�A���A���A�dZA�`BA�VA���A���A�O�A��!A��+A�%A|ȴAu�Aq��Am`BAg�PAd��A^9XAYƨAU��ASx�ARVAP�`AN��AMoAHJAF��ADbNAAoA?7LA>=qA<jA85?A5�wA2A//A.�/A.v�A.bNA.A-�A.jA/VA/�A/oA.�/A/XA/�#A0-A.��A*�9A)�-A)XA(r�A&ffA&z�A'��A'�A'XA'A'�A'��A'��A&��A%l�A$n�A#�A#�-A"�A!�-A!\)A!
=A ffA�TA+AVA��A�AhsA�uA��AVA�RA9XA��A�^A\)A�A��AJA��Ax�AA�A5?AƨAl�A?}AVA��A��A�A1Ap�A%A�9Av�A-A�A�FA�PA+A�Av�A$�A�
A��At�A+A�HA�uAv�AffA�A�A�PA�AdZAS�A�HAI�A  A�wAdZAA
n�A
{A	A	t�A	dZA	O�A	oA�yA��A~�An�AffAn�AM�At�A?}AO�A�AQ�A(�A1A�A�Ap�Ap�Ap�A33A�9A=qA�A�#AXA�A�`AjA�A��AhsAO�A ��A 9XA {@���@���@�5?@���@�p�@�z�@��P@�@���@���@�~�@�J@��^@�G�@�%@��9@�ƨ@��!@�{@�@�`B@��@���@��@�j@�\)@�^5@�@���@�hs@�?}@���@�l�@�S�@�@홚@�j@�b@���@�P@�+@�~�@��@�x�@�&�@��`@�u@�9X@��@�^5@��@���@���@�-@�?}@��@�Q�@�b@�@�V@��@�?}@��@�1'@��;@�S�@ޗ�@�{@ݲ-@�/@��`@��`@�Ĝ@�A�@�ƨ@�t�@�dZ@���@ٺ^@���@�j@�Q�@�9X@��@׮@�S�@��@�ff@ՙ�@�  @�C�@���@���@��@щ7@��@�r�@�9X@���@�;d@�"�@�o@��y@Χ�@��@���@̬@�z�@�j@�Z@�1'@��@�v�@�J@�@�x�@�?}@��@�Ĝ@�  @ǅ@�S�@�o@���@��@�x�@�/@���@���@�r�@þw@���@�ff@�J@��@���@�x�@�`B@���@��j@��@�9X@�S�@��@��\@�v�@�ff@�V@�E�@��@��-@�G�@���@���@�z�@�Q�@�Q�@�A�@�9X@�b@���@�+@�v�@��@��-@���@��m@�S�@�ȴ@��R@���@��+@�^5@�=q@��h@�Ĝ@��D@� �@��@��w@�t�@�;d@���@�X@�G�@��`@�bN@�A�@�  @���@��F@��@�33@��H@���@�5?@�@�x�@�/@���@�  @�dZ@��@�5?@��@�{@�@���@�O�@�/@���@��u@��D@��@�Q�@��@�ȴ@�^5@�{@���@���@��#@��7@���@��u@�Z@��@�t�@�33@��y@��!@�~�@���@���@�I�@�b@��w@�t�@��@�@��@���@���@���@�v�@�M�@���@�?}@���@�j@�  @�
=@��\@�5?@�-@�-@��@��T@��-@�hs@��`@�r�@�b@���@�S�@�@���@�ff@�=q@�$�@�@��@���@���@�O�@�V@��`@���@�j@��m@�;d@�v�@�-@���@��7@�hs@�`B@�`B@�G�@��D@��@��w@�dZ@�"�@���@�n�@���@�O�@��@�%@���@���@��
@�S�@���@���@���@��+@�v�@�V@��@��^@��@�X@�%@��j@�A�@��@�ƨ@�l�@��@��y@���@��R@���@�~�@�$�@��@���@���@�x�@�X@���@��@�A�@��@��
@���@�+@���@��y@���@���@�V@�=q@��@��^@�x�@�`B@�O�@�7L@��@�%@���@��@��`@���@��D@�Z@�9X@�b@�;@�w@~�y@}��@}p�@|��@|�/@|�j@|z�@{��@z��@y�^@y�7@y7L@x�u@xQ�@xA�@x �@w��@w|�@vff@u�@u�-@up�@u?}@t�D@t(�@sƨ@sC�@r~�@rJ@p��@o��@n��@n�y@n�@nff@n{@mO�@l�/@l��@l�j@l�D@l(�@k�m@k��@k��@k��@kC�@j�!@jn�@j=q@jJ@i�@i��@i%@hbN@g�@g|�@g;d@g
=@f�@f�R@f��@f5?@e@e��@e/@dz�@c�@co@b��@bJ@a�@`Ĝ@`A�@`b@_�@_��@_�w@_�@_;d@^E�@^5?@^{@]�T@]��@]��@]p�@]/@\�/@\j@[��@[C�@Z�!@Z^5@ZM�@Z-@Y�7@Y%@X��@XĜ@XĜ@X�u@Xr�@XQ�@W�P@WK�@W�@V�R@Vv�@VV@V5?@U�h@U�@T��@T�j@T�D@Tj@Tj@T(�@Sƨ@SS�@R�@R�!@R�\@R~�@RM�@R�@Q�@Q�^@Q�7@Q7L@PĜ@PQ�@Pb@O�@O�w@O|�@OK�@O�@N�y@N�@N��@N��@Nff@M�-@Mp�@M`B@L��@L��@L�j@L�j@Lj@K�m@Kƨ@Kt�@K@J�H@J��@J��@J�\@JM�@I��@I%@I%@H��@H��@HĜ@Hr�@HA�@G�;@G�P@F�@Fv�@F$�@E@E�@Ep�@E`B@E�@D9X@C��@C�F@Ct�@CC�@B��@A��@A�^@A&�@@r�@?�@?�P@?\)@?;d@?+@>��@>v�@=�T@=�@=/@<�j@<�@<�D@;�
@;dZ@;"�@:��@:�!@:��@:=q@:�@:�@9��@9�@8��@8Q�@7�;@7�w@7�P@7�P@7\)@7+@6��@6�R@65?@5�@5��@5p�@5?}@5�@4�/@4�j@4j@3��@3C�@2�!@2~�@2~�@2^5@2-@1�#@1��@1X@1%@0Ĝ@0A�@/�w@/��@/|�@/K�@/+@/�@.ȴ@.��@.v�@.v�@.E�@.@-�@-�h@-?}@-�@-V@,�@,�j@,��@,I�@+�
@+S�@*�@*��@*��@*�!@*��@*��@*�\@*�\@*n�@*^5@*M�@*�@)��@)hs@)X@)G�@(bN@'��@'��@'�w@'�@'l�@';d@';d@'+@'
=@&��@&��@%�@%O�@%�@$�@$�j@$�D@$9X@#�m@#�F@#�F@#dZ@#o@"�@"�\@!�@!��@!�7@!7L@!%@ �`@ ��@ 1'@ 1'@�@�w@K�@�y@��@��@v�@V@@�@j@Z@Z@I�@(�@1@�
@�F@��@t�@33@@�!@J@�7@&�@%@Ĝ@��@�u@r�@bN@A�@b@  @�;@�@l�@K�@
=@�+@5?@$�@�@��@�j@�j@�j@�@��@�D@Z@�m@��@�@S�@C�@�@��@�\@~�@^5@-@�#@��@hs@hs@7L@��@Ĝ@�9@��@��@��@��@�9@�u@�@�@bN@b@  @  @�@�@�@��@;d@�y@��@v�@E�@E�@5?@{G�O�A�7A�+A�+A�PA�+A�DA�7A�+A�+A�A�7A�A�DA�~�A�+A�A�7A�A�+A�+A�7A�+A�+A�7A�A�PA�DA�7A�PA�7A�hA�7A�~�A�`BA�O�A�\)A�Q�A�XA�bNA�\)A�XA�ffA�\)A�Q�A�XA�Q�A�\)A�S�A�K�A�A�A�C�A�E�A�O�A�S�A�Q�A�VA�VA�ZA�S�A�S�A�ZA�^5A�^5A�XA�bNA�^5A�VA�O�A�^5A�M�A�S�A�Q�A�S�A�7LA�bA��A�JA�A��A��A�(�A�"�A�(�A�$�A�+A� �A� �A� �A� �A�"�A�oA���A���A�  A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��mA��A��`A��yA��yA��mA��A��`A��A��mA��`A��mA��TA��yA��TA��mA��mA��HA��yA��TA��yA��mA��`A��mA��HA��mA��HA��HA��`A��;A��HA��;A��/A��TA��;A��TA��mA��HA��`A��`A��HA��`A��`A��TA��mA��HA��mA��HA��TA��`A��HA��mA��HA��`A��`A��HA��`A��HA��`A��TA��TA��`A��HA��`A��TA��HA��`A��HA��`A��;A��;A��/A��A��/A��#A��#A��/A���A��
A���A�x�A�O�A��A⟾A�33A�A�Aߴ9A޼jA���Aݲ-A�p�A�\)A�/A�JA�ĜAܕ�A�9XA���AۑhAڋDAٝ�A��A�C�AׁAփA�9XA��AԮA���A���A�ĜAө�A�|�A�p�A�=qA�&�A�1A��A��A��
A�ȴA���AҴ9Aҧ�Aң�Aҧ�Aң�Aҧ�Aҥ�Aҙ�Aҗ�AҋDA�-Aџ�A�S�A��Aϝ�AΡ�A�^5A�7LA��TA�1Aʏ\A�?}A���Aɕ�A�\)A��;A�r�A�"�AǶFA�XA��A���A��Aƴ9A�r�A� �A���A���Aŝ�A�ffA�7LA���Aě�A�Q�A��A���AÑhA�dZA� �A��HA�VA��A��-A�n�A���A�VA�v�A�+A���A���A��A��A��A��`A��`A��mA��HA��/A��HA��;A��A���A���A�ȴA��jA��A���A���A��hA��PA��7A��A�z�A�v�A�t�A�p�A�n�A�bNA�C�A�&�A�{A���A��wA��7A�
=A��A��9A���A�z�A�\)A��A�  A��TA��jA�`BA���A�S�A��PA�A���A�I�A�(�A��A�JA���A���A��TA���A���A���A���A�ȴA���A���A��jA��^A��^A��RA��FA��FA��RA��FA���A��A��!A���A��A���A���A���A���A���A���A��\A��PA��DA��+A�~�A�z�A��A�z�A�r�A�n�A�p�A�VA��A���A�|�A�;dA�A��TA���A�ƨA��FA���A�v�A�ffA�Q�A�?}A�$�A���A���A��PA��A���A�O�A�-A���A��/A���A���A�I�A�ȴA�bA���A��jA�"�A��^A�5?A�{A��`A�ĜA��A�-A��FA�$�A� �A�dZA�{A��A���A�
=A�1A�hsA�A��A���A�p�A�?}A�M�A�G�A�O�A�  A���A���A�ffA�C�A��A��A��#A���A��FA���A��7A�r�A�M�A�-A�
=A��A��/A�ȴA��FA��hA�dZA��A��mA��
A���A�ĜA���A�?}A���A��!A�&�A��jA�G�A��9A�Q�A���A��A���A���A��\A��A��hA��A�bNA�7LA��A�A��A���A��wA���A��hA��7A��7A�x�A�dZA�33A��RA�jA�JA��wA�M�A�  A��#A�K�A��FA�VA��FA�K�A��yA���A��hA�p�A�Q�A�=qA��A��HA���A���A�bNA�7LA���A���A�G�A�ĜA�hsA���A���A�A�A�S�A��`A��7A�^5A��A��`A��A��+A���A�ƨA���A�r�A�AXA}��A|jA{�Az�uAzbAx�DAwG�Avz�AvJAu�-AuXAt��At9XAt$�AsƨAs\)Ar�HAr-Aq|�Aq�Ap9XAo�#Ao�PAo?}An�HAnv�AmAm&�Al��Al1Ak?}Ai�wAh�9Ag��AgdZAgVAf�yAf��Af�RAfz�Af5?AeƨAe&�Ad�Ad�Ac�7Ac�Ab�\Aa��A` �A^�RA]�A\ĜA\VA\JA[�A[�PAZ��AZVAY�wAYVAX^5AW�AW"�AV�DAU�mAUdZAU&�AT�AT�!ATjAS��AS�^ASl�AS;dASAR�HAR��AR�AR��ARz�ARM�AR�AQ��AQ�AQXAQ/AP��AP�jAP��APr�APVAOAN��AN�AN��ANȴAN��ANI�AN�ANJAMƨAM�AK�AK+AJ{AH��AG�TAGƨAG�AG��AG�7AGp�AG33AF�AF��AFr�AFffAF �AE��AE33AD��AD=qAC|�AC%ABz�AA�;AA�A@�HA@��A@  A?t�A?XA?K�A??}A?�A>��A>��A>��A>n�A>E�A>�A=�mA=dZA="�A=
=A<��A<��A;XA:��A:9XA9K�A8�A7G�A6��A6ZA6A�A6$�A5�
A5�7A5O�A5VA4��A3�wA2�A0ĜA/�A/�A/C�A/33A/+A/+A/&�A/�A/A.��A.�yA.�A.��A.��A.~�A.n�A.z�A.�A.z�A.z�A.�A.�A.z�A.M�A.�A.jA.ffA.M�A.�A-�#A-��A-��A-��A-A-�
A-�A-�A.JA.=qA.M�A.M�A.Q�A.ZA.�DA.�RA.�/A/%A/oA/�A/�A/�A/�A/oA/�A/�A/�A/�A/�A/"�A/"�A/"�A/�A/VA/A.�A.�`A.�A.��A.��A.��A.�yA/
=A/+A/7LA/K�A/XA/hsA/x�A/�7A/��A/��A/�wA/�mA01A0{A0�A0�A0 �A0-A01'A09XA0=qA0=qA01A/�^A/`BA.E�A-33A,ĜA,$�A+?}A*��A*ZA*{A)�TA)��A)��A)��A)�FG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                   ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B]�B]�B]�B]dB]�B]dB]dB_;B\�B\�B]�B]�B\�B]�B]�B]/B\�B`vBb�Bd�Be�Bf2Bf2Bf�Bf�Bh
Bh�BhsBi�Bi�BiyBg�BffB��B	�B	2aB	�B	�~B	��B	��B	�FB	�?B	�'B	�0B	��B	�B
\B
&�B
-wB
$�B
B
+B
�B
�B
uB
�B
#�B
z�B
�<B
�B
�'B
RTB
0!B
kB	�B	�B	�zB	� B	|�B	d&B	S�B	DgB	-wB	!bB	�B	B	�B	�B		B	 4B��B�2B��B��B�B�GB�.B�AB�(B�`B�2B��B�B	 iB	JB	&LB	:�B	B[B	D�B	E�B	^�B	uZB	��B	��B	��B	��B	��B	��B	��B	�-B	�NB	�B	�ZB	�cB
JB
*�B
)�B
8�B
4nB
7�B
5?B
8B
<B
;dB
;�B
>�B
A�B
C�B
I�B
E�B
D�B
C�B
C�B
K^B
IB
I�B
JXB
L�B
K�B
L�B
M�B
L�B
N�B
NB
MjB
NB
N�B
O�B
S&B
VB
U�B
T�B
TaB
S�B
S[B
Q�B
S�B
R�B
S&B
S�B
S�B
S&B
RTB
Q�B
S�B
TaB
S�B
Q�B
R�B
R B
R�B
S&B
R�B
R�B
Q�B
P}B
P�B
R B
R�B
R B
T,B
U�B
UgB
V�B
P�B
O�B
PHB
O�B
OvB
O�B
N�B
O�B
OB
OBB
P�B
Q�B
Q�B
R B
QNB
P�B
Q�B
R�B
S[B
R�B
MB
N�B
O�B
IB
H�B
G�B
G�B
H�B
H�B
I�B
J�B
LdB
LdB
J�B
I�B
H�B
H�B
F�B
E�B
GB
DgB
D3B
B�B
A�B
C-B
AUB
?B
?�B
?B
=qB
<�B
<�B
=<B
<6B
:^B
9�B
9XB
9�B
8�B
8�B
8B
7B
6�B
7�B
6FB
5?B
4nB
4nB
3�B
2�B
2aB
1�B
3�B
2-B
0�B
0�B
/�B
/�B
1'B
-wB
-B
-B
/�B
,qB
+B
*�B
*�B
*�B
+kB
*0B
)�B
)*B
(XB
($B
'�B
(�B
'RB
&�B
%�B
%zB
%zB
%�B
%�B
$@B
$@B
&�B
#:B
#�B
#�B
"�B
!�B
"4B
"�B
"4B
!bB
!-B
!-B
 \B
�B
 �B
 �B
 �B
�B
 \B
 �B
!-B
 'B
�B
B
�B
�B
�B
B
qB
�B
�B
IB
qB
	B
�B
�B
�B
�B
�B
CB
~B
CB
B
�B
�B
=B
xB
B
kB
�B
B
�B
�B
CB
	B
kB
B
�B
�B
�B
�B
7B
�B
YB
�B
�B
�B
�B
+B
�B
�B
_B
+B
+B
�B
$B
�B
�B
�B
MB
B
B
B
B
MB
FB
{B
{B
FB
{B
FB
�B
B
�B
�B
�B
�B
{B
{B
FB
�B
B
�B
@B
�B
 B
hB
4B
�B
(B
\B
�B
�B
�B
�B
�B
�B
�B
oB
oB
:B
B
:B
�B
B
oB
oB
uB
uB
uB
B
�B
�B
�B
{B
{B
{B
�B
�B
�B
$B
�B
�B
1B
7B
7B
	B
�B
�B
IB
B
B
IB
�B
�B
xB
B
B
xB
B
B
�B
qB
=B
qB
�B
xB
CB
�B
IB
~B
~B
�B
�B
�B
�B
�B
�B
 'B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!-B
 �B
!�B
#B
#nB
#�B
$�B
&�B
&�B
'B
&�B
&�B
&�B
&�B
&LB
&�B
&�B
'�B
'�B
(�B
(�B
)�B
+6B
+kB
+�B
+�B
,B
,B
,=B
,qB
-wB
-wB
-wB
-�B
-wB
.�B
.�B
/OB
/OB
/�B
/�B
/OB
.�B
.�B
.}B
0UB
/OB
0UB
0�B
0UB
0�B
1[B
1'B
1�B
1�B
1�B
1�B
1[B
49B
3�B
4�B
4�B
4�B
4�B
4�B
5?B
6FB
7�B
7�B
7�B
8RB
8�B
:*B
:^B
:�B
<B
<6B
<�B
<jB
<�B
<�B
<�B
>BB
=�B
=�B
>wB
=�B
>B
?�B
?�B
@OB
@�B
@OB
@�B
B'B
A�B
A�B
A�B
B[B
B�B
B[B
B�B
C�B
C�B
C�B
C�B
C�B
D3B
C�B
D3B
D3B
C�B
C�B
EB
D�B
D�B
E9B
EB
D�B
F?B
FtB
F�B
F�B
F�B
FtB
F�B
G�B
HB
IRB
H�B
IB
I�B
I�B
I�B
I�B
I�B
I�B
K�B
K)B
K)B
K)B
K)B
L0B
K�B
K�B
K�B
L�B
L�B
N<B
N<B
OB
N<B
NpB
N�B
N�B
O�B
OBB
OBB
OBB
OBB
O�B
O�B
PB
PB
O�B
PHB
PHB
PHB
P}B
PHB
PB
P}B
P�B
Q�B
Q�B
Q�B
Q�B
R B
R B
Q�B
Q�B
RTB
RTB
Q�B
RTB
R�B
S�B
S[B
S�B
T�B
U2B
T�B
U�B
U�B
U�B
U�B
UgB
UgB
V�B
W
B
V�B
W
B
V�B
V�B
W
B
V�B
W?B
W
B
W�B
XyB
XEB
YB
X�B
X�B
XyB
Y�B
Y�B
YB
YB
YB
Y�B
Y�B
YB
Z�B
ZB
ZQB
Z�B
ZQB
ZQB
ZQB
[�B
[#B
[#B
[�B
[WB
Z�B
[WB
[#B
[�B
[�B
\)B
[�B
[�B
[�B
\)B
\)B
\)B
\]B
\)B
\�B
]dB
]/B
]�B
]dB
]�B
^B
^5B
^5B
^5B
^5B
^�B
^5B
^�B
_pB
_;B
_;B
`B
_�B
_pB
_;B
`B
`�B
`B
`�B
`�B
`�B
`�B
`�B
`vB
aB
a�B
bB
a�B
a�B
a�B
bB
bNB
bNB
b�B
b�B
c B
c B
cTB
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
e�B
f2B
e�B
f�B
gB
g�B
g�B
h
B
g�B
g�B
h
B
hsB
iB
iDB
iyB
i�B
i�B
i�B
j�B
j�B
j�B
kQB
kB
kB
k�B
k�B
kQB
k�B
l�B
lWB
l�B
m]B
m]B
m�B
m�B
ncB
n�B
n�B
n�B
oiB
oiB
o�B
o�B
o�B
o�B
pB
o�B
p;B
qB
qB
q�B
qvB
q�B
q�B
q�B
q�B
rB
rGB
r|B
rB
sB
sMB
s�B
s�B
s�B
s�B
s�B
tB
tB
tB
tB
t�B
t�B
t�B
t�B
t�B
uZB
t�B
uZB
uZB
uZB
u�B
v+B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
v�B
w2B
v�B
x�B
x8B
x8B
xlB
xlB
x�B
y	B
y>B
yrB
y�B
y�B
zDB
zDB
{B
z�B
z�B
z�B
zxB
{JB
{�B
{�B
{B
{�B
|B
{�B
|�B
}VB
}"B
|�B
}VB
}�B
}"B
}�B
~�B
}�B
~�B
~�B
~�B
.B
�4B
cB
�B
� B
� B
�4B
�B
�oB
�B
�oB
�B
�;B
�B
�AB
�B
��B
�uB
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�SB
��B
�SB
�B
�SB
��B
�YB
��B
�_B
��B
��B
��B
�fB
�fB
��B
��B
��B
�1B
��B
��B
��B
��B
��B
��B
�1B
�1B
�fB
�B
�B
��B
��B
��B
�lB
�	B
�=B
�=B
��B
��B
�=B
�DB
��B
�B
�=B
�=B
��B
�B
��B
��B
�JB
�~B
�JB
�~B
��B
�JB
��B
��B
�B
��B
��B
�PB
��B
�"B
�VB
��B\�B^�B]�B]dB]dB]dB]�B^B]/B^jB\]B^jB[�B_;B\�B]�B\�B^�B\�B]�B_pB]�B]�B\�B^jB\�B]dB]/B\]B^jB\�B^�B\)BZ�B`vBZ�B\)B^�BaB[WB\�BZQB]�B]�B[�B^5B[#B^B\�B^�B_�B[�B\)BaB]dBY�B^jB[�B^B]�B[#B\�B]�B`�B\)B_pBZ�B_�B^jB^�B\)B\�B^�Bd�B_;B\�B_;BaHBX�B[�B[WB]dB[�B\�B[�B]�B\�B[�B]dB\�B[�B^5B`�B`BBa�Ba�B`�BcTBaBc�Bb�Bb�BdZBb�Bd�Bc�Bd�Be`Bc�Bd�Be�Bd&Be�Be,Be,Bf�Bf2Bd�BgBe`Be�BgmBe,Bg8Be�BgBe�Bf�BffBf2BgBe,BgmBe�Be�Bg8Be�Bh
Be�Bf2BgBe�BgmBd�Bf�BgBe�Bh
Be�Bg�Bf�Bf�BhsBf�BiDBh
BiBiBhsBjBgmBiBh�BgBiDBg�Bg�BiBg8Bh�Bh
Bg�BjBiDBiyBj�BiyBi�BjBiBkBh�BjKBiyBh�BjKBh
BjBi�BiBjBh�Bj�Bh�BiDBiyBh
BiyBgmBgmBg�Bf�Bh>Bf2Bg�Bf�Bf�Bg�Be�Be�Be�Bd&Bf�Bc�Bd�ByrB|�B��B��B�B�PB	 iB	($B	�B	�B	�B	�B	�B	_B	�B	�B	$@B	!�B	=B	g8B	m]B	~(B	�xB	�LB	�kB	�B	�+B	��B	��B	��B	��B	��B	��B	��B	�B	�kB	�+B	��B	�qB	�7B	�qB	�eB	�+B	��B	�	B	�+B	�=B	��B	�B	��B	�xB	��B	��B	�jB	�CB	��B	��B	�0B	�9B	��B	��B	��B	�XB	�$B	�B	��B	�?B	�B	��B	�NB	��B	ԕB	��B	רB	خB	�/B	��B	��B	�B	�cB	�MB	��B	�>B	��B
�B
�B
�B
�B
+B
�B
 'B
&�B
1'B
.}B
*0B
.IB
6zB
EB
6�B
+�B
&�B
%zB
%zB
$tB
$�B
&B
$�B
$B
%�B
&�B
$�B
%B
&�B
&�B
&�B
%FB
&�B
&�B
)*B
'�B
&B
$�B
"�B
#�B
$B
!�B
!�B
!�B
�B
 �B
"hB
!�B
�B
"4B
&B
)*B
%B
7B
%�B
�B
�B
xB
!bB
B
�B
1B
�B
�B
"hB
�B
�B
�B
�B
bB
�B
�B
�B
bB
B
�B
B
�B
uB
�B
�B
:B
�B
�B
B
B
�B
FB
oB
�B
B
�B
oB
B
@B
:B
�B
�B
oB
�B
�B
hB
�B
�B
�B
�B
�B

	B
�B
JB
�B
�B
�B
	�B
�B
�B
�B
�B
~B
B
�B
�B
IB
 �B
�B
qB
=B
xB
~B
YB
!-B
,=B
L0B
C�B
#B
�B
!�B
'�B
'�B
�B	��B
�B
.�B
($B
	B
1B
�B
�B
B

�B
hB
B
	�B
�B
.�B
 �B
�B

	B
7LB
U2B
"�B
�B
B
#B
�B
/OB
�B
	B
SB

rB
#:B
!B
B
�B
YB
�B
�B
�B
\B
.B
�B
�B
\B
:B
�B
�B
(B
"B
"B
B
(B
4B
7B
B
�B
�B
�B
!�B
-�B
/�B
5?B
;�B
GB
^5B
cB
w�B
�rB
�B
��B
��B
��B
�4B
�B
ɆB
�#B
͟B
ȀB
�?B
�EB
��B
�aB
��B
��B
�jB
�*B
�qB
��B
�B
��B
�B
�hB
��B
��B
�AB
{�B
�"B
{�B
d�B
Z�B
V�B
L�B
:�B
3hB
9�B
7�B
5�B
5�B
8B
0�B
0�B
1�B
)�B
�B
&B
.}B
B
$B
�B
T�B
*0B	�B	�%B	�/B	��B	�5B	��B
�B	�B	�B	�?B	�2B	҉B	�vB	�jB	�B	�]B	��B	�*B	��B	��B	�XB	��B	��B	�!B	�tB	�$B	�$B	��B	�eB	��B	�_B	�CB	�~B	��B	��B	�MB	�B	��B	|B	.B	�;B	w2B	xlB	v+B	x�B	��B	n/B	m�B	d�B	^�B	Z�B	X�B	WsB	Z�B	XEB	Z�B	WsB	W
B	T�B	NpB	FtB	NB	N�B	[�B	RTB	I�B	7B	3�B	-�B	'�B	2-B	0�B	,=B	.�B	*�B	,�B	(�B	&�B	'RB	)*B	"4B	�B	B	CB	�B	!B	�B	�B	�B	1B	�B	SB	�B	uB	�B	FB	�B	�B	�B	B	B	B	�B	(B	�B	�B	7B	�B	�B	�B	{B	%B	�B	oB�]B	�B	PB	�B	:B	�B	�B��B�8B��B�+B�B�B��B�8B�	B��B�;B�xB��B�ZB�B�PB�fB�+B�B��B��B��B�B	B�/B�B��B�"B�cB��B��B��B��B��B�B�B�%B�yB�8B�>B�;B	
=B�B��B��B	
�B	B��B�oB��B�B�+B�B�B�|B��B	�B	�B	MB��B��B�8B�B�ZB�ZB�ZB�B�ZB�,B�ZB�,B�B�B�mB�`B�TB�KB�"B�B�WB�;B��B�B�B�JB�(B	 iB	�B	AB��B��B	B	�B	fB	�B	oB	�B	�B	#B	$tB	&�B	%zB	#nB	6B	6�B	0UB	<�B	=qB	?HB	A B	B'B	C�B	B'B	B[B	B�B	B[B	A�B	CaB	C�B	D�B	EB	D3B	E�B	D�B	D�B	DgB	C�B	EB	DgB	B�B	MjB	V9B	ZQB	]/B	_�B	c�B	ffB	jB	k�B	poB	m�B	s�B	{�B	�B	�{B	�1B	�DB	�PB	�4B	�B	��B	��B	�FB	��B	�B	��B	�VB	��B	�MB	��B	�rB	��B	��B	�{B	��B	��B	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                   B]�B]�B]�B]OB]�B]�B^�B_�B\�B]FB]�B]�B\�B]�B^�B]B]sB`�Bb�Bd�Be�Bf4BfQBf�Bf�BhBh�BhdBi�Bi�Bi�Bh�BmCB�.B	!B	MB	�B	�,B	��B	�|B	��B	�0B	�|B	��B	�B	�AB
B
5�B
8*B
@oB
@uB
h�B
I�B
+�B
>B
B
5�B
�VB
��B
ˤB
��B
_�B
K�B
;�B	�LB	�bB	�:B	�yB	�mB	onB	h�B	SYB	:�B	(�B	�B	B	hB	�B	�B	HB�B	B	 #B��B�B	 DB	%B��B	�B�LB�B�`B��B	 �B	
�B	$BB	:�B	B�B	E'B	DtB	\�B	t�B	�*B	��B	��B	�CB	�B	�YB	�OB	� B	�B	��B	�UB	��B
B
+�B
-B
=~B
7�B
9~B
6iB
;�B
?6B
<�B
=&B
@�B
C�B
FgB
L�B
G�B
E<B
DGB
F�B
N�B
K(B
J�B
LB
M{B
L�B
NB
N�B
NkB
P�B
N�B
N�B
O�B
PB
QjB
T�B
W3B
VB
U�B
T�B
T-B
T�B
S�B
U�B
TB
TQB
T�B
T�B
T	B
S$B
R�B
UB
VB
TbB
R�B
T
B
SB
SB
T/B
S�B
S�B
Q�B
P�B
Q�B
S�B
SB
R_B
T�B
V+B
W%B
X�B
RB
P�B
Q�B
QTB
Q{B
Q3B
PB
P�B
O\B
O�B
Q�B
R B
RZB
R�B
Q�B
P�B
Q~B
SiB
VLB
S�B
L�B
O�B
R B
I�B
I3B
H^B
H�B
IPB
H�B
I�B
K�B
N)B
NB
KUB
J�B
J}B
I{B
G�B
G�B
H�B
E�B
D�B
CB
C�B
EIB
A�B
?�B
A�B
@B
=�B
=�B
>�B
>�B
=-B
:�B
9�B
9�B
:�B
9�B
9�B
8�B
7�B
8�B
9�B
7gB
5�B
5*B
4�B
4.B
3KB
3B
4 B
5B
2�B
1qB
1�B
0lB
1�B
2B
-�B
-�B
/�B
1�B
-%B
+PB
+�B
+aB
,B
,sB
+B
*/B
)�B
(�B
(�B
(�B
*�B
( B
&�B
%�B
%�B
&ZB
&�B
&|B
$�B
&6B
'�B
$B
$�B
$�B
#vB
"�B
#FB
#�B
#)B
"1B
"B
!�B
 mB
 B
!�B
!�B
!�B
 >B
!�B
"�B
"�B
!3B
+B
YB
xB
7B
�B
�B
�B
;B
�B
�B
B
�B
�B
�B
B
�B
`B
B
rB
}B
;B
5B
.B
XB
�B
�B
�B
�B
/B
CB
�B
nB
�B
B
�B
;B
XB
KB
4B
B
B
�B
�B
.B
�B
$B
�B
UB
bB
�B
�B
BB
�B
sB
|B
AB
�B
B
�B
�B
�B
�B
B
�B
�B
�B
nB
�B
�B
8B
�B
�B
B
B
�B
�B
�B
cB
?B
�B
�B
�B
�B
�B
B
�B
�B
 B
�B
�B
�B
�B
�B
B
=B
%B
8B
�B
�B
�B
�B
�B
?B
�B
2B
cB
�B
�B
qB
�B
EB
�B
B

B
SB
VB
B
"B
�B
BB
3B
CB
^B
qB
!B
�B
�B
�B
gB
�B
�B
�B
�B
�B
SB
�B
QB
�B
BB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
-B
TB
!]B
�B
 8B
 +B
 �B
!nB
 �B
!!B
!:B
!"B
!2B
![B
!�B
!�B
#_B
#�B
$@B
$�B
&�B
'�B
'_B
'4B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(~B
(�B
)}B
)tB
*�B
+�B
+�B
,	B
+�B
,0B
,RB
,�B
-B
-�B
-�B
.B
.TB
.�B
/�B
0NB
/�B
0 B
0B
/�B
/dB
.�B
.�B
/�B
16B
0B
1B
1B
1B
1�B
2�B
1�B
2B
1�B
1�B
2MB
2�B
5-B
4�B
4�B
4�B
5B
5B
5&B
5�B
6�B
7�B
7�B
8�B
8�B
9�B
:�B
:�B
;~B
<�B
<�B
=B
<�B
<�B
=B
=TB
>�B
>B
>:B
>�B
>3B
?B
@uB
@6B
@�B
@�B
@�B
A�B
B�B
A�B
A�B
BgB
B�B
B�B
B�B
C�B
DB
D1B
C�B
C�B
D3B
DeB
D$B
DHB
DKB
D;B
D}B
EeB
EB
E&B
E|B
E9B
E�B
GEB
F�B
GB
GB
F�B
F�B
G�B
HxB
IB
I�B
IB
I�B
J0B
I�B
I�B
I�B
JOB
J�B
LB
KqB
KmB
KhB
K�B
L�B
L/B
L�B
L�B
MSB
M�B
OMB
OB
O$B
N]B
N�B
O7B
OdB
PB
OZB
OZB
OzB
O�B
P#B
PB
P)B
PB
PB
P�B
P�B
PB
P�B
PsB
PlB
QB
Q�B
R,B
Q�B
R/B
R#B
RTB
RDB
RB
RB
R�B
R�B
RhB
SB
S�B
T6B
S�B
T�B
UyB
U�B
U�B
VB
U�B
U�B
U�B
U�B
U�B
W�B
W$B
V�B
W9B
V�B
W
B
W=B
WB
W�B
W�B
XhB
X�B
X�B
YdB
X�B
X�B
YB
Z3B
ZB
Y�B
Y�B
Y�B
ZB
ZB
Z;B
Z�B
ZXB
Z�B
Z�B
ZxB
Z�B
Z�B
[�B
[OB
[jB
[�B
[yB
Z�B
[�B
[�B
\0B
[�B
\hB
\B
\B
\*B
\^B
\_B
\_B
\�B
\�B
]lB
]�B
]tB
]�B
]�B
^B
^6B
^iB
^hB
^MB
^hB
^�B
^vB
_zB
_�B
_WB
_�B
`?B
_�B
_|B
_�B
`�B
`�B
`jB
aB
aB
`�B
`�B
`�B
`�B
a�B
b~B
b!B
a�B
a�B
a�B
bgB
b�B
b�B
b�B
c1B
c�B
cwB
c�B
c�B
dB
c�B
dB
d�B
eB
eB
e	B
e6B
e�B
f4B
f}B
fgB
gLB
g�B
h B
g�B
h,B
g�B
hB
h�B
iB
isB
i�B
i�B
i�B
i�B
j`B
k"B
k/B
k:B
ktB
k9B
k|B
k�B
k�B
k�B
l}B
l�B
l�B
mcB
m�B
m�B
m�B
m�B
n�B
n�B
n�B
oLB
o�B
o�B
o�B
o�B
o�B
pB
p0B
p1B
qB
qiB
q�B
q�B
qB
q�B
q�B
r B
r#B
rZB
r�B
r�B
r�B
s�B
ssB
s�B
s�B
tB
tB
t
B
tQB
tBB
t)B
tVB
t�B
t�B
t�B
u@B
uB
umB
uB
u�B
u�B
u�B
vB
v�B
v�B
v�B
v�B
v�B
v�B
wB
v�B
v�B
v�B
wB
v�B
w3B
wRB
w�B
v�B
wVB
wvB
y#B
xBB
xMB
x�B
x�B
x�B
yB
ySB
y�B
y�B
zBB
z�B
z�B
{HB
z�B
z�B
{B
z�B
{�B
{�B
{�B
{�B
|B
|EB
|RB
}ZB
}�B
}LB
}CB
}�B
}�B
}kB
~-B
~�B
~9B
~�B
B
(B
~B
�8B
�B
�B
�XB
��B
�@B
�B
�uB
�B
��B
�,B
�oB
�/B
�dB
�5B
�B
��B
�B
��B
�hB
�GB
�B
�-B
�AB
��B
��B
��B
�B
��B
�B
�yB
�UB
��B
��B
��B
�AB
��B
�B
�;B
��B
�zB
�iB
�B
��B
��B
�DB
��B
�B
�B
��B
��B
��B
��B
�fB
��B
�B
�(B
��B
��B
�B
��B
�B
�rB
��B
�B
��B
�SB
�EB
��B
�B
�8B
�_B
��B
�B
��B
�-B
�[B
��B
�bB
��B
��B
�eB
�EB
�B
�nB
��B
�B
�VB
��B
�MB
�fG�O�B\�B^�B]�B]dB]dB]dB]�B^B]/B^jB\]B^jB[�B_;B\�B]�B\�B^�B\�B]�B_pB]�B]�B\�B^jB\�B]dB]/B\]B^jB\�B^�B\)BZ�B`vBZ�B\)B^�BaB[WB\�BZQB]�B]�B[�B^5B[#B^B\�B^�B_�B[�B\)BaB]dBY�B^jB[�B^B]�B[#B\�B]�B`�B\)B_pBZ�B_�B^jB^�B\)B\�B^�Bd�B_;B\�B_;BaHBX�B[�B[WB]dB[�B\�B[�B]�B\�B[�B]dB\�B[�B^5B`�B`BBa�Ba�B`�BcTBaBc�Bb�Bb�BdZBb�Bd�Bc�Bd�Be`Bc�Bd�Be�Bd&Be�Be,Be,Bf�Bf2Bd�BgBe`Be�BgmBe,Bg8Be�BgBe�Bf�BffBf2BgBe,BgmBe�Be�Bg8Be�Bh
Be�Bf2BgBe�BgmBd�Bf�BgBe�Bh
Be�Bg�Bf�Bf�BhsBf�BiDBh
BiBiBhsBjBgmBiBh�BgBiDBg�Bg�BiBg8Bh�Bh
Bg�BjBiDBiyBj�BiyBi�BjBiBkBh�BjKBiyBh�BjKBh
BjBi�BiBjBh�Bj�Bh�BiDBiyBh
BiyBgmBgmBg�Bf�Bh>Bf2Bg�Bf�Bf�Bg�Be�Be�Be�Bd&Bf�Bc�Bd�ByrB|�B��B��B�B�PB	 iB	($B	�B	�B	�B	�B	�B	_B	�B	�B	$@B	!�B	=B	g8B	m]B	~(B	�xB	�LB	�kB	�B	�+B	��B	��B	��B	��B	��B	��B	��B	�B	�kB	�+B	��B	�qB	�7B	�qB	�eB	�+B	��B	�	B	�+B	�=B	��B	�B	��B	�xB	��B	��B	�jB	�CB	��B	��B	�0B	�9B	��B	��B	��B	�XB	�$B	�B	��B	�?B	�B	��B	�NB	��B	ԕB	��B	רB	خB	�/B	��B	��B	�B	�cB	�MB	��B	�>B	��B
�B
�B
�B
�B
+B
�B
 'B
&�B
1'B
.}B
*0B
.IB
6zB
EB
6�B
+�B
&�B
%zB
%zB
$tB
$�B
&B
$�B
$B
%�B
&�B
$�B
%B
&�B
&�B
&�B
%FB
&�B
&�B
)*B
'�B
&B
$�B
"�B
#�B
$B
!�B
!�B
!�B
�B
 �B
"hB
!�B
�B
"4B
&B
)*B
%B
7B
%�B
�B
�B
xB
!bB
B
�B
1B
�B
�B
"hB
�B
�B
�B
�B
bB
�B
�B
�B
bB
B
�B
B
�B
uB
�B
�B
:B
�B
�B
B
B
�B
FB
oB
�B
B
�B
oB
B
@B
:B
�B
�B
oB
�B
�B
hB
�B
�B
�B
�B
�B

	B
�B
JB
�B
�B
�B
	�B
�B
�B
�B
�B
~B
B
�B
�B
IB
 �B
�B
qB
=B
xB
~B
YB
!-B
,=B
L0B
C�B
#B
�B
!�B
'�B
'�B
�B	��B
�B
.�B
($B
	B
1B
�B
�B
B

�B
hB
B
	�B
�B
.�B
 �B
�B

	B
7LB
U2B
"�B
�B
B
#B
�B
/OB
�B
	B
SB

rB
#:B
!B
B
�B
YB
�B
�B
�B
\B
.B
�B
�B
\B
:B
�B
�B
(B
"B
"B
B
(B
4B
7B
B
�B
�B
�B
!�B
-�B
/�B
5?B
;�B
GB
^5B
cB
w�B
�rB
�B
��B
��B
��B
�4B
�B
ɆB
�#B
͟B
ȀB
�?B
�EB
��B
�aB
��B
��B
�jB
�*B
�qB
��B
�B
��B
�B
�hB
��B
��B
�AB
{�B
�"B
{�B
d�B
Z�B
V�B
L�B
:�B
3hB
9�B
7�B
5�B
5�B
8B
0�B
0�B
1�B
)�B
�B
&B
.}B
B
$B
�B
T�B
*0B	�B	�%B	�/B	��B	�5B	��B
�B	�B	�B	�?B	�2B	҉B	�vB	�jB	�B	�]B	��B	�*B	��B	��B	�XB	��B	��B	�!B	�tB	�$B	�$B	��B	�eB	��B	�_B	�CB	�~B	��B	��B	�MB	�B	��B	|B	.B	�;B	w2B	xlB	v+B	x�B	��B	n/B	m�B	d�B	^�B	Z�B	X�B	WsB	Z�B	XEB	Z�B	WsB	W
B	T�B	NpB	FtB	NB	N�B	[�B	RTB	I�B	7B	3�B	-�B	'�B	2-B	0�B	,=B	.�B	*�B	,�B	(�B	&�B	'RB	)*B	"4B	�B	B	CB	�B	!B	�B	�B	�B	1B	�B	SB	�B	uB	�B	FB	�B	�B	�B	B	B	B	�B	(B	�B	�B	7B	�B	�B	�B	{B	%B	�B	oB�]B	�B	PB	�B	:B	�B	�B��B�8B��B�+B�B�B��B�8B�	B��B�;B�xB��B�ZB�B�PB�fB�+B�B��B��B��B�B	B�/B�B��B�"B�cB��B��B��B��B��B�B�B�%B�yB�8B�>B�;B	
=B�B��B��B	
�B	B��B�oB��B�B�+B�B�B�|B��B	�B	�B	MB��B��B�8B�B�ZB�ZB�ZB�B�ZB�,B�ZB�,B�B�B�mB�`B�TB�KB�"B�B�WB�;B��B�B�B�JB�(B	 iB	�B	AB��B��B	B	�B	fB	�B	oB	�B	�B	#B	$tB	&�B	%zB	#nB	6B	6�B	0UB	<�B	=qB	?HB	A B	B'B	C�B	B'B	B[B	B�B	B[B	A�B	CaB	C�B	D�B	EB	D3B	E�B	D�B	D�B	DgB	C�B	EB	DgB	B�B	MjB	V9B	ZQB	]/B	_�B	c�B	ffB	jB	k�B	poB	m�B	s�B	{�B	�B	�{B	�1B	�DB	�PB	�4B	�B	��B	��B	�FB	��B	�B	��B	�VB	��B	�MB	��B	�rB	��B	��B	�{B	��B	��B	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�m�<g�<ڣ�<�^�<#�
<#�
<#�
<��a<���<Y��<$�H<#�
<#�
<3��<��K<B-N<��.=+5?=��=D�=<r2<#�
<<��<�-:<#�
<#�
<N4W<��5<m=!<� t=*<�9�<��<n��<yp_<�̸<I��<�+�<�<i�<#�
<#�
<#�
<#�
<#�
<�<#�
<#�
<Dc�<#�
<#�
<#�
<q�/<))<]�N<%�=<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<c}4<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2019112003431620191120034316IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019113003004720191130030047QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019113003004720191130030047QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020010906573220200109065732IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                