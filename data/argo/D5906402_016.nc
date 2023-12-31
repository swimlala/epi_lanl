CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-04-21T01:20:20Z creation; 2022-04-26T16:06:57Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�        =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  Z(   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�        ap   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  ~�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�        ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        �`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H &   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       -P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H Jp   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       Q�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` n�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   o8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   u8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   {8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210421012020  20220426232404  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_016                 8138_008904_016                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @�n��@��@�n��@��11  @�n��4֡@�n��4֡@-0�d��8@-0�d��8�dAg��	l�dAg��	l11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@:�H@�  @�  @�  @�  A   A\)A   A,(�A@  A^�RA\)A�  A�Q�A�  A�  AϮA�  A�  B (�BQ�B  B�B�
B'�
B0(�B8  B?�
BG�
BP  BXQ�B`(�Bh(�Bp(�Bx(�B�
B�  B�(�B�{B�{B�{B�{B�  B�  B��B��B�  B��B�  B�{B�  B�{B�{B�  B�  BЏ\BӅB��
B��B�  B��B��B��B�  B�{B�  B��
B��C  C  C  C
=C
{C  C��C  C
=C
=C
=C
=C
=C  C��C��C!��C#��C%��C(  C*  C,
=C-��C0  C2  C4  C6  C8
=C:
=C<
=C>  C?��CB  CD
=CF
=CH
=CJ
=CL
=CN
=CO��CQ��CT  CV  CX  CZ  C\  C^  C`
=Cb
=Cd  Ce��Cg��Ci��Cl  Cn  Cp  Cq��Cs��Cv  Cx  Cy��C{��C~  C�  C�  C���C���C�  C�  C�C�C�C�C�C�C���C���C�  C�C�  C���C�  C�
=C�
=C�C�C�C�C�  C�  C�  C�  C���C�  C�C�  C���C�  C�C�C�C���C���C�  C�  C���C�  C�  C�C�  C���C���C���C���C���C���C���C�  C�C�C���C���C���C���C���C���C�  C���C���C���C���C�  C���C���C�  C�  C���C���C���C�  C�C�  C�C�C�  C���C�  C�  C�C���C���C���C�  C�  C���C�  C�  C�  C�C�  C�  C�  C�  C�  C�C�C�  C�  C�  C���C���C�C�C���C���C�  C�  C���C�  C�  C�C�C�  C�  C�  C�  C�  C�  C�C���C���D   D z�D  D� D�qD� D�qD}qD  D� D�qD� D  D� D  D��D�Dz�D�qD	}qD
  D
� D  D}qD�qD��D  D��DD��D  D}qD�D� D  D��D�D� D�qD}qD  D�DD�D  Dz�D��D}qD�qD� D  D��D  D}qD  D}qD��D}qD�D��D  D}qD  D� D   D � D ��D!}qD"  D"� D#�D#��D$  D$� D$�qD%��D&�D&� D'  D'}qD'�qD(}qD(�qD)}qD)�qD*� D+�D+� D+��D,� D-�D-}qD-��D.}qD/�D/� D/�qD0� D1�D1}qD2  D2�D3�D3��D4  D4� D5�D5�D6�D6� D7  D7}qD7�qD8}qD9  D9�D:D:�D;D;� D;��D<}qD<�qD=}qD=�qD>� D?�D?� D?��D@}qD@�qDA� DB�DB� DB�qDC� DD�DD��DE  DE� DE��DF��DGDG� DH  DH��DI�DI}qDI��DJ}qDK�DK� DL  DL��DM  DM� DN  DN}qDO  DO��DO�qDP� DQ�DQ��DRDR� DR�qDS}qDT  DT��DU�DU� DV�DV� DV�qDW� DX  DX��DY�DY��DZ  DZ� D[�D[}qD\  D\� D]  D]� D^�D^��D^�qD_}qD`  D`� D`�qDa}qDa�qDb}qDc  Dc� Dd�Dd�De  De}qDe�qDf� Dg  Dg}qDh  Dh��Di  Di}qDi�qDj}qDj�qDk}qDk�qDl� Dm�Dm��Dn  Dn}qDo  Do� Dp�Dp��Dq  Dq� Dr  Dr� Ds  Ds��Ds�qDt}qDu  Duz�Dv  Dv��Dw�Dw� Dx�Dx��DyDy�DzDz�D{  D{z�D|  D|��D}  D}��D~�D~��D�D}qD�  D�B�D���D�� D�  D�AHD�� D�� D�HD�>�D�~�D���D�  D�B�D���D�� D���D�=qD�� D�D�  D�@ D�� D��qD�  D�B�D�� D���D�  D�AHD��HD��HD�  D�@ D�~�D���D���D�>�D��HD��HD�HD�@ D�� D��HD�HD�=qD�~�D��HD�  D�@ D��HD�� D�  D�AHD�� D�� D�  D�AHD��HD���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�� D��qD�>�D��HD��HD�  D�@ D�� D�� D��qD�@ D�� D�� D���D�@ D��HD���D�  D�AHD�� D���D���D�=qD�� D�� D���D�AHD��HD��HD�  D�@ D�� D��HD�  D�=qD�~�D�� D�HD�AHD�� D�� D��D�AHD�~�D��HD��D�@ D�}qD�� D�HD�@ D�~�D��HD�  D�=qD�� D�� D�HD�AHD��HD���D�  D�B�D��HD�� D�HD�@ D�~�D�� D�  D�@ D�� D���D�HD�AHD�� D��HD���D�>�D�� D��HD�  D�AHD�~�D���D�  D�@ D�� D��HD�  D�>�D�~�D�� D�HD�>�D�~�D�� D���D�AHD��HD��HD�HD�AHD�~�D�� D�HD�AHD���D�� D���D�@ D��HD�� D�  D�@ D�� D���D�  D�AHD�� D�� D�HD�@ D�� D��HD�HD�>�D�~�D���D�  D�>�D�~�D��HD�HD�AHD�� D�� D�HD�@ D�� D��HD�  D�AHD�� D�� D�HD�@ D�� D��HD�HD�AHD��HD�� D���D�@ D�~�D���D���D�=qD�~�D�� D�HD�AHD��HD��HD�  D�=qD�~�D���D�HD�@ D�~�D�� D���D�>�D�� D��HD�  D�>�D�� D��HD�  D�@ D�� D�� D���D�@ D��HD�� D�  D�>�D��HD�� D���D�AHDHD¾�D�  D�AHDÀ D�� D�  D�AHDĀ Dľ�D���D�>�Dŀ D�� D�  D�>�D�~�Dƾ�D���D�@ Dǀ D��HD�HD�@ DȀ DȾ�D�  D�@ Dɀ Dɾ�D�  D�AHDʁHD�� D���D�>�D�~�D�� D��qD�>�D̀ D�� D�HD�@ D�~�D�� D�  D�>�D�~�D�� D���D�@ Dπ DϾ�D���D�@ D�~�Dо�D�HD�AHDсHD�� D���D�>�D�~�DҾ�D���D�@ DӀ D�� D�  D�>�DԀ DԾ�D���D�@ DՁHD�� D�  D�B�DցHD־�D�  D�AHD�~�D׽qD���D�@ D؁HD��HD���D�>�Dـ D��HD�  D�=qDڀ D��HD�HD�@ D�~�D�� D��D�B�D܁HD�D��D�AHD݂�D�D�HD�B�DށHD��HD�HD�AHD߁HD��HD�  D�=qD�}qD�qD��qD�@ D� D��HD�HD�AHD�HD⾸D���D�>�D�}qD㾸D���D�>�D�}qD�qD��qD�>�D�}qD��HD�HD�>�D�~�D澸D��qD�@ D�p�?�?#�
?u?�=q?���?���?�G�@   @\)@!G�@(��@=p�@L��@Y��@n{@z�H@��@���@�@��R@��@�{@�
=@��R@�ff@�{@�@޸R@��@�{@�Q�@��RA�
AQ�A�A  Az�AQ�A��A ��A%�A)��A-p�A1G�A6ffA:=qA?\)AC33AG
=AK�AP��AS�
AXQ�A\��A`��AeAj=qAn{Aq�Aw
=A|(�A�  A��\A��A�\)A���A�(�A�{A���A�33A�p�A�\)A�=qA�z�A�ffA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ?�  ?��H@:�H@�  @�  @�  @�  A   A\)A   A,(�A@  A^�RA\)A�  A�Q�A�  A�  AϮA�  A�  B (�BQ�B  B�B�
B'�
B0(�B8  B?�
BG�
BP  BXQ�B`(�Bh(�Bp(�Bx(�B�
B�  B�(�B�{B�{B�{B�{B�  B�  B��B��B�  B��B�  B�{B�  B�{B�{B�  B�  BЏ\BӅB��
B��B�  B��B��B��B�  B�{B�  B��
B��C  C  C  C
=C
{C  C��C  C
=C
=C
=C
=C
=C  C��C��C!��C#��C%��C(  C*  C,
=C-��C0  C2  C4  C6  C8
=C:
=C<
=C>  C?��CB  CD
=CF
=CH
=CJ
=CL
=CN
=CO��CQ��CT  CV  CX  CZ  C\  C^  C`
=Cb
=Cd  Ce��Cg��Ci��Cl  Cn  Cp  Cq��Cs��Cv  Cx  Cy��C{��C~  C�  C�  C���C���C�  C�  C�C�C�C�C�C�C���C���C�  C�C�  C���C�  C�
=C�
=C�C�C�C�C�  C�  C�  C�  C���C�  C�C�  C���C�  C�C�C�C���C���C�  C�  C���C�  C�  C�C�  C���C���C���C���C���C���C���C�  C�C�C���C���C���C���C���C���C�  C���C���C���C���C�  C���C���C�  C�  C���C���C���C�  C�C�  C�C�C�  C���C�  C�  C�C���C���C���C�  C�  C���C�  C�  C�  C�C�  C�  C�  C�  C�  C�C�C�  C�  C�  C���C���C�C�C���C���C�  C�  C���C�  C�  C�C�C�  C�  C�  C�  C�  C�  C�C���C���D   D z�D  D� D�qD� D�qD}qD  D� D�qD� D  D� D  D��D�Dz�D�qD	}qD
  D
� D  D}qD�qD��D  D��DD��D  D}qD�D� D  D��D�D� D�qD}qD  D�DD�D  Dz�D��D}qD�qD� D  D��D  D}qD  D}qD��D}qD�D��D  D}qD  D� D   D � D ��D!}qD"  D"� D#�D#��D$  D$� D$�qD%��D&�D&� D'  D'}qD'�qD(}qD(�qD)}qD)�qD*� D+�D+� D+��D,� D-�D-}qD-��D.}qD/�D/� D/�qD0� D1�D1}qD2  D2�D3�D3��D4  D4� D5�D5�D6�D6� D7  D7}qD7�qD8}qD9  D9�D:D:�D;D;� D;��D<}qD<�qD=}qD=�qD>� D?�D?� D?��D@}qD@�qDA� DB�DB� DB�qDC� DD�DD��DE  DE� DE��DF��DGDG� DH  DH��DI�DI}qDI��DJ}qDK�DK� DL  DL��DM  DM� DN  DN}qDO  DO��DO�qDP� DQ�DQ��DRDR� DR�qDS}qDT  DT��DU�DU� DV�DV� DV�qDW� DX  DX��DY�DY��DZ  DZ� D[�D[}qD\  D\� D]  D]� D^�D^��D^�qD_}qD`  D`� D`�qDa}qDa�qDb}qDc  Dc� Dd�Dd�De  De}qDe�qDf� Dg  Dg}qDh  Dh��Di  Di}qDi�qDj}qDj�qDk}qDk�qDl� Dm�Dm��Dn  Dn}qDo  Do� Dp�Dp��Dq  Dq� Dr  Dr� Ds  Ds��Ds�qDt}qDu  Duz�Dv  Dv��Dw�Dw� Dx�Dx��DyDy�DzDz�D{  D{z�D|  D|��D}  D}��D~�D~��D�D}qD�  D�B�D���D�� D�  D�AHD�� D�� D�HD�>�D�~�D���D�  D�B�D���D�� D���D�=qD�� D�D�  D�@ D�� D��qD�  D�B�D�� D���D�  D�AHD��HD��HD�  D�@ D�~�D���D���D�>�D��HD��HD�HD�@ D�� D��HD�HD�=qD�~�D��HD�  D�@ D��HD�� D�  D�AHD�� D�� D�  D�AHD��HD���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�� D��qD�>�D��HD��HD�  D�@ D�� D�� D��qD�@ D�� D�� D���D�@ D��HD���D�  D�AHD�� D���D���D�=qD�� D�� D���D�AHD��HD��HD�  D�@ D�� D��HD�  D�=qD�~�D�� D�HD�AHD�� D�� D��D�AHD�~�D��HD��D�@ D�}qD�� D�HD�@ D�~�D��HD�  D�=qD�� D�� D�HD�AHD��HD���D�  D�B�D��HD�� D�HD�@ D�~�D�� D�  D�@ D�� D���D�HD�AHD�� D��HD���D�>�D�� D��HD�  D�AHD�~�D���D�  D�@ D�� D��HD�  D�>�D�~�D�� D�HD�>�D�~�D�� D���D�AHD��HD��HD�HD�AHD�~�D�� D�HD�AHD���D�� D���D�@ D��HD�� D�  D�@ D�� D���D�  D�AHD�� D�� D�HD�@ D�� D��HD�HD�>�D�~�D���D�  D�>�D�~�D��HD�HD�AHD�� D�� D�HD�@ D�� D��HD�  D�AHD�� D�� D�HD�@ D�� D��HD�HD�AHD��HD�� D���D�@ D�~�D���D���D�=qD�~�D�� D�HD�AHD��HD��HD�  D�=qD�~�D���D�HD�@ D�~�D�� D���D�>�D�� D��HD�  D�>�D�� D��HD�  D�@ D�� D�� D���D�@ D��HD�� D�  D�>�D��HD�� D���D�AHDHD¾�D�  D�AHDÀ D�� D�  D�AHDĀ Dľ�D���D�>�Dŀ D�� D�  D�>�D�~�Dƾ�D���D�@ Dǀ D��HD�HD�@ DȀ DȾ�D�  D�@ Dɀ Dɾ�D�  D�AHDʁHD�� D���D�>�D�~�D�� D��qD�>�D̀ D�� D�HD�@ D�~�D�� D�  D�>�D�~�D�� D���D�@ Dπ DϾ�D���D�@ D�~�Dо�D�HD�AHDсHD�� D���D�>�D�~�DҾ�D���D�@ DӀ D�� D�  D�>�DԀ DԾ�D���D�@ DՁHD�� D�  D�B�DցHD־�D�  D�AHD�~�D׽qD���D�@ D؁HD��HD���D�>�Dـ D��HD�  D�=qDڀ D��HD�HD�@ D�~�D�� D��D�B�D܁HD�D��D�AHD݂�D�D�HD�B�DށHD��HD�HD�AHD߁HD��HD�  D�=qD�}qD�qD��qD�@ D� D��HD�HD�AHD�HD⾸D���D�>�D�}qD㾸D���D�>�D�}qD�qD��qD�>�D�}qD��HD�HD�>�D�~�D澸D��qD�@ G�O�?�?#�
?u?�=q?���?���?�G�@   @\)@!G�@(��@=p�@L��@Y��@n{@z�H@��@���@�@��R@��@�{@�
=@��R@�ff@�{@�@޸R@��@�{@�Q�@��RA�
AQ�A�A  Az�AQ�A��A ��A%�A)��A-p�A1G�A6ffA:=qA?\)AC33AG
=AK�AP��AS�
AXQ�A\��A`��AeAj=qAn{Aq�Aw
=A|(�A�  A��\A��A�\)A���A�(�A�{A���A�33A�p�A�\)A�=qA�z�A�ffA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AѶFAѮAѣ�Aџ�AѓuAёhAя\Aя\Aя\Aч+AсA�t�A�`BA�7LA�&�A�"�A��A�oA�JA�JA�
=A�1A�%A�A�A�  A���A���A���A���A��A���A��A���A���A���A���A��A��A���A���A���A��A��A��A��mA��HA��/A��
A�ȴAЋDA�E�A�(�A�VA��/Aϙ�A��A��A�;dA�(�A���A���A��PA���A��A�33A�  A��`A��mA�|�A��^A�(�A��7A�5?A���A��`A�XA�x�A�I�A��+A���A���A��!A���A�|�A�~�A��RA�oA�Q�A���A~M�Au��Aq�PAk��Ae��AbZA_p�AZjAW��AQ�#AO&�ALn�AIAF�HAE\)AAK�A>�\A<��A;;dA:�`A:��A:1A9G�A6�jA4�+A3�#A3�A21'A0  A-��A-A,��A, �A*�yA)�A)XA(  A'
=A%�A%7LA$��A#�A$$�A$ȴA$�9A$jA"bA!x�A ��A �A ��A �yA �`A �jA �AK�A�yA�/A�AoA�A��A$�A��A�HA��A%A;dA&�A�DAn�A�AjA`BA�Ar�A��AA��Ar�A�A�TAx�A��A��AM�A��A�wA�A�AJAƨA�A`BA�A�`A��AQ�A �A�;A��A\)A��AE�AA�#A�AXA%A��A�AbNA9XA-AbA�A�-A��At�A�A�RA��An�A9XA��AO�A�RAz�A�Al�AoA
�/A
�A
�A
r�A
jA
VA
{A	ƨA	O�A�A�\A-A��A�A|�A�AĜAn�A(�A�^A��A�AZA�A�A��A  A��AhsAC�A ��A jA -A 1@���@��
@�C�@��R@��@���@�O�@�r�@��
@�;d@��@���@���@�=q@��#@�O�@���@���@�  @��R@�ȴ@�n�@��h@�z�@�+@���@�
=@�~�@�{@��#@��@���@�P@�"�@���@�Q�@�ƨ@��@陚@���@� �@��;@睲@��H@���@�^@�@��@�l�@���@�ff@��T@��@�Q�@�  @��m@��;@ߍP@�o@ޗ�@���@�=q@�-@��@��@ۮ@��@�n�@�J@ى7@�X@��@�V@ؓu@׍P@ְ!@�@�x�@�7L@ԛ�@ҏ\@�hs@���@�Q�@ϥ�@�\)@Ώ\@�-@��#@�7L@�%@̣�@�I�@˝�@�~�@�@�p�@�V@ȴ9@�(�@��;@Ǯ@��@�n�@��#@ŉ7@���@�z�@�9X@�1'@���@Ý�@�+@�@�=q@��@��7@�V@���@��u@�(�@�ƨ@�|�@���@��\@���@�G�@��/@�r�@� �@��
@��P@�t�@�dZ@���@��+@��@��T@��-@��h@��@�hs@�X@�?}@��@���@�bN@���@���@�S�@���@���@�ff@�=q@���@�x�@�V@��9@�r�@�(�@��;@��P@�dZ@�K�@�;d@��@�ȴ@�$�@���@��^@�`B@���@��/@��@�I�@��;@���@�dZ@��R@�@���@�p�@��@��9@�bN@� �@��@��F@��P@�|�@�l�@�\)@�C�@���@���@�M�@��#@��@��@�z�@�Q�@�9X@� �@�1@��F@�;d@���@�n�@�^5@��@���@�x�@��@���@�bN@��@��F@�\)@��@��\@�V@���@���@��h@�?}@���@��D@�(�@���@��@�;d@���@�$�@��h@�?}@���@�z�@��@���@�l�@�
=@��@���@���@�^5@�J@��^@��@�/@��`@��@�z�@�Z@�Q�@�(�@���@��y@��!@�V@�$�@��@��7@�p�@�hs@�7L@��@��;@�l�@�"�@���@�E�@�@��h@�X@��@��9@��u@��D@�z�@�A�@�(�@�(�@��@���@��P@�dZ@�K�@���@��+@�-@�@��@���@��7@�/@��j@�z�@�Q�@�9X@��m@���@��P@�dZ@�;d@��@��@���@�ff@�=q@���@�O�@�V@���@�Ĝ@��@��@��@���@�A�@��F@���@�|�@�l�@�S�@�+@�
=@���@��\@�M�@��T@��h@�hs@�G�@��/@���@��@�j@� �@��P@�C�@�
=@��@��@��!@��\@�M�@��@��-@�`B@�&�@���@���@��9@���@��@�Z@�A�@�w@~ȴ@~v�@~$�@~{@~@}@}O�@|�@|9X@|1@{��@z�@z�!@z^5@y��@y�7@yX@y�@x�`@xĜ@xr�@wl�@w�@v�y@v��@vE�@u��@t�@t(�@s��@s�@sS�@s"�@r��@r-@q�#@q��@p��@p �@o�@o;d@n�@n��@nff@n$�@m�-@l��@lj@l(�@k��@k��@kS�@k@j��@j~�@j�@i�@i��@i��@i�@h�u@g�;@g��@g��@g�P@g
=@fv�@f5?@e@e?}@e�@d�/@d�D@c�m@c@b��@bn�@b=q@a��@a��@a��@ax�@aG�@a7L@`�`@`A�@_�w@_K�@^�y@^�+@^E�@]��@]`B@\�@\�j@\1@[�
@[��@[�@[t�@[33@Z�\@Y��@Y��@Y�@X�9@Xr�@X �@W�;@W|�@V��@V��@VE�@U@U�@UV@T�j@T�D@S�m@SC�@R�H@R�!@Rn�@Q�@Q��@QX@Q7L@Q7L@Q7L@P��@P�9@PA�@Ol�@Nff@M@M�@K��@K��@K"�@J�@I�#@I��@I�7@I7L@H��@HQ�@HA�@H1'@G��@F�y@F�R@F��@F��@Fff@E�@E��@EO�@EV@D�D@C�
@C��@CdZ@CS�@C33@C@B��@B�\@BM�@B=q@A��@@�`@@��@@�u@@ �@?�@?�P@?K�@?�@>ȴ@>{@=`B@=�@<��@<9X@;�
@;ƨ@;��@;C�@:�@:�\@:=q@:�@9�@9x�@9�@8��@8Q�@7��@7��@7l�@6�@65?@5�@5��@5`B@5O�@5?}@5�@4��@4�@4z�@4�@3�@3t�@3o@2�H@2^5@2M�@1��@0Ĝ@0�@/�@/�@/
=@.�+@.{@-��@-�-@-O�@-/@-�@,��@,9X@+�
@+t�@+t�@+S�@+33@*�H@*�\@*M�@*=q@*-@*�@*J@)�@)�#@)�^@)��@)7L@)�@)%@(�`@(1'@'�@'�;@'�@'\)@'+@'�@'�@'�@&�y@&��@%�T@%�-@%��@%��@%��@%p�@%/@%�@$�@$��@#�m@#��@#C�@#33@#o@"��@"~�@"n�@"^5@"M�@"=q@"�@!��@!��@!�#@!��@!�^@!�7@!x�@!hs@!G�@!&�@!%@ ��@ ��@ �9@ r�@ Q�@ 1'@ b@�;@�w@�@�P@l�@��@ff@5?@5?@{@�@��@��@p�@O�AѶFAѲ-AѸRAѲ-AѺ^AѺ^AѰ!Aѧ�Aѥ�Aѥ�Aћ�Aѣ�Aѥ�Aѣ�Aѩ�Aѩ�Aѝ�AыDAёhAѕ�Aѕ�AёhAэPAѕ�AёhAя\Aя\AыDAэPAя\AэPAя\Aя\AэPAёhAѓuAя\AыDAыDAсAхAыDA�|�A�x�A�x�A�r�A�x�A�x�A�v�A�n�A�ffA�^5A�n�A�XA�ffA�VA�ZA�hsA�;dA�A�A�9XA�5?A�33A�(�A�&�A�(�A�&�A�&�A�(�A�&�A�$�A�$�A�+A�$�A�+A�&�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        AѶFAѮAѣ�Aџ�AѓuAёhAя\Aя\Aя\Aч+AсA�t�A�`BA�7LA�&�A�"�A��A�oA�JA�JA�
=A�1A�%A�A�A�  A���A���A���A���A��A���A��A���A���A���A���A��A��A���A���A���A��A��A��A��mA��HA��/A��
A�ȴAЋDA�E�A�(�A�VA��/Aϙ�A��A��A�;dA�(�A���A���A��PA���A��A�33A�  A��`A��mA�|�A��^A�(�A��7A�5?A���A��`A�XA�x�A�I�A��+A���A���A��!A���A�|�A�~�A��RA�oA�Q�A���A~M�Au��Aq�PAk��Ae��AbZA_p�AZjAW��AQ�#AO&�ALn�AIAF�HAE\)AAK�A>�\A<��A;;dA:�`A:��A:1A9G�A6�jA4�+A3�#A3�A21'A0  A-��A-A,��A, �A*�yA)�A)XA(  A'
=A%�A%7LA$��A#�A$$�A$ȴA$�9A$jA"bA!x�A ��A �A ��A �yA �`A �jA �AK�A�yA�/A�AoA�A��A$�A��A�HA��A%A;dA&�A�DAn�A�AjA`BA�Ar�A��AA��Ar�A�A�TAx�A��A��AM�A��A�wA�A�AJAƨA�A`BA�A�`A��AQ�A �A�;A��A\)A��AE�AA�#A�AXA%A��A�AbNA9XA-AbA�A�-A��At�A�A�RA��An�A9XA��AO�A�RAz�A�Al�AoA
�/A
�A
�A
r�A
jA
VA
{A	ƨA	O�A�A�\A-A��A�A|�A�AĜAn�A(�A�^A��A�AZA�A�A��A  A��AhsAC�A ��A jA -A 1@���@��
@�C�@��R@��@���@�O�@�r�@��
@�;d@��@���@���@�=q@��#@�O�@���@���@�  @��R@�ȴ@�n�@��h@�z�@�+@���@�
=@�~�@�{@��#@��@���@�P@�"�@���@�Q�@�ƨ@��@陚@���@� �@��;@睲@��H@���@�^@�@��@�l�@���@�ff@��T@��@�Q�@�  @��m@��;@ߍP@�o@ޗ�@���@�=q@�-@��@��@ۮ@��@�n�@�J@ى7@�X@��@�V@ؓu@׍P@ְ!@�@�x�@�7L@ԛ�@ҏ\@�hs@���@�Q�@ϥ�@�\)@Ώ\@�-@��#@�7L@�%@̣�@�I�@˝�@�~�@�@�p�@�V@ȴ9@�(�@��;@Ǯ@��@�n�@��#@ŉ7@���@�z�@�9X@�1'@���@Ý�@�+@�@�=q@��@��7@�V@���@��u@�(�@�ƨ@�|�@���@��\@���@�G�@��/@�r�@� �@��
@��P@�t�@�dZ@���@��+@��@��T@��-@��h@��@�hs@�X@�?}@��@���@�bN@���@���@�S�@���@���@�ff@�=q@���@�x�@�V@��9@�r�@�(�@��;@��P@�dZ@�K�@�;d@��@�ȴ@�$�@���@��^@�`B@���@��/@��@�I�@��;@���@�dZ@��R@�@���@�p�@��@��9@�bN@� �@��@��F@��P@�|�@�l�@�\)@�C�@���@���@�M�@��#@��@��@�z�@�Q�@�9X@� �@�1@��F@�;d@���@�n�@�^5@��@���@�x�@��@���@�bN@��@��F@�\)@��@��\@�V@���@���@��h@�?}@���@��D@�(�@���@��@�;d@���@�$�@��h@�?}@���@�z�@��@���@�l�@�
=@��@���@���@�^5@�J@��^@��@�/@��`@��@�z�@�Z@�Q�@�(�@���@��y@��!@�V@�$�@��@��7@�p�@�hs@�7L@��@��;@�l�@�"�@���@�E�@�@��h@�X@��@��9@��u@��D@�z�@�A�@�(�@�(�@��@���@��P@�dZ@�K�@���@��+@�-@�@��@���@��7@�/@��j@�z�@�Q�@�9X@��m@���@��P@�dZ@�;d@��@��@���@�ff@�=q@���@�O�@�V@���@�Ĝ@��@��@��@���@�A�@��F@���@�|�@�l�@�S�@�+@�
=@���@��\@�M�@��T@��h@�hs@�G�@��/@���@��@�j@� �@��P@�C�@�
=@��@��@��!@��\@�M�@��@��-@�`B@�&�@���@���@��9@���@��@�Z@�A�@�w@~ȴ@~v�@~$�@~{@~@}@}O�@|�@|9X@|1@{��@z�@z�!@z^5@y��@y�7@yX@y�@x�`@xĜ@xr�@wl�@w�@v�y@v��@vE�@u��@t�@t(�@s��@s�@sS�@s"�@r��@r-@q�#@q��@p��@p �@o�@o;d@n�@n��@nff@n$�@m�-@l��@lj@l(�@k��@k��@kS�@k@j��@j~�@j�@i�@i��@i��@i�@h�u@g�;@g��@g��@g�P@g
=@fv�@f5?@e@e?}@e�@d�/@d�D@c�m@c@b��@bn�@b=q@a��@a��@a��@ax�@aG�@a7L@`�`@`A�@_�w@_K�@^�y@^�+@^E�@]��@]`B@\�@\�j@\1@[�
@[��@[�@[t�@[33@Z�\@Y��@Y��@Y�@X�9@Xr�@X �@W�;@W|�@V��@V��@VE�@U@U�@UV@T�j@T�D@S�m@SC�@R�H@R�!@Rn�@Q�@Q��@QX@Q7L@Q7L@Q7L@P��@P�9@PA�@Ol�@Nff@M@M�@K��@K��@K"�@J�@I�#@I��@I�7@I7L@H��@HQ�@HA�@H1'@G��@F�y@F�R@F��@F��@Fff@E�@E��@EO�@EV@D�D@C�
@C��@CdZ@CS�@C33@C@B��@B�\@BM�@B=q@A��@@�`@@��@@�u@@ �@?�@?�P@?K�@?�@>ȴ@>{@=`B@=�@<��@<9X@;�
@;ƨ@;��@;C�@:�@:�\@:=q@:�@9�@9x�@9�@8��@8Q�@7��@7��@7l�@6�@65?@5�@5��@5`B@5O�@5?}@5�@4��@4�@4z�@4�@3�@3t�@3o@2�H@2^5@2M�@1��@0Ĝ@0�@/�@/�@/
=@.�+@.{@-��@-�-@-O�@-/@-�@,��@,9X@+�
@+t�@+t�@+S�@+33@*�H@*�\@*M�@*=q@*-@*�@*J@)�@)�#@)�^@)��@)7L@)�@)%@(�`@(1'@'�@'�;@'�@'\)@'+@'�@'�@'�@&�y@&��@%�T@%�-@%��@%��@%��@%p�@%/@%�@$�@$��@#�m@#��@#C�@#33@#o@"��@"~�@"n�@"^5@"M�@"=q@"�@!��@!��@!�#@!��@!�^@!�7@!x�@!hs@!G�@!&�@!%@ ��@ ��@ �9@ r�@ Q�@ 1'@ b@�;@�w@�@�P@l�@��@ff@5?@5?@{@�@��@��@p�G�O�AѶFAѲ-AѸRAѲ-AѺ^AѺ^AѰ!Aѧ�Aѥ�Aѥ�Aћ�Aѣ�Aѥ�Aѣ�Aѩ�Aѩ�Aѝ�AыDAёhAѕ�Aѕ�AёhAэPAѕ�AёhAя\Aя\AыDAэPAя\AэPAя\Aя\AэPAёhAѓuAя\AыDAыDAсAхAыDA�|�A�x�A�x�A�r�A�x�A�x�A�v�A�n�A�ffA�^5A�n�A�XA�ffA�VA�ZA�hsA�;dA�A�A�9XA�5?A�33A�(�A�&�A�(�A�&�A�&�A�(�A�&�A�$�A�$�A�+A�$�A�+A�&�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�LB	�B	�B	�B	��B	�B	��B	��B	�B	�B	��B	��B	�FB	��B	�LB	��B	�LB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�RB	��B	��B	��B	��B	��B	�$B	�$B	�$B	�XB	�XB	��B	��B	��B	��B	��B	��B	�*B	�*B	�*B	�*B	��B	��B	��B	�@B	�B	��B	�FB	��B	�1B	�AB	�@B	��B
��B'RBR�BZ�B^5BPB\�BY�BNpBFBB�B1�B!�BkB
�]B
��B
��B�BB�B
��B
��B
�B
ѷB
��B
}�B
ffB
J�B
.IB
+�B
�B
oB	ƨB	��B	�CB	{B	ffB	\�B	A B	:*B	"�B	FB	�B	SB��B��B��B�B��B��B�DB��B�B	MB	bB	�B	B	qB	!-B	&LB	1�B	0UB	/�B	2�B	8B	?�B	EmB	Q�B	YKB	a�B	e�B	k�B	p;B	�B	�-B	��B	��B	��B	�0B	�<B	�)B	�9B	�B	�B	��B	�B	�]B	�B	�|B	��B
�B
	7B
�B
B
B
+B
xB
�B
"hB
/�B
+kB
+6B
3�B
5�B
/OB
)�B
-CB
9XB
C�B
E�B
GzB
GzB
GzB
HKB
HKB
G�B
H�B
G�B
FB
GB
F�B
DgB
E�B
HB
I�B
J#B
I�B
I�B
IB
IRB
I�B
I�B
H�B
G�B
I�B
I�B
I�B
I�B
K)B
J�B
JXB
K�B
K�B
LdB
LdB
MB
M�B
N�B
N<B
N�B
NB
MjB
L�B
K)B
J�B
IRB
F�B
E9B
FtB
IRB
G�B
GB
FtB
FtB
E�B
EmB
EB
EB
EB
C�B
B�B
AUB
?HB
?B
>�B
=�B
=�B
=qB
<B
:�B
9�B
7�B
5?B
2�B
1[B
0UB
.}B
.IB
-B
*0B
)�B
(�B
&�B
%B
%B
$tB
$@B
$tB
%FB
%B
$tB
"�B
"4B
"hB
!�B
 �B
 �B
!�B
"�B
"�B
"hB
"4B
!bB
!bB
 �B
�B
!�B
!bB
�B
IB
�B
eB
�B
�B
B
 �B
!B
xB
=B
	B
+B
B
�B
�B
PB
B
�B
"B
hB
�B
oB
hB
�B
hB
�B
B
JB
DB

�B
	�B
	lB

	B
�B
�B
B
�B
B
�B
:B
4B
�B
�B
�B
�B
~B
�B
B
�B
xB
JB
�B
	�B
	7B
�B
�B
1B
{B
MB
GB
�B
�B
{B
�B
�B
�B
B
B
B
uB
GB
�B
{B
�B
MB
�B
SB
�B
�B
B
�B
�B
SB
YB
�B
�B
�B
YB
%B
�B
�B
�B
�B
�B
�B
_B
+B
�B
+B
�B
fB
�B
	B
	7B
�B
	B
	7B
	7B
	7B
	B
�B
	�B

rB

=B

�B

�B

�B

�B

rB

rB

rB

=B

�B

�B
DB
DB
DB
DB
�B
�B
�B
�B
�B
�B
�B
�B
�B
VB
VB
"B
�B
�B
�B
�B
�B
�B
(B
�B
�B
�B
�B
�B
oB
�B
hB
B
:B
oB
�B
�B
B
B
@B
@B
�B
uB
�B
�B
uB
�B
FB
B
FB
�B
�B
�B
�B
MB
MB
MB
MB
�B
SB
�B
�B
�B
$B
$B
�B
YB
�B
�B
+B
�B
_B
_B
+B
+B
+B
+B
_B
_B
_B
�B
B
�B
B
eB
7B
kB
�B
	B
�B
qB
xB
xB
�B
�B
OB
�B
!B
�B
 'B
!�B
!�B
!bB
 \B
�B
!B
�B
�B
�B
VB
�B
�B
 �B
!�B
!�B
"�B
"hB
"hB
"hB
$tB
"�B
#:B
"hB
"�B
#B
#�B
#nB
#�B
#�B
$�B
$�B
$�B
$tB
%B
$�B
$�B
%�B
%�B
%�B
&LB
&LB
'RB
'�B
'�B
($B
(XB
(�B
(�B
(�B
)_B
)_B
)�B
)_B
*0B
*eB
*�B
*0B
*�B
+B
+B
+B
,B
+�B
-CB
-B
-CB
-CB
-�B
-�B
-wB
-wB
-CB
.IB
.�B
.}B
.�B
.�B
/B
.�B
/OB
/OB
/�B
/�B
0�B
1[B
1[B
1�B
2�B
2�B
33B
2�B
3�B
4nB
4nB
5B
4�B
5B
5B
5?B
5�B
5�B
6�B
6�B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
7LB
8�B
9$B
9$B
9XB
9$B
9$B
9XB
9�B
9�B
:*B
9�B
:�B
:�B
:^B
:�B
:�B
;0B
;�B
;dB
;dB
;dB
<B
<6B
<�B
<�B
<�B
=B
<�B
=qB
=�B
=�B
=�B
>B
>B
>BB
?HB
>�B
>�B
?�B
@OB
@�B
@�B
@�B
A B
A B
AUB
A�B
B�B
B[B
B�B
B�B
C-B
C-B
C�B
C�B
C�B
D�B
DgB
DgB
DgB
EB
E�B
FB
F?B
E�B
E�B
FtB
F�B
F�B
GzB
G�B
GzB
GzB
G�B
HKB
IRB
IB
I�B
IRB
I�B
I�B
I�B
I�B
I�B
I�B
J#B
J�B
J�B
K^B
K)B
K�B
K�B
L0B
L�B
L�B
L�B
M�B
MjB
M�B
M�B
MjB
M�B
N<B
N<B
N�B
N�B
OB
OBB
OBB
OvB
O�B
O�B
PB
PHB
P}B
P�B
P�B
P�B
QB
Q�B
R B
RTB
R�B
R�B
R�B
S�B
S[B
S�B
S[B
S[B
S[B
S�B
S�B
T,B
T�B
T�B
UgB
T,B
S�B
T�B
T�B
T�B
T�B
T�B
U2B
UgB
U�B
U�B
V9B
VB
W
B
W?B
V�B
WsB
WsB
XB
W�B
XB
XB
X�B
YKB
YKB
Y�B
YKB
YB
Y�B
Y�B
Y�B
ZB
Y�B
Z�B
[�B
[WB
[�B
[�B
[�B
\)B
\]B
\]B
\�B
]�B
^5B
^B
^�B
_;B
_�B
_�B
_;B
`BB
`B
`vB
`vB
`BB
`vB
`vB
`vB
`vB
`vB
`�B
`vB
`B
aHB
a|B
a|B
bB
bNB
bNB
b�B
c B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
e`B
e`B
e�B
ffB
f�B
f�B
g�B
h>B
h>B
h�B
h�B
h�B
h�B
iB
iDB
iDB
iDB
iDB
iDB
iB
i�B
jB
i�B
jB
jKB
jB
jKB
j�B
jKB
j�B
j�B
k�B
kB
kB
kB
l"B
k�B
k�B
lWB
l�B
l�B
l�B
l�B
lWB
l�B
m]B
n/B
m�B
m�B
m�B
m�B
m�B
ncB
n/B
ncB
n�B
o�B
o�B
p;B
pB
pB
p�B
p�B
poB
p�B
poB
p�B
qB
qAB
qB
p�B
qB
qAB
q�B
qAB
qvB
qvB
qvB
q�B
q�B
q�B
rGB
rGB
rGB
r�B
rGB
r�B
r�B
r�B
sB
r�B
sB
s�B
tB
s�B
tB
tTB
t�B
u%B
uZB
u�B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	�B	�B	��B	��B	��B	�B	��B	�RB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�XB	�nB	��B	�FB	�zB	�B	�B	��B	�XB	�tB	�FB	�B	�@B	��B	��B	�zB	�zB	��B	�FB	�zB	��B	�B	��B	�B	�B	��B	��B	�tB	��B	�LB	��B	�-B	��B	��B	�LB	��B	��B	�zB	��B	��B	�FB	�B	��B	��B	��B	��B	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        B	�uB	�IB	�5B	�XB	�	B	�6B	��B	��B	��B	��B	�bB	��B	�9B	�B	�vB	��B	��B	��B	��B	��B	��B	�B	�BB	�,B	�7B	�7B	�`B	��B	��B	�B	��B	�B	�$B	�0B	�1B	�eB	�pB	��B	��B	��B	��B	��B	�B	�QB	�RB	�]B	�TB	�5B	�PB	�CB	��B	��B	�^B	��B	��B	��B	��B	��B	ڒB
�=B:QB[�Bj�Bh�BZ�BnB`�BU�BY�BR,B5�B&�B'�B\B;B�B�B�BB�B
��B
��B
�B
�hB
��B
w�B
Z�B
3�B
53B
4B
�B	��B	�*B	�-B	��B	p�B	l.B	K{B	K�B	,�B	�B	�B	uB	]B	B	 )B��B��B��B�WB��B��B	 B	B	�B	mB	�B	(B	-!B	4�B	1\B	1�B	6]B	;,B	A�B	I�B	T�B	\�B	d5B	g�B	m�B	o�B	�7B	�pB	��B	��B	�B	�!B	��B	ʽB	�B	�B	�7B	��B	�B	�B	��B	�PB	��B
B

*B
�B
�B
\B
�B

�B
B
"�B
1B
+�B
*�B
51B
9�B
2B
*�B
,B
8�B
DbB
G!B
H�B
H�B
IRB
JEB
I[B
I�B
I�B
H�B
H�B
I�B
H�B
E�B
F�B
H�B
KB
K%B
J�B
J�B
I�B
JuB
K%B
J�B
J�B
J�B
J�B
JuB
J�B
KfB
LlB
J�B
K�B
MB
LyB
L�B
L�B
M�B
N�B
OGB
O%B
P�B
N�B
NB
MSB
L:B
L~B
KzB
I;B
F�B
H�B
KzB
I(B
G�B
GHB
GB
E�B
E�B
E~B
F*B
FlB
E�B
D�B
B�B
@�B
@�B
@B
>B
?nB
?B
=�B
<B
;�B
:�B
6�B
4�B
3�B
3�B
1B
1B
.�B
+KB
*�B
+KB
'�B
&/B
%�B
$�B
$�B
%�B
&�B
&�B
%�B
#�B
$BB
#�B
#%B
!|B
!CB
"|B
#zB
#�B
#�B
#B
"B
#wB
#�B
B
"�B
#�B
"�B
!B
B
CB
�B
�B
�B
"�B
!�B
�B
�B
�B
�B
�B
�B
cB
JB
�B
3B
B
EB
�B
B
%B
uB
�B
gB
JB
�B
2B
VB

bB
	�B

IB
�B
�B
OB
B
�B
B
.B
�B
�B
�B
'B
�B
�B
vB
�B
&B
�B
�B
B
�B

�B
�B

B
#B
]B
�B
�B
YB
�B
jB
�B
�B
bB
�B
B
1B
�B
%B
�B
qB
�B
\B
B
B
�B
�B
VB
_B
�B
�B
�B
eB
�B
?B
[B
jB
�B
�B
�B
�B
1B
LB
B
FB
�B
'B
	=B
	NB
	�B

�B

ZB
	�B
	�B

 B
	�B
	�B
	gB
	�B

�B
|B

�B
&B
 B

�B

�B

�B

�B

�B
B
�B
�B
3B
B
B
�B
"B
B
�B
iB
�B
�B
@B
}B
�B
�B
�B
YB
B
�B
vB
�B
SB
�B
�B
UB
�B
nB
�B
�B
�B
0B
�B
dB
�B
�B
OB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
2B
lB
�B
�B
B
�B
�B
�B
�B
�B
hB
bB
%B
HB
�B
�B
DB
 B
wB
�B
�B
�B
JB
4B
�B
�B
�B
�B
B
B
RB
�B
�B
{B
�B
kB
�B
�B
iB
B
nB
�B
$B
B
{B
#B
�B
B
�B
 GB
 �B
"B
"JB
"B
 �B
�B
oB
B
�B
�B
 �B
 �B
 �B
!;B
"B
"�B
"�B
"�B
# B
$B
%�B
#�B
#�B
"�B
$B
$&B
$MB
#�B
$>B
$�B
$�B
$�B
$�B
$�B
%GB
$�B
%kB
&~B
&&B
&B
&�B
'iB
(B
(]B
(XB
(mB
(�B
)qB
)�B
*B
*B
)�B
)�B
*)B
*�B
*�B
+B
*�B
*�B
+lB
+vB
,	B
,�B
,�B
.B
-�B
-�B
-�B
.B
-�B
-�B
-�B
.GB
/{B
/
B
.�B
/B
.�B
/�B
/PB
0B
/�B
0<B
0�B
1�B
1�B
1�B
2�B
3YB
3XB
3�B
3�B
4�B
5'B
4�B
5NB
5$B
5qB
5qB
5�B
6AB
6�B
7�B
7yB
7�B
7�B
7�B
7�B
7�B
7�B
8B
8PB
9�B
9�B
9�B
9tB
9KB
9�B
9�B
:B
:�B
:�B
:�B
;VB
:�B
:�B
;SB
;�B
;zB
;�B
;�B
;�B
;�B
=>B
<�B
=B
=9B
=oB
=�B
=�B
>qB
>�B
=�B
>"B
>]B
>�B
?B
?�B
?%B
?�B
@�B
@�B
AB
A<B
AIB
AlB
A�B
BB
B�B
CwB
B�B
CDB
C9B
C�B
C�B
DBB
D	B
DGB
D�B
D�B
D�B
E.B
E�B
F�B
F]B
FKB
E�B
F�B
G-B
GCB
G{B
HB
HB
G�B
HB
H�B
I`B
I�B
I�B
I�B
I�B
I�B
I�B
JB
J/B
I�B
JsB
J�B
K�B
K-B
K�B
K�B
L.B
L6B
L�B
M'B
MB
MBB
M�B
M�B
M�B
M�B
M�B
NFB
N�B
N�B
O$B
O>B
OUB
O�B
O�B
O�B
P.B
PDB
PlB
P�B
P�B
Q B
Q9B
Q%B
Q�B
RVB
R�B
R�B
R�B
S9B
SGB
S�B
SB
S�B
SfB
S�B
S�B
TB
T�B
U-B
U�B
U�B
VlB
T�B
TB
U�B
T�B
T�B
UB
T�B
U�B
U�B
U�B
U�B
V�B
V�B
W<B
WRB
V�B
W�B
W�B
XbB
XB
XjB
X�B
YqB
Y�B
Y�B
Y�B
Y{B
Y�B
Y�B
Z5B
Z6B
ZKB
Z�B
[�B
[�B
[�B
\=B
\<B
\fB
\yB
\�B
\�B
]jB
^\B
^�B
^nB
_wB
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aB
aB
`�B
`�B
a�B
a�B
a�B
bcB
bgB
biB
cB
c�B
dB
dB
dB
d�B
c�B
d0B
eB
eB
e*B
e�B
fCB
e�B
fjB
f�B
g�B
gnB
hUB
h�B
hsB
iB
iB
h�B
iaB
i�B
i�B
i�B
iUB
ivB
iB
i�B
jQB
jgB
i�B
j0B
jeB
j6B
jyB
j�B
j�B
k%B
kaB
k�B
kCB
kjB
k�B
lwB
lB
l;B
l�B
l�B
mB
l�B
l�B
l�B
mxB
n;B
npB
nB
m�B
m�B
m�B
nB
n�B
n�B
n�B
o�B
pCB
pB
p[B
pCB
pxB
q*B
p�B
p�B
p�B
p�B
qB
q8B
qLB
q;B
p�B
q.B
q�B
q�B
q`B
q�B
q�B
q�B
q�B
q�B
rB
r�B
rzB
r{B
r�B
r�B
sB
r�B
r�B
s\B
s�B
s�B
t&B
t+B
s�B
tPB
t�B
t�B
ueB
u�G�O�B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	�B	�B	��B	��B	��B	�B	��B	�RB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�XB	�nB	��B	�FB	�zB	�B	�B	��B	�XB	�tB	�FB	�B	�@B	��B	��B	�zB	�zB	��B	�FB	�zB	��B	�B	��B	�B	�B	��B	��B	�tB	��B	�LB	��B	�-B	��B	��B	�LB	��B	��B	�zB	��B	��B	�FB	�B	��B	��B	��B	��B	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�^5=@HB="v�<��<#�
<�aj<9�Q<<�[<�%�<#�
<#�
<��t<~я<#�
<#�
<d~<]<#�
<-�n<=i8<#�
<#�
<#�
<#�
<��	<�b<��<`�<�/�<�M�<#�
<*,[<Ť<ϊ�<|�<� �<�]�<O�<3"�<��U<4��<��<+� <*g�<;�<#�
<#�
<R	�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021042101202020210421012020IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021050102003620210501020036QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021050102003620210501020036QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364620220422133646IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064220220426160642IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064220220426160642IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064220220426160642IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                