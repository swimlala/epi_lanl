CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-02-20T02:31:17Z creation; 2022-04-26T16:06:56Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210220023117  20220426232402  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL               
   
AA  AOAO8138_008904_010                 8138_008904_010                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @�_�k���@�_�k���11  @�_���v�@�_���v�@,[)^�	@,[)^�	�dS)�y���dS)�y��11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @=p�@�  @�G�@�  @�G�A ��A  A   A,��AAG�A`  A�  A��A��A�  A�  AϮA�  A�Q�B   B�
B(�B(�B�B'�
B0(�B8  B@  BH(�BP  BXQ�B`(�Bg�Bp  Bx  B�  B�{B�  B�  B�  B�  B��B��B�  B�  B�{B�{B�{B�{B�  B�  B�  B��B��B�  B��
B��B�  B�  B�{B�{B�{B�  B��B��B�  B�  B��C  C
=C{C
=C

=C
=C  C{C
=C
=C
=C�C
=C
=C  C��C!��C#��C%��C'��C*  C,  C.
=C0
=C2
=C4  C5��C8  C9��C<  C>
=C@
=CB
=CD
=CF  CH  CJ  CL  CN  CP
=CR
=CT  CU��CW��CZ  C[��C^  C`  Ca��Cd  Cf
=Ch
=Cj
=Cl
=Cm��Co��Cq��Ct  Cv  Cx  Cz  C{��C}��C�  C�C�C�
=C�  C�  C���C���C�  C�C�C�  C���C�C�C�  C�  C�  C�C�  C���C�  C�  C�  C�C�  C���C���C�  C�  C�  C�  C�  C���C���C�  C�C�  C���C���C���C���C�  C�  C�  C���C�  C�
=C�C�C�C�C�  C���C�  C�  C���C�  C�C�C�C�C�  C���C�  C�  C�  C�  C�C�  C���C���C�  C���C�C�C�C�C���C���C���C�  C�  C���C�C�C���C�  C�  C���C���C�  C�C�  C���C���C���C���C�  C�  C�C�  C���C�C�  C�C�C���C�  C�C�C�C�C�C�
=C�  C���C�  C���C���C�  C�  C���C���C���C���C�  C�  D   D � D �qD}qD  D��DD� D�qD}qD�qD� D  Dz�D�qD� D�qD}qD	  D	}qD
�D
��D  D� D�qD� D  D}qD  D��D  D}qD  D��D  Dz�D�qD� DD�D�D��D  D� D�qD� D�D� D�qDz�D��D}qD  D}qD�qD}qD��Dz�D��Dz�D  D� D�qDz�D�qD � D �qD!}qD!��D"z�D"��D#}qD$D$��D%�D%� D%�qD&� D'D'��D(�D(� D)  D)}qD)�qD*� D+D+��D,�D,� D,�qD-}qD.  D.��D/  D/��D0  D0}qD0�qD1� D1�qD2}qD3�D3�D4�D4� D4�qD5}qD6  D6� D6�qD7��D8  D8� D9  D9� D:  D:��D;  D;}qD;�qD<��D=�D=��D>�D>� D?�D?� D?��D@}qDA  DA��DB�DB� DB�qDC}qDD  DD� DE  DE� DF  DF� DF�qDG}qDH  DH� DI  DI��DJ  DJz�DJ�qDK� DL  DL� DL�qDM}qDM�qDN}qDO  DO��DP  DP� DP�qDQ� DR  DR� DR�qDS}qDT�DT��DT�qDU}qDV�DV��DW�DW�DX�DX��DY�DY� DZ�DZ��D[  D[}qD\  D\�D]D]�D^�D^��D_  D_}qD_�qD`}qD`�qDa}qDa�qDb}qDc  Dc� Dd  Dd��De�De��Df�Df��Dg�Dg}qDh  Dh��DiDi��Dj  Dj}qDj�qDk��Dl�Dl� Dm�Dm� Dm�qDn� Do�Do� Dp  Dp��Dq�Dq}qDq��Dr� Ds  Ds� Dt�Dt��DuDu� Dv�Dv��Dw  Dw� Dx  Dx� Dy  Dy}qDy�qDz}qD{  D{� D|  D|��D}�D}��D~�D~�D  D}qD�qD�@ D��HD��HD�HD�AHD��HD��HD�  D�>�D�� D�� D�HD�>�D�}qD�� D�HD�AHD��HD��HD���D�=qD�� D��HD�  D�@ D��HD��HD�  D�>�D�� D�� D�HD�AHD�~�D�� D�HD�@ D�� D��HD�HD�@ D�� D�� D���D�>�D�� D��HD���D�=qD�� D�� D���D�>�D�� D��HD�  D�@ D��HD�� D�  D�AHD��HD�� D��qD�>�D�� D���D���D�@ D��HD��HD�HD�@ D��HD��HD���D�@ D�� D���D�  D�B�D��HD�� D�  D�>�D�� D��HD�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�>�D�~�D���D�  D�@ D��HD��HD�  D�>�D��HD�D��D�AHD�� D��HD�HD�@ D�~�D�� D�HD�AHD�� D�D�HD�@ D�~�D��HD�HD�AHD���D�D�HD�AHD��HD��HD�  D�@ D�~�D�� D�  D�@ D��HD���D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�AHD��HD��HD�  D�AHD��HD�� D�  D�>�D�}qD���D�HD�@ D�� D�� D�  D�>�D�� D��HD��D�AHD�� D��HD�  D�AHD�� D�� D�  D�@ D��HD��HD�  D�>�D�~�D�� D�HD�AHD�� D�� D�  D�@ D��HD�� D���D�>�D�� D��HD�  D�>�D�� D�� D�  D�>�D�� D�� D�HD�@ D�� D�� D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D��HD�� D���D�>�D�~�D���D�  D�AHD�� D�� D�  D�@ D�� D��HD�  D�>�D�� D��HD�HD�AHD��HD�� D�  D�>�D�~�D���D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD��HD��HD�  D�@ D��HD�� D�  D�AHDHD��HD�HD�@ D�~�D�� D�HD�@ DĀ D�� D�  D�@ Dŀ D�� D���D�@ Dƀ D��HD�HD�AHDǁHD��HD�  D�>�DȀ D��HD�  D�>�Dɀ D��HD�  D�>�Dʀ D��HD�HD�@ Dˀ D�� D�HD�@ D̀ D�� D�  D�>�D�~�D�� D��qD�>�D΀ Dξ�D���D�AHDπ DϾ�D���D�@ DЁHD��HD�  D�@ Dр D�� D���D�>�D�}qDҾ�D���D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�AHDՁHD��HD�HD�AHDւ�D��HD�HD�AHDׁHD��HD�HD�@ D�}qDؾ�D���D�>�Dـ D�� D�  D�AHDڀ DڽqD��qD�=qD�~�D��HD�  D�@ D�~�Dܾ�D���D�>�D�}qDݽqD�  D�@ Dހ D�� D���D�>�D߀ D��HD�HD�=qD�~�DྸD���D�=qD� D��HD�HD�@ D�}qD�� D�HD�@ D�~�D��HD�  D�@ D�HD��HD�  D�@ D�HD�� D�  D�>�D�~�D�� D��D�B�D�` ?\)?8Q�?��?��R?�p�?�
=@   @��@��@&ff@:�H@L��@W
=@k�@}p�@�ff@�\)@�
=@��\@���@��@�(�@��@�{@�z�@޸R@�@�\)@�Q�A ��A�A	��Ap�A�A
=A�HA   A$z�A)��A.{A1�A7
=A;�A@��ADz�AH��AMp�AR�\AW
=AZ�HA`  Ae�Ai��An{Ar�\Aw
=A|(�A���A��\A���A��A��A�z�A�ffA���A��A�A�  A��\A���A�
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ?�  @   @=p�@�  @�G�@�  @�G�A ��A  A   A,��AAG�A`  A�  A��A��A�  A�  AϮA�  A�Q�B   B�
B(�B(�B�B'�
B0(�B8  B@  BH(�BP  BXQ�B`(�Bg�Bp  Bx  B�  B�{B�  B�  B�  B�  B��B��B�  B�  B�{B�{B�{B�{B�  B�  B�  B��B��B�  B��
B��B�  B�  B�{B�{B�{B�  B��B��B�  B�  B��C  C
=C{C
=C

=C
=C  C{C
=C
=C
=C�C
=C
=C  C��C!��C#��C%��C'��C*  C,  C.
=C0
=C2
=C4  C5��C8  C9��C<  C>
=C@
=CB
=CD
=CF  CH  CJ  CL  CN  CP
=CR
=CT  CU��CW��CZ  C[��C^  C`  Ca��Cd  Cf
=Ch
=Cj
=Cl
=Cm��Co��Cq��Ct  Cv  Cx  Cz  C{��C}��C�  C�C�C�
=C�  C�  C���C���C�  C�C�C�  C���C�C�C�  C�  C�  C�C�  C���C�  C�  C�  C�C�  C���C���C�  C�  C�  C�  C�  C���C���C�  C�C�  C���C���C���C���C�  C�  C�  C���C�  C�
=C�C�C�C�C�  C���C�  C�  C���C�  C�C�C�C�C�  C���C�  C�  C�  C�  C�C�  C���C���C�  C���C�C�C�C�C���C���C���C�  C�  C���C�C�C���C�  C�  C���C���C�  C�C�  C���C���C���C���C�  C�  C�C�  C���C�C�  C�C�C���C�  C�C�C�C�C�C�
=C�  C���C�  C���C���C�  C�  C���C���C���C���C�  C�  D   D � D �qD}qD  D��DD� D�qD}qD�qD� D  Dz�D�qD� D�qD}qD	  D	}qD
�D
��D  D� D�qD� D  D}qD  D��D  D}qD  D��D  Dz�D�qD� DD�D�D��D  D� D�qD� D�D� D�qDz�D��D}qD  D}qD�qD}qD��Dz�D��Dz�D  D� D�qDz�D�qD � D �qD!}qD!��D"z�D"��D#}qD$D$��D%�D%� D%�qD&� D'D'��D(�D(� D)  D)}qD)�qD*� D+D+��D,�D,� D,�qD-}qD.  D.��D/  D/��D0  D0}qD0�qD1� D1�qD2}qD3�D3�D4�D4� D4�qD5}qD6  D6� D6�qD7��D8  D8� D9  D9� D:  D:��D;  D;}qD;�qD<��D=�D=��D>�D>� D?�D?� D?��D@}qDA  DA��DB�DB� DB�qDC}qDD  DD� DE  DE� DF  DF� DF�qDG}qDH  DH� DI  DI��DJ  DJz�DJ�qDK� DL  DL� DL�qDM}qDM�qDN}qDO  DO��DP  DP� DP�qDQ� DR  DR� DR�qDS}qDT�DT��DT�qDU}qDV�DV��DW�DW�DX�DX��DY�DY� DZ�DZ��D[  D[}qD\  D\�D]D]�D^�D^��D_  D_}qD_�qD`}qD`�qDa}qDa�qDb}qDc  Dc� Dd  Dd��De�De��Df�Df��Dg�Dg}qDh  Dh��DiDi��Dj  Dj}qDj�qDk��Dl�Dl� Dm�Dm� Dm�qDn� Do�Do� Dp  Dp��Dq�Dq}qDq��Dr� Ds  Ds� Dt�Dt��DuDu� Dv�Dv��Dw  Dw� Dx  Dx� Dy  Dy}qDy�qDz}qD{  D{� D|  D|��D}�D}��D~�D~�D  D}qD�qD�@ D��HD��HD�HD�AHD��HD��HD�  D�>�D�� D�� D�HD�>�D�}qD�� D�HD�AHD��HD��HD���D�=qD�� D��HD�  D�@ D��HD��HD�  D�>�D�� D�� D�HD�AHD�~�D�� D�HD�@ D�� D��HD�HD�@ D�� D�� D���D�>�D�� D��HD���D�=qD�� D�� D���D�>�D�� D��HD�  D�@ D��HD�� D�  D�AHD��HD�� D��qD�>�D�� D���D���D�@ D��HD��HD�HD�@ D��HD��HD���D�@ D�� D���D�  D�B�D��HD�� D�  D�>�D�� D��HD�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�>�D�~�D���D�  D�@ D��HD��HD�  D�>�D��HD�D��D�AHD�� D��HD�HD�@ D�~�D�� D�HD�AHD�� D�D�HD�@ D�~�D��HD�HD�AHD���D�D�HD�AHD��HD��HD�  D�@ D�~�D�� D�  D�@ D��HD���D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�AHD��HD��HD�  D�AHD��HD�� D�  D�>�D�}qD���D�HD�@ D�� D�� D�  D�>�D�� D��HD��D�AHD�� D��HD�  D�AHD�� D�� D�  D�@ D��HD��HD�  D�>�D�~�D�� D�HD�AHD�� D�� D�  D�@ D��HD�� D���D�>�D�� D��HD�  D�>�D�� D�� D�  D�>�D�� D�� D�HD�@ D�� D�� D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D��HD�� D���D�>�D�~�D���D�  D�AHD�� D�� D�  D�@ D�� D��HD�  D�>�D�� D��HD�HD�AHD��HD�� D�  D�>�D�~�D���D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD��HD��HD�  D�@ D��HD�� D�  D�AHDHD��HD�HD�@ D�~�D�� D�HD�@ DĀ D�� D�  D�@ Dŀ D�� D���D�@ Dƀ D��HD�HD�AHDǁHD��HD�  D�>�DȀ D��HD�  D�>�Dɀ D��HD�  D�>�Dʀ D��HD�HD�@ Dˀ D�� D�HD�@ D̀ D�� D�  D�>�D�~�D�� D��qD�>�D΀ Dξ�D���D�AHDπ DϾ�D���D�@ DЁHD��HD�  D�@ Dр D�� D���D�>�D�}qDҾ�D���D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�AHDՁHD��HD�HD�AHDւ�D��HD�HD�AHDׁHD��HD�HD�@ D�}qDؾ�D���D�>�Dـ D�� D�  D�AHDڀ DڽqD��qD�=qD�~�D��HD�  D�@ D�~�Dܾ�D���D�>�D�}qDݽqD�  D�@ Dހ D�� D���D�>�D߀ D��HD�HD�=qD�~�DྸD���D�=qD� D��HD�HD�@ D�}qD�� D�HD�@ D�~�D��HD�  D�@ D�HD��HD�  D�@ D�HD�� D�  D�>�D�~�D�� D��D�B�G�O�?\)?8Q�?��?��R?�p�?�
=@   @��@��@&ff@:�H@L��@W
=@k�@}p�@�ff@�\)@�
=@��\@���@��@�(�@��@�{@�z�@޸R@�@�\)@�Q�A ��A�A	��Ap�A�A
=A�HA   A$z�A)��A.{A1�A7
=A;�A@��ADz�AH��AMp�AR�\AW
=AZ�HA`  Ae�Ai��An{Ar�\Aw
=A|(�A���A��\A���A��A��A�z�A�ffA���A��A�A�  A��\A���A�
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��`A��mA��`A��
A��A�Aɕ�AɑhAɓuAɑhA�x�A�XA�;dA�-A�(�A��A��A��A��A�{A�{A�{A�{A�oA�bA�VA�A���A��Aȏ\A�A�A���A���A�7LA���A�5?A�-A�p�A�`BA��^A��A���A��A�33A���A���A�hsA��HA��jA�K�A��A��\A��A���A�"�A�G�A���A��jA�+A�A�A�\)A�/A�33A�%A��uA��;A�-A�dZA�1A�S�A�&�A��A�x�A�"�A�|�A���A�%AA}ƨA{�;Awl�ArffAp�\Ao;dAml�Ai��Ac�7A`=qA_|�AZ �AY?}AUC�AQ��AO?}AL��AK?}AJbNAI��AIC�AH�\AH�AG%ABVA?;dA;dZA9l�A7&�A4�`A2��A1�7A1VA0��A0z�A0ffA0��A0��A/C�A.��A.��A/?}A/p�A/t�A/x�A1VA0��A0-A/p�A//A/�-A.5?A,�`A+"�A)��A&�jA${A#C�A"A�A!XAĜA�yA��A�+A�PA33A
=A�7A�+A�HAr�AE�A(�A�A(�A(�A{AA  A�AA�A��AhsAt�A��A�RAoA�A��A�HA��A�+AbNA(�AK�AAK�A��Av�AJA�hA`BA�A�A�TA�7A\)A+A�`A��A�!Az�An�AjAn�A��A��AS�A7LA"�A��AJA�hA��A�A^5AA��A�A�A�Av�A��A�FA
��A
z�A
A�A	�hA��A�A�AjA(�A�mA�AdZA7LA33A33A�A�9AM�A�;A��A7LA�A�jA�AE�A�TA��A�A��A �AA|�AS�A33A ��A ��A z�A 1'@��@�dZ@�v�@��@��@�&�@�1'@���@��R@��@��@���@�1@���@�ȴ@��@�hs@���@�@�w@�@�x�@�@��m@�P@�33@�hs@�  @���@�+@��T@�-@陚@�X@�Q�@��@�=q@�-@�7@���@��;@�R@�E�@�^@�7@�hs@�%@�9@�Z@߶F@�"�@ݲ-@��`@ܓu@�Z@�;d@ڗ�@�5?@٩�@���@��
@�
=@�ff@�5?@��#@ՙ�@�/@Դ9@�I�@�ƨ@��@�@�@Ѳ-@љ�@��@�A�@ΰ!@��@��T@͑h@��@�b@˥�@�K�@��@�=q@�?}@�/@�&�@�Ĝ@�z�@�1@Ǖ�@�33@ư!@�M�@��@�V@ċD@�9X@�b@���@���@Õ�@�dZ@�S�@�C�@��y@��@�`B@�&�@�&�@�V@���@���@�Q�@�A�@�1@��F@���@�=q@��@��-@��@�(�@���@�33@�@��!@�@��7@�?}@���@�z�@�1'@���@�C�@��+@���@��@�/@���@��u@�j@�Q�@� �@��@���@��@���@�t�@�K�@��@��@��R@�@�Ĝ@�Z@��@�l�@���@�E�@�-@��@��@���@�7L@��j@��9@��@� �@�C�@�
=@���@��#@���@��h@�`B@�/@��/@���@�Q�@��@��m@�|�@�C�@���@�-@���@��-@�7L@��/@��@�b@�S�@���@�{@��#@��-@��@�/@�j@�(�@��
@���@�+@���@��\@�v�@�ff@�E�@�=q@�5?@�{@��@��-@���@��7@�p�@�O�@�Ĝ@�I�@�1@�ƨ@�K�@�@��@��!@��+@�V@�-@��@�@���@�hs@��@�b@�t�@�
=@���@��@��#@���@���@�z�@�9X@��@��m@�ƨ@���@�l�@�o@���@�V@��@���@��@�p�@�`B@�?}@�%@��`@�Ĝ@��9@���@��D@�Z@��
@��!@�E�@��@���@�`B@�/@�%@�z�@�b@��@�ƨ@�l�@��y@��R@�v�@�V@�=q@��#@��h@�p�@�?}@�&�@��@��@�V@�%@���@��j@���@�z�@�1@��m@��
@���@�S�@��@��@�~�@�M�@�E�@�E�@�5?@��7@�/@���@�A�@��@�K�@�@���@��@��@��y@��H@�ȴ@���@�n�@�^5@�{@���@��7@�x�@�p�@�X@�7L@��`@���@�Q�@l�@~ff@}p�@|��@|1@{�@{"�@z^5@y�@y��@y��@yG�@y%@x��@x�9@xr�@x  @w�w@wl�@w
=@vv�@v5?@u��@t��@t�@t��@tz�@tj@t9X@s�@s33@so@r�@r~�@q�#@qG�@p��@p1'@o�@o;d@n�y@nȴ@n�R@n�R@n��@n$�@mp�@m�@l�@l�j@lj@k��@k@j��@j��@jM�@i�#@ix�@h��@h��@g�@g�P@gl�@gl�@g
=@f��@fE�@f5?@f$�@e�@e@e�@d�j@d�@c��@cS�@cS�@co@b��@a��@ax�@`�9@`1'@_\)@^��@^v�@]O�@\z�@\�@[�
@[C�@Z�H@Z�!@Z~�@Zn�@Z=q@Y�^@X��@X  @W�w@W��@W+@Vȴ@U�-@U�@T�D@T9X@S��@SS�@So@R�H@R�\@R=q@RJ@Q�#@Q�7@P��@P�@O�@O\)@O+@N�@N�+@N5?@M�T@M�-@M�@M`B@M/@L�@K��@KS�@Ko@J�@J��@J�!@J��@J�\@JM�@JJ@I��@Ihs@IX@I�@H�@G+@F{@E�@E�@E�T@E��@E�-@E�@Ep�@EO�@E�@D��@Dj@D�@CdZ@B�@B��@B�!@B�\@Bn�@BM�@A�@Ax�@A�@@bN@?�;@?��@?l�@?l�@?K�@>�@>v�@>$�@=@=�-@=�h@=O�@<�@<Z@<9X@<1@;�F@;dZ@;33@;@:��@:^5@:J@9��@9��@9G�@9%@8Ĝ@8bN@7�@7�w@7��@7�P@7|�@7K�@6�R@6$�@5��@5�-@5�@5`B@5?}@4�@4�@4��@4z�@4j@4j@49X@3��@333@3@2�\@2^5@1�@1��@1hs@1&�@1&�@1�@1%@0��@0Ĝ@0 �@/�;@/�w@.�@.{@-@-�-@-��@-?}@,�j@,�D@,9X@,(�@+��@+�@*��@*�!@*�!@*��@*�\@*n�@*M�@*-@)�@)�^@)��@)��@)��@)�7@)&�@(�@'�@'�w@'�P@'K�@&�y@&�+@&ff@&@%��@%�-@%�@%p�@%`B@%?}@%V@$��@$�/@$��@$z�@$j@$I�@$�@#�m@#�
@#�@"�@"^5@!x�@!%@ �u@ A�@  �@�;@�@l�@��@�@�+@V@�@�h@O�@/@�@�@z�@j@Z@�m@��@�F@��@��@��@�@�@�@�@�@��@��@��@��@�@��@�@�@dZ@C�@33@33@o@�H@��@M�@J@�#@��@hs@7LA��#A��/A��`A��mA��`A��TA��yA��`A��`A��mA��yA��A��TA��`A��mA��TA��HA��
A��
A���A��A��HA��TA�ȴA���A���AɾwAɩ�Aə�AɓuAɓuAɍPAɏ\AɓuAɓuAɑhAɑhAɓuAɓuAɏ\Aɏ\Aɇ+A�~�A�z�A�hsA�jA�r�A�n�A�dZA�\)A�\)A�O�A�I�A�E�A�E�A�C�A�=qA�5?A�5?A�7LA�1'A�/A�-A�/A�/A�-A�-A�/A�-A�(�A�&�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A��TA��`A��mA��`A��
A��A�Aɕ�AɑhAɓuAɑhA�x�A�XA�;dA�-A�(�A��A��A��A��A�{A�{A�{A�{A�oA�bA�VA�A���A��Aȏ\A�A�A���A���A�7LA���A�5?A�-A�p�A�`BA��^A��A���A��A�33A���A���A�hsA��HA��jA�K�A��A��\A��A���A�"�A�G�A���A��jA�+A�A�A�\)A�/A�33A�%A��uA��;A�-A�dZA�1A�S�A�&�A��A�x�A�"�A�|�A���A�%AA}ƨA{�;Awl�ArffAp�\Ao;dAml�Ai��Ac�7A`=qA_|�AZ �AY?}AUC�AQ��AO?}AL��AK?}AJbNAI��AIC�AH�\AH�AG%ABVA?;dA;dZA9l�A7&�A4�`A2��A1�7A1VA0��A0z�A0ffA0��A0��A/C�A.��A.��A/?}A/p�A/t�A/x�A1VA0��A0-A/p�A//A/�-A.5?A,�`A+"�A)��A&�jA${A#C�A"A�A!XAĜA�yA��A�+A�PA33A
=A�7A�+A�HAr�AE�A(�A�A(�A(�A{AA  A�AA�A��AhsAt�A��A�RAoA�A��A�HA��A�+AbNA(�AK�AAK�A��Av�AJA�hA`BA�A�A�TA�7A\)A+A�`A��A�!Az�An�AjAn�A��A��AS�A7LA"�A��AJA�hA��A�A^5AA��A�A�A�Av�A��A�FA
��A
z�A
A�A	�hA��A�A�AjA(�A�mA�AdZA7LA33A33A�A�9AM�A�;A��A7LA�A�jA�AE�A�TA��A�A��A �AA|�AS�A33A ��A ��A z�A 1'@��@�dZ@�v�@��@��@�&�@�1'@���@��R@��@��@���@�1@���@�ȴ@��@�hs@���@�@�w@�@�x�@�@��m@�P@�33@�hs@�  @���@�+@��T@�-@陚@�X@�Q�@��@�=q@�-@�7@���@��;@�R@�E�@�^@�7@�hs@�%@�9@�Z@߶F@�"�@ݲ-@��`@ܓu@�Z@�;d@ڗ�@�5?@٩�@���@��
@�
=@�ff@�5?@��#@ՙ�@�/@Դ9@�I�@�ƨ@��@�@�@Ѳ-@љ�@��@�A�@ΰ!@��@��T@͑h@��@�b@˥�@�K�@��@�=q@�?}@�/@�&�@�Ĝ@�z�@�1@Ǖ�@�33@ư!@�M�@��@�V@ċD@�9X@�b@���@���@Õ�@�dZ@�S�@�C�@��y@��@�`B@�&�@�&�@�V@���@���@�Q�@�A�@�1@��F@���@�=q@��@��-@��@�(�@���@�33@�@��!@�@��7@�?}@���@�z�@�1'@���@�C�@��+@���@��@�/@���@��u@�j@�Q�@� �@��@���@��@���@�t�@�K�@��@��@��R@�@�Ĝ@�Z@��@�l�@���@�E�@�-@��@��@���@�7L@��j@��9@��@� �@�C�@�
=@���@��#@���@��h@�`B@�/@��/@���@�Q�@��@��m@�|�@�C�@���@�-@���@��-@�7L@��/@��@�b@�S�@���@�{@��#@��-@��@�/@�j@�(�@��
@���@�+@���@��\@�v�@�ff@�E�@�=q@�5?@�{@��@��-@���@��7@�p�@�O�@�Ĝ@�I�@�1@�ƨ@�K�@�@��@��!@��+@�V@�-@��@�@���@�hs@��@�b@�t�@�
=@���@��@��#@���@���@�z�@�9X@��@��m@�ƨ@���@�l�@�o@���@�V@��@���@��@�p�@�`B@�?}@�%@��`@�Ĝ@��9@���@��D@�Z@��
@��!@�E�@��@���@�`B@�/@�%@�z�@�b@��@�ƨ@�l�@��y@��R@�v�@�V@�=q@��#@��h@�p�@�?}@�&�@��@��@�V@�%@���@��j@���@�z�@�1@��m@��
@���@�S�@��@��@�~�@�M�@�E�@�E�@�5?@��7@�/@���@�A�@��@�K�@�@���@��@��@��y@��H@�ȴ@���@�n�@�^5@�{@���@��7@�x�@�p�@�X@�7L@��`@���@�Q�@l�@~ff@}p�@|��@|1@{�@{"�@z^5@y�@y��@y��@yG�@y%@x��@x�9@xr�@x  @w�w@wl�@w
=@vv�@v5?@u��@t��@t�@t��@tz�@tj@t9X@s�@s33@so@r�@r~�@q�#@qG�@p��@p1'@o�@o;d@n�y@nȴ@n�R@n�R@n��@n$�@mp�@m�@l�@l�j@lj@k��@k@j��@j��@jM�@i�#@ix�@h��@h��@g�@g�P@gl�@gl�@g
=@f��@fE�@f5?@f$�@e�@e@e�@d�j@d�@c��@cS�@cS�@co@b��@a��@ax�@`�9@`1'@_\)@^��@^v�@]O�@\z�@\�@[�
@[C�@Z�H@Z�!@Z~�@Zn�@Z=q@Y�^@X��@X  @W�w@W��@W+@Vȴ@U�-@U�@T�D@T9X@S��@SS�@So@R�H@R�\@R=q@RJ@Q�#@Q�7@P��@P�@O�@O\)@O+@N�@N�+@N5?@M�T@M�-@M�@M`B@M/@L�@K��@KS�@Ko@J�@J��@J�!@J��@J�\@JM�@JJ@I��@Ihs@IX@I�@H�@G+@F{@E�@E�@E�T@E��@E�-@E�@Ep�@EO�@E�@D��@Dj@D�@CdZ@B�@B��@B�!@B�\@Bn�@BM�@A�@Ax�@A�@@bN@?�;@?��@?l�@?l�@?K�@>�@>v�@>$�@=@=�-@=�h@=O�@<�@<Z@<9X@<1@;�F@;dZ@;33@;@:��@:^5@:J@9��@9��@9G�@9%@8Ĝ@8bN@7�@7�w@7��@7�P@7|�@7K�@6�R@6$�@5��@5�-@5�@5`B@5?}@4�@4�@4��@4z�@4j@4j@49X@3��@333@3@2�\@2^5@1�@1��@1hs@1&�@1&�@1�@1%@0��@0Ĝ@0 �@/�;@/�w@.�@.{@-@-�-@-��@-?}@,�j@,�D@,9X@,(�@+��@+�@*��@*�!@*�!@*��@*�\@*n�@*M�@*-@)�@)�^@)��@)��@)��@)�7@)&�@(�@'�@'�w@'�P@'K�@&�y@&�+@&ff@&@%��@%�-@%�@%p�@%`B@%?}@%V@$��@$�/@$��@$z�@$j@$I�@$�@#�m@#�
@#�@"�@"^5@!x�@!%@ �u@ A�@  �@�;@�@l�@��@�@�+@V@�@�h@O�@/@�@�@z�@j@Z@�m@��@�F@��@��@��@�@�@�@�@�@��@��@��@��@�@��@�@�@dZ@C�@33@33@o@�H@��@M�@J@�#@��@hsG�O�A��#A��/A��`A��mA��`A��TA��yA��`A��`A��mA��yA��A��TA��`A��mA��TA��HA��
A��
A���A��A��HA��TA�ȴA���A���AɾwAɩ�Aə�AɓuAɓuAɍPAɏ\AɓuAɓuAɑhAɑhAɓuAɓuAɏ\Aɏ\Aɇ+A�~�A�z�A�hsA�jA�r�A�n�A�dZA�\)A�\)A�O�A�I�A�E�A�E�A�C�A�=qA�5?A�5?A�7LA�1'A�/A�-A�/A�/A�-A�-A�/A�-A�(�A�&�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bw�Bw�Bw�Bw�BwfBw�Bx8Bv�Bu�Bv+Bu�Bv+Bv�Bu�Bt�Bt�Bu%Bt�Bt�Bt�Bt�Bt�Bt�Bu%Bt�Bt�Bt�BsBqBn�Bh�BH�B�BB�B	��B
�bB
�HB
�B
��B
�(B
=B/OB=qB@�B?B9XBAUBe�B~]B�_B��B�GB�B�4B�B�B�B�)B��B�LB��B�_B^5B6�BIB�B
�B
�KB
�pB
�!B
�eB
�B
��B
��B
s�B
WsB
+B	�B	�B	�<B	�tB	��B	�+B	��B	�	B	�B	q�B	l�B	e�B	^jB	NB	>�B	-wB	SB	VB	oB	B	B	MB	�B	�B	B	YB�vB��B	 �B	GB	�B	!�B	$�B	%FB	3�B	CaB	G�B	bB	o�B	��B	��B	��B	�B	�?B	��B	�QB	��B
B
!�B
 �B
�B
$B
B'B
:�B
6B
'B
�B
�B	��B	�TB	�EB	�B	��B	��B	��B	��B	�kB	�B	�B	ɆB	�XB	��B	�hB	��B	ʌB	�)B	�6B	�HB	��B	бB	�,B	��B	�B	�B
�B
�B
bB
)�B
5�B
<6B
?B
D3B
K)B
NB
QB
PHB
P}B
N�B
NB
M�B
OBB
PHB
PB
OB
O�B
OvB
NpB
M6B
MB
L�B
M�B
MjB
M�B
NB
M�B
N�B
O�B
U2B
T�B
RTB
Q�B
RTB
P�B
S[B
T�B
VB
VB
P}B
O�B
RTB
S[B
T�B
R�B
R�B
PB
OBB
M�B
J�B
I�B
K�B
GzB
E�B
DgB
C�B
B�B
C-B
C-B
B�B
B�B
B�B
B'B
B�B
A�B
B'B
@�B
?�B
?HB
>B
=�B
=<B
=qB
=�B
;�B
;dB
9�B
9XB
8�B
7B
6zB
5tB
4�B
3�B
3hB
3hB
1[B
1�B
0�B
/�B
.�B
/OB
-B
,�B
+�B
)�B
*0B
)*B
(XB
&�B
(�B
'B
&�B
%�B
%B
$@B
#�B
!�B
!bB
 'B
�B
IB
!�B
�B
�B
eB
1B
�B
SB
�B
SB
�B
B
B
uB
B
FB
B
�B
B
:B
:B
�B
B
�B
�B
B
B
bB
�B
.B
�B
�B
�B
�B
�B
VB
�B
�B
xB
B

rB

	B
	7B
	B
�B
	7B
_B
YB
�B
�B
SB
%B
�B
�B
B
GB
�B
B
�B
B
oB
�B
B
oB
;B
�B
�B
�B
�B
B
B
uB
B
�B
�B
YB
�B
+B
�B
�B
�B
�B
�B
1B
	�B

rB

=B

rB

�B
DB
B
DB

�B

�B

rB
xB
xB
B
B
B
�B
xB
�B
DB
�B
~B
B
JB
~B
~B
JB
~B
JB
�B
B
B
B
�B
PB
�B
�B
PB
PB
�B
PB
PB
PB
PB
PB
�B
�B
�B
�B
VB
"B
(B
bB
.B
�B
bB
�B
�B
:B
B
hB
:B
:B
�B
oB
�B
�B
@B
uB
�B
FB
�B
�B
�B
�B
�B
�B
�B
�B
�B
_B
1B
1B
�B
�B
=B
IB
�B
B
�B
B
�B
�B
�B
�B
VB
�B
!�B
!�B
"4B
"hB
"hB
"�B
"�B
"�B
#:B
#:B
"�B
"�B
#:B
#B
#B
$B
"�B
"�B
#nB
$�B
%FB
%�B
%zB
%FB
%B
$tB
$tB
$B
$@B
#:B
#�B
"4B
 �B
�B
�B
B
�B
B
�B
�B
�B
�B
�B
 'B
 'B
 �B
 �B
!-B
!�B
!�B
"hB
"hB
"hB
"4B
"hB
"�B
"�B
"�B
"�B
"�B
"hB
"hB
#�B
&�B
%zB
%�B
&�B
'B
'B
'�B
(�B
)_B
)_B
(�B
*eB
*eB
*eB
+B
*�B
+B
,=B
,qB
,�B
-CB
-CB
-B
-CB
-CB
-CB
-�B
-�B
-�B
.�B
/�B
/�B
/�B
0UB
/�B
/�B
/OB
0!B
/�B
/OB
.�B
.�B
/�B
.�B
/OB
/�B
0UB
1�B
2-B
2-B
2-B
2-B
1�B
1�B
1�B
2-B
3�B
49B
5tB
5�B
5tB
5tB
5B
5B
4�B
5B
49B
4�B
4�B
5�B
6B
5�B
6zB
6�B
6�B
7�B
7�B
7�B
7�B
8�B
8RB
8�B
8�B
8�B
9XB
9XB
9�B
9�B
:�B
:*B
:�B
;�B
;�B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
=<B
=�B
>B
>�B
?HB
?B
@B
@B
@B
@B
?�B
@B
@�B
A B
A B
AUB
A B
A�B
B[B
B�B
B�B
B�B
CaB
CaB
C�B
C�B
D3B
EB
E9B
E9B
EB
EmB
E�B
E�B
E�B
E�B
FB
FB
F?B
F�B
GB
GB
GEB
GB
GB
GEB
G�B
GB
GEB
GEB
GB
GB
GB
HKB
H�B
IB
IB
I�B
I�B
I�B
J#B
J#B
I�B
JXB
K^B
K�B
K�B
K�B
K�B
K�B
L�B
MB
M6B
MjB
N<B
N<B
NpB
NpB
N�B
N�B
N�B
N�B
OBB
O�B
O�B
P}B
P}B
P�B
P�B
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
S&B
R�B
S&B
S&B
S[B
S[B
S[B
S[B
S[B
S�B
S�B
S�B
S�B
S�B
S�B
V9B
V9B
V9B
VmB
V9B
V9B
V�B
V�B
V�B
V�B
V�B
W
B
W?B
W?B
XyB
XyB
XyB
X�B
XyB
X�B
X�B
YB
YB
YB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
]/B
]�B
]�B
]�B
^5B
^5B
^jB
^jB
^�B
_B
_;B
_pB
_�B
`B
`BB
`vB
`vB
`BB
`BB
aHB
a|B
a�B
a�B
a�B
a�B
bB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c B
c�B
cTB
c�B
c�B
d�B
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
e`B
gB
gmB
gmB
g8B
g8B
g�B
h>B
hsB
h�B
hsB
h�B
iyB
jB
i�B
i�B
jB
jB
jB
jB
j�B
kQB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
m�B
n/B
n/B
n/B
ncB
ncB
n�B
n�B
n�B
o B
o B
o B
o B
o5B
oiB
o5B
oiB
o�B
pB
p�B
q�B
q�B
rGB
r|B
r|B
r�B
r�B
sMB
s�B
s�B
s�B
tB
t�B
t�B
t�B
u%B
u%B
u�B
u�B
u�B
u�B
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
v�B
v�B
v�B
v�B
v�B
v�B
w2B
v�B
v�B
v�B
v�B
v�B
wfB
w�B
w�B
x8B
x8B
x�B
x�Bv�By�Bv�Bv�By	BxBv�Bw�BxlBw2Bv�BwfBx�Bv�Bv�BxBy�Bw�Bv+Bx�Bw�BsBv`By�Bv`Bt�BxlB|B}�Bw2Bt�Bv�Bv`BuZBu�Bv�Bv�Bv+Bu�Bu�Bv+BxlBu�Bx�BwfBu�Bu%Bu�BwfBv`Bv`Bx8Bv`Bv�Bu�BuZBuZBv`Bu�BuZBt�Bt�Bu�Bu%BtBt�Bu�Bt�Bu%BtBt�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Bw�Bw�Bw�Bw�BwuBxhBy.Bv�BvBv�Bv�Bw	BwDBvBt�Bu.Bu=Bt�Bt�Bt�Bt�Bt�Bt�Bu1Bt�BuBt�BsrBq�BqoBo]BG�B��B�MB	��B
��B
ֶB
��ByB�B�B4BC[BE�BG�BA|BO�Bu\B��B�B�,B��B��B�B��B��B�vB̈́B��BðB�iB�xBq"BEB&�B�B�B
�B
�rB
�-B
��B
͢B
�zB
��B
�EB
o�B
IB	�!B	��B	ĠB	��B	��B	��B	�qB	�B	�8B	��B	uB	h2B	k4B	Q*B	I B	6�B	�B	�B	�B	_B	�B	�B	�B	B	eB	RB�8B�B	B		�B	�B	&�B	(/B	&�B	4�B	D,B	HB	apB	o�B	�dB	�kB	��B	��B	ŔB	��B	�B	��B
�B
#�B
"�B
 �B
#fB
G�B
@B
<�B
-dB
"{B
!B	�B	�fB	ܫB	��B	�pB	��B	�YB	�[B	�B	��B	��B	͋B	�B	��B	�^B	�B	ʵB	�B	�IB	ЎB	�%B	��B	��B	�BB	�B	�iB
:B
�B
�B
(oB
5�B
<�B
?�B
E5B
K�B
N�B
R?B
S�B
ULB
Q�B
O\B
O�B
P�B
RB
P�B
PUB
QxB
RXB
O�B
N B
M�B
M�B
N0B
M�B
NbB
N;B
M�B
OB
QvB
V�B
U�B
R�B
RWB
T:B
S�B
UHB
T�B
X]B
YB
R&B
P�B
STB
VnB
U�B
S�B
T�B
Q�B
R|B
O�B
K�B
L�B
N$B
H:B
F�B
E�B
E6B
DJB
D�B
C�B
CHB
CB
B�B
C_B
D0B
C�B
DB
A�B
A�B
@�B
?B
>�B
>~B
?/B
?7B
=�B
=�B
;�B
;B
9�B
7�B
7-B
6�B
69B
4�B
4�B
4�B
2�B
3{B
1dB
07B
0�B
1qB
.�B
.�B
-�B
+tB
+mB
*�B
)mB
(�B
*�B
(hB
'�B
',B
'B
&�B
&0B
#�B
"�B
! B
�B
!�B
$�B
�B
fB
B
�B
TB
PB
]B
$B
�B
_B
�B
B
�B
�B
CB
B
�B
�B
)B
�B
B
;B
�B
CB
�B
IB
�B
�B
B
�B
MB
/B
�B
:B
�B
�B
UB
�B
�B
;B

]B

tB

�B

B

B
�B
cB
�B
�B
	�B
�B
�B
B
�B
�B
$B
�B
JB
"B
�B
IB
�B
/B
�B
�B
�B
�B
FB
B
�B
B
)B
�B
�B
B
�B
�B
<B
�B
�B
HB
	�B

�B

�B

LB

�B
SB
�B
�B
nB
IB
LB
�B
�B
B
�B
/B
�B
�B
5B
B
�B
=B
�B
�B
B
B
B
@B
FB
�B
�B
uB
�B
�B
�B
�B
�B
�B
qB
�B
�B
lB
�B
�B
�B
�B
gB
<B
�B
�B
�B
XB
�B
B
fB
�B
�B
CB
B
'B
-B
�B
+B
�B
eB
"B
�B
B
�B
�B
$B
�B
8B
�B
B
B
�B
[B
>B
�B
B
B
5B
B
tB
�B
�B
�B
�B
�B
MB
�B
XB
 B
XB
>B
�B
 �B
"�B
" B
"nB
"�B
"�B
"�B
"�B
# B
#�B
#�B
"�B
#B
#xB
#lB
$8B
%
B
#=B
#?B
$jB
%HB
%�B
&B
%�B
%�B
%jB
$�B
$�B
$�B
$�B
$6B
&BB
#�B
!�B
 �B
 B
�B
�B
�B
 �B
 $B
 MB
 7B
 JB
 �B
 �B
!jB
!�B
"/B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#%B
"�B
# B
"�B
"�B
#B
#�B
&(B
'�B
&B
&�B
'�B
'�B
'�B
(�B
)}B
)�B
)�B
)�B
+yB
*�B
*�B
+WB
*�B
+�B
,�B
,�B
-JB
-B
-]B
-(B
-^B
-kB
-�B
-�B
.)B
._B
/�B
/�B
/�B
0XB
0�B
0zB
05B
0RB
0�B
/�B
/aB
/B
08B
0dB
/�B
0�B
0�B
1�B
2�B
2JB
2DB
26B
2GB
2B
2CB
2gB
2�B
4B
4�B
6-B
6wB
5�B
5�B
5UB
5yB
5vB
5�B
5B
6%B
6-B
6�B
7B
6�B
7'B
7AB
7�B
8>B
7�B
80B
8WB
8�B
8�B
8�B
9B
9IB
9�B
9�B
:B
:uB
:�B
:�B
;�B
;�B
;�B
;�B
;�B
;�B
<�B
=7B
<�B
<�B
=DB
>B
>�B
>�B
?pB
?�B
?�B
@|B
@GB
@3B
@(B
@&B
@�B
AYB
A�B
AdB
A�B
A�B
B�B
CB
CLB
C1B
CB
C�B
C�B
DB
DVB
D�B
EbB
E\B
EFB
EdB
E�B
F"B
E�B
E�B
FB
F?B
FXB
F�B
GtB
G�B
GPB
GOB
GUB
G�B
G�B
H2B
G�B
G�B
HB
GyB
G�B
H&B
IB
IHB
IbB
I�B
JB
J"B
J B
J:B
JYB
JrB
K-B
L B
L
B
K�B
LB
L2B
L�B
M\B
M�B
M�B
M�B
N�B
NB
N�B
N�B
N�B
OB
OB
O3B
O�B
PB
PIB
QB
P�B
QB
Q?B
QrB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R�B
S�B
S<B
SLB
SNB
S�B
SrB
SvB
S�B
S�B
T6B
T/B
TB
TB
TuB
U[B
WBB
VdB
V@B
V�B
VQB
VbB
V�B
V�B
V�B
V�B
W2B
WtB
W�B
W�B
X�B
X�B
X�B
X�B
X�B
YB
YB
Y�B
Y�B
Z8B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[XB
[�B
[�B
[�B
[�B
[�B
\4B
\�B
\�B
\�B
]bB
]�B
]�B
]�B
^CB
^�B
^�B
^�B
^�B
_JB
__B
_�B
_�B
``B
`MB
`lB
`�B
`�B
`�B
`�B
a�B
a�B
a�B
b$B
bB
bB
b|B
b�B
b�B
b�B
b�B
b�B
b�B
chB
c�B
c�B
c�B
d;B
d{B
d�B
d�B
e>B
eB
eB
eB
eB
eB
e�B
e�B
e�B
fyB
g�B
g�B
g�B
g`B
g�B
hjB
h�B
h�B
h�B
h�B
iEB
jCB
j?B
i�B
i�B
j1B
jEB
j�B
j�B
k9B
k�B
k�B
k�B
k�B
k�B
lB
l�B
mmB
m:B
mB
l�B
m�B
m�B
m�B
nB
n:B
n_B
nkB
nJB
n�B
n�B
n�B
n�B
n�B
oQB
o-B
oB
o1B
owB
o�B
o_B
o�B
p_B
p�B
q�B
rFB
rnB
r�B
r�B
r�B
s-B
sFB
s�B
s�B
s�B
t8B
t�B
uB
u@B
uB
uFB
usB
vB
vB
v B
v�B
wB
v�B
v�B
wB
v�B
v�B
v�B
v�B
w B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w(B
w\B
wB
wB
w-B
wEB
wXB
w�B
w�B
xB
x{B
x�B
x�G�O�Bv�By�Bv�Bv�By	BxBv�Bw�BxlBw2Bv�BwfBx�Bv�Bv�BxBy�Bw�Bv+Bx�Bw�BsBv`By�Bv`Bt�BxlB|B}�Bw2Bt�Bv�Bv`BuZBu�Bv�Bv�Bv+Bu�Bu�Bv+BxlBu�Bx�BwfBu�Bu%Bu�BwfBv`Bv`Bx8Bv`Bv�Bu�BuZBuZBv`Bu�BuZBt�Bt�Bu�Bu%BtBt�Bu�Bt�Bu%BtBt�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<oK�<���<S8y<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-�p<S��<Z��<�d9<�A`<q]�<'��<#�
<E@�<���<�\<���<#�
<#�
</f�<&�I<V<ǆ�<��^<2A<#�
<#�
<s#�<nP<#�
<#�
<#�
<*6S<�ͤ<#�
<#�
<Yo�<#�
<1��<'�X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<M�<#�
<2�'<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<>�<)o<<#�
<#�
<#�
<,q<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021022002311720210220023117IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030203004020210302030040QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030203004020210302030040QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364520220422133645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064020220426160640IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064020220426160640IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064020220426160640IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                