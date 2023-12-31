CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-06-20T03:25:42Z creation; 2022-04-26T16:06:59Z DMQC;      
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
_FillValue        G�O�     `  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  Zh   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     `  a�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X      PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     `  �x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  Ȑ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` 
�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X (   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` /`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X L�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` qx   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   q�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   w�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   }�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20210620032542  20220426232406  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_022                 8138_008904_022                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @�}Õ1�@�}Õ1�11  @�}û/�W@�}û/�W@,�vt�c5@,�vt�c5�d�����d����11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?k�?��H@B�\@�  @��R@��R@޸RA   A��A   A,(�A@  A`  A�  A�Q�A�Q�A��A�  A�  A߮A�  B (�B  B  B  B   B'�
B0  B8(�B@  BH  BP  BX  B`  Bg�
Bp(�Bx(�B�(�B�  B��
B��
B�  B�  B�  B�  B�  B�  B�  B�  B��B�  B�  B�=qB�{B�{B��B��B��B��B��B��B�  B�{B�  B��B�  B�  B�  B�{B��C��C  C��C�C
  C  C  C
=C{C  C  C
=C  C�C�C   C!��C#��C&  C(  C*  C+��C.  C0
=C1��C3�C6  C8{C:  C;��C=��C?��CA�CC��CE��CH  CJ  CK��CM��CP  CQ��CS��CV  CX  CZ  C\
=C^  C_�Ca��Cd  Cf  Ch  Ci��Cl  Cm��Co��Cq��Ct  Cv  Cw�Cy��C|
=C~  C�  C�C�  C���C�C�C�  C�C�  C�  C�  C�C�  C���C���C���C�  C�  C�  C�  C�C�C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C�  C�C�C���C�  C���C���C�  C�  C�C�  C���C���C�  C�C�C�C�  C���C�  C�  C�C�C�  C�C�C�  C���C�  C�
=C�C���C���C�  C�C�C�  C���C���C�  C�  C�C�C�  C���C���C�  C�C�  C�  C�  C���C���C���C�  C�  C���C���C�  C�  C�C�  C�  C���C���C�C�  C�  C�  C���C���C�  C�C�  C���C���C�  C�  C���C�  C�C�  C�  C�  C�  C���C���C�  C���C���C�  C�D �D }qD  D��D  D� D�qD}qD  D��D�D��D�qDz�D�qD� D�D� D�qD	}qD	�qD
}qD  D��D  Dz�D�qD� D�qD}qD�qD}qD��D}qD  D��D�D�D�D� D�qD}qD�qDz�D  D� D�D��D  D}qD  D� D�D�D  D� D  D� D  D}qD�qD}qD�qD}qD�qD � D!�D!��D!�qD"z�D"�qD#� D$  D$}qD$��D%}qD&  D&� D'�D'}qD(  D(��D)�D)��D*�D*� D*�qD+� D,�D,� D,�qD-}qD.�D.��D/  D/� D0�D0� D0�qD1� D2  D2� D2�qD3� D4�D4��D5  D5z�D5�qD6� D7�D7� D8  D8� D9  D9� D:  D:� D;  D;��D<  D<� D=  D=� D>  D>}qD?  D?��D@  D@� DA  DA� DB  DB� DB��DC}qDD�DD� DE  DE� DF  DF� DF�qDG��DH  DH� DI  DI��DJ�DJ� DK  DK}qDL  DL��DM�DM� DN  DN� DO�DO}qDP  DP��DP�qDQ� DRDR��DS�DS� DS�qDT}qDU  DU��DV�DV� DW�DW��DX�DX��DY�DY}qDZ  DZ}qD[  D[� D\  D\}qD\�qD]��D^  D^� D_�D_� D`  D`��Da�Da� Db�Db��Dc  Dc}qDc��Ddz�Dd�qDe}qDe�qDf}qDg  Dg� Dg�qDh}qDi�Di� Dj  Dj��Dk�Dk}qDl  Dl� Dm  Dm��Dn  Dnz�Dn�qDo� Do�qDp� Dq�Dq� Dq��Drz�Dr��Ds}qDt  Dt� Du  Du��Dv  Dv� Dw�Dw��Dx�Dx��Dy  Dyz�Dy�qDz� D{  D{� D{�qD|}qD}�D}��D~�D~� D  D��D�HD�@ D�~�D���D�  D�AHD�� D���D�  D�@ D�~�D�� D�HD�@ D�}qD�� D�HD�AHD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�AHD�� D���D���D�@ D�� D��HD�HD�AHD��HD�� D�HD�B�D��HD�� D���D�@ D��HD�� D���D�>�D�� D�� D�  D�AHD�� D��qD��qD�>�D�� D�� D���D�>�D�� D�D�HD�@ D�� D�� D�HD�B�D��HD�� D�  D�AHD�~�D�� D�HD�>�D�� D��HD�HD�>�D�� D��HD���D�>�D�~�D���D���D�>�D��HD��HD�  D�>�D�~�D�� D�  D�>�D�~�D�� D���D�@ D��HD�� D�  D�AHD���D��HD�  D�>�D�~�D���D�  D�AHD�� D��qD�  D�B�D���D�� D�HD�B�D��HD��HD�  D�AHD��HD�� D�  D�AHD�� D��qD���D�>�D��HD��HD�  D�>�D��HD��HD�HD�AHD�� D�� D�HD�AHD��HD���D���D�>�D��HD��HD�  D�@ D�� D��HD���D�@ D���D��HD�  D�@ D���D��HD�HD�AHD���D��HD�HD�@ D��HD��HD�  D�@ D�~�D���D���D�@ D��HD��HD���D�AHD�� D���D�  D�>�D�}qD�� D�HD�AHD��HD�D�HD�AHD���D��HD�HD�@ D�~�D���D���D�>�D�~�D�� D���D�@ D�� D�� D�HD�@ D�~�D���D���D�@ D�� D���D�HD�AHD��HD��HD��D�AHD�� D�� D�  D�AHD�� D���D�  D�AHD�~�D���D���D�>�D�� D�� D�  D�AHD�� D���D�HD�@ D�� D��HD�HD�@ D��HD�� D�  D�@ D�~�D�� D���D�>�D�� D�� D���D�>�D�~�D¾�D�  D�@ D�~�Dþ�D�  D�@ DĀ D��HD�  D�>�D�~�Dž�D�HD�AHD�~�D�� D�  D�@ D�~�D�� D��D�AHDȁHD�� D���D�>�DɁHD�� D�  D�@ D�~�D��HD�HD�B�Dˀ D˾�D���D�>�D�~�D�� D���D�@ D́HD;�D�  D�AHD΀ Dξ�D�  D�@ D�~�D�� D�HD�@ DЀ D��HD�HD�@ Dр D�� D�HD�AHD�}qDҾ�D�HD�AHD�~�DӾ�D�  D�>�D�~�D�� D�  D�>�DՀ D�� D���D�@ Dր D־�D���D�@ D׀ D��HD�  D�>�D؀ D��HD�  D�=qDـ D�D�HD�@ D�~�DڽqD���D�AHDۀ D۾�D�  D�@ D�~�D�� D��qD�@ D݀ Dݾ�D���D�@ D�~�D�� D���D�@ D߀ D��HD�HD�@ D�~�D�� D�HD�AHD�~�D�� D�  D�@ D�HD⾸D�  D�@ D�HD��HD���D�@ D� D�� D�  D�=qD�~�D��HD�HD�@ D�HD�� D���D�>�D�~�D�� D�  D�@ D� D�� D�  D�B�D�aH>�?��?aG�?�\)?�{?��?�ff@�@z�@#�
@0��@@  @Q�@^�R@p��@z�H@�ff@���@�z�@��H@��
@���@�33@�@��R@��@���@�33@��H@�G�@�=q@��@���AG�A�A��A��A�A�A=qA{A"�\A&ffA+�A.�RA3�
A7�A<��AAG�AE�AI��AN{AR�\AVffA\(�A`  Ae�AhQ�Amp�AqG�AvffAz�HA\)A��A�(�A�ffA�Q�A��HA��A�
=A�G�A��A�{A�  A�=qA�z�A�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ?k�?��H@B�\@�  @��R@��R@޸RA   A��A   A,(�A@  A`  A�  A�Q�A�Q�A��A�  A�  A߮A�  B (�B  B  B  B   B'�
B0  B8(�B@  BH  BP  BX  B`  Bg�
Bp(�Bx(�B�(�B�  B��
B��
B�  B�  B�  B�  B�  B�  B�  B�  B��B�  B�  B�=qB�{B�{B��B��B��B��B��B��B�  B�{B�  B��B�  B�  B�  B�{B��C��C  C��C�C
  C  C  C
=C{C  C  C
=C  C�C�C   C!��C#��C&  C(  C*  C+��C.  C0
=C1��C3�C6  C8{C:  C;��C=��C?��CA�CC��CE��CH  CJ  CK��CM��CP  CQ��CS��CV  CX  CZ  C\
=C^  C_�Ca��Cd  Cf  Ch  Ci��Cl  Cm��Co��Cq��Ct  Cv  Cw�Cy��C|
=C~  C�  C�C�  C���C�C�C�  C�C�  C�  C�  C�C�  C���C���C���C�  C�  C�  C�  C�C�C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C�  C�C�C���C�  C���C���C�  C�  C�C�  C���C���C�  C�C�C�C�  C���C�  C�  C�C�C�  C�C�C�  C���C�  C�
=C�C���C���C�  C�C�C�  C���C���C�  C�  C�C�C�  C���C���C�  C�C�  C�  C�  C���C���C���C�  C�  C���C���C�  C�  C�C�  C�  C���C���C�C�  C�  C�  C���C���C�  C�C�  C���C���C�  C�  C���C�  C�C�  C�  C�  C�  C���C���C�  C���C���C�  C�D �D }qD  D��D  D� D�qD}qD  D��D�D��D�qDz�D�qD� D�D� D�qD	}qD	�qD
}qD  D��D  Dz�D�qD� D�qD}qD�qD}qD��D}qD  D��D�D�D�D� D�qD}qD�qDz�D  D� D�D��D  D}qD  D� D�D�D  D� D  D� D  D}qD�qD}qD�qD}qD�qD � D!�D!��D!�qD"z�D"�qD#� D$  D$}qD$��D%}qD&  D&� D'�D'}qD(  D(��D)�D)��D*�D*� D*�qD+� D,�D,� D,�qD-}qD.�D.��D/  D/� D0�D0� D0�qD1� D2  D2� D2�qD3� D4�D4��D5  D5z�D5�qD6� D7�D7� D8  D8� D9  D9� D:  D:� D;  D;��D<  D<� D=  D=� D>  D>}qD?  D?��D@  D@� DA  DA� DB  DB� DB��DC}qDD�DD� DE  DE� DF  DF� DF�qDG��DH  DH� DI  DI��DJ�DJ� DK  DK}qDL  DL��DM�DM� DN  DN� DO�DO}qDP  DP��DP�qDQ� DRDR��DS�DS� DS�qDT}qDU  DU��DV�DV� DW�DW��DX�DX��DY�DY}qDZ  DZ}qD[  D[� D\  D\}qD\�qD]��D^  D^� D_�D_� D`  D`��Da�Da� Db�Db��Dc  Dc}qDc��Ddz�Dd�qDe}qDe�qDf}qDg  Dg� Dg�qDh}qDi�Di� Dj  Dj��Dk�Dk}qDl  Dl� Dm  Dm��Dn  Dnz�Dn�qDo� Do�qDp� Dq�Dq� Dq��Drz�Dr��Ds}qDt  Dt� Du  Du��Dv  Dv� Dw�Dw��Dx�Dx��Dy  Dyz�Dy�qDz� D{  D{� D{�qD|}qD}�D}��D~�D~� D  D��D�HD�@ D�~�D���D�  D�AHD�� D���D�  D�@ D�~�D�� D�HD�@ D�}qD�� D�HD�AHD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�AHD�� D���D���D�@ D�� D��HD�HD�AHD��HD�� D�HD�B�D��HD�� D���D�@ D��HD�� D���D�>�D�� D�� D�  D�AHD�� D��qD��qD�>�D�� D�� D���D�>�D�� D�D�HD�@ D�� D�� D�HD�B�D��HD�� D�  D�AHD�~�D�� D�HD�>�D�� D��HD�HD�>�D�� D��HD���D�>�D�~�D���D���D�>�D��HD��HD�  D�>�D�~�D�� D�  D�>�D�~�D�� D���D�@ D��HD�� D�  D�AHD���D��HD�  D�>�D�~�D���D�  D�AHD�� D��qD�  D�B�D���D�� D�HD�B�D��HD��HD�  D�AHD��HD�� D�  D�AHD�� D��qD���D�>�D��HD��HD�  D�>�D��HD��HD�HD�AHD�� D�� D�HD�AHD��HD���D���D�>�D��HD��HD�  D�@ D�� D��HD���D�@ D���D��HD�  D�@ D���D��HD�HD�AHD���D��HD�HD�@ D��HD��HD�  D�@ D�~�D���D���D�@ D��HD��HD���D�AHD�� D���D�  D�>�D�}qD�� D�HD�AHD��HD�D�HD�AHD���D��HD�HD�@ D�~�D���D���D�>�D�~�D�� D���D�@ D�� D�� D�HD�@ D�~�D���D���D�@ D�� D���D�HD�AHD��HD��HD��D�AHD�� D�� D�  D�AHD�� D���D�  D�AHD�~�D���D���D�>�D�� D�� D�  D�AHD�� D���D�HD�@ D�� D��HD�HD�@ D��HD�� D�  D�@ D�~�D�� D���D�>�D�� D�� D���D�>�D�~�D¾�D�  D�@ D�~�Dþ�D�  D�@ DĀ D��HD�  D�>�D�~�Dž�D�HD�AHD�~�D�� D�  D�@ D�~�D�� D��D�AHDȁHD�� D���D�>�DɁHD�� D�  D�@ D�~�D��HD�HD�B�Dˀ D˾�D���D�>�D�~�D�� D���D�@ D́HD;�D�  D�AHD΀ Dξ�D�  D�@ D�~�D�� D�HD�@ DЀ D��HD�HD�@ Dр D�� D�HD�AHD�}qDҾ�D�HD�AHD�~�DӾ�D�  D�>�D�~�D�� D�  D�>�DՀ D�� D���D�@ Dր D־�D���D�@ D׀ D��HD�  D�>�D؀ D��HD�  D�=qDـ D�D�HD�@ D�~�DڽqD���D�AHDۀ D۾�D�  D�@ D�~�D�� D��qD�@ D݀ Dݾ�D���D�@ D�~�D�� D���D�@ D߀ D��HD�HD�@ D�~�D�� D�HD�AHD�~�D�� D�  D�@ D�HD⾸D�  D�@ D�HD��HD���D�@ D� D�� D�  D�=qD�~�D��HD�HD�@ D�HD�� D���D�>�D�~�D�� D�  D�@ D� D�� D�  D�B�G�O�>�?��?aG�?�\)?�{?��?�ff@�@z�@#�
@0��@@  @Q�@^�R@p��@z�H@�ff@���@�z�@��H@��
@���@�33@�@��R@��@���@�33@��H@�G�@�=q@��@���AG�A�A��A��A�A�A=qA{A"�\A&ffA+�A.�RA3�
A7�A<��AAG�AE�AI��AN{AR�\AVffA\(�A`  Ae�AhQ�Amp�AqG�AvffAz�HA\)A��A�(�A�ffA�Q�A��HA��A�
=A�G�A��A�{A�  A�=qA�z�A�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A�(�A��A�"�A��/AՑhA՟�AՉ7A��#A�r�A�K�A�7LA�&�A��A�bA�
=A�A���A���A���A���A��A��A��A��yA��mA��`A��TA��HA��;A��/A��
A���A�ƨA���AӸRAӰ!Aӧ�Aӛ�AӋDA�|�A�hsA�^5A�`BA�\)A�XA�S�A�K�A�E�A�9XA�1'A�&�A�{A҉7A�hsA�I�AΑhA��/A��yA�ƨA��A�n�A�A���A�=qA��A��/A��DA��PA�hsA���A�E�A�K�A�hsA�A�\)A���A��TA�9XA�ĜA�`BA��A�VA���A�p�A�bNA�^5A�=qA��A�`BA��RA��A� �A���A��HA��jA���A���A�-A�M�A��/A��mA�A�A���A���A���A��uA��wA���A��hA�x�A{t�Ax��AvĜAt1'Aq��Ap�+An��AlM�Ag�-Ad�AdffAd{AbQ�A]�AY�AW|�ATbAP��AM%AJ�AI��AIAG/AD��AC�PAA|�A?dZA>A=A;�A;?}A:�uA9;dA8bNA6�/A6bNA5x�A5VA4�A4v�A3�A2�uA25?A1t�A0ZA.ĜA-��A-l�A,�uA+��A+l�A+hsA+O�A+/A*��A*��A*��A*v�A*I�A*E�A*$�A*A)��A)��A)7LA(�HA(��A'��A'C�A&�!A&v�A%��A%&�A$��A#��A"v�A"{A!��A!?}A!%A {A?}A�HA��A �A&�AJA��AA�uAJA �AZA��A��A��A7LA��A�DA�\Az�Av�A^5A�A�hA�7AK�A��AVAbA�AoA��A9XAAC�A�A�+A�mA�A�wA��A
=A	��Ap�A�A�/A�RA|�A~�A�A^5AAl�A  A�A1'A�A�;A �`A �DA  �@��@��T@���@�C�@�
=@�n�@��^@�@���@��7@��@��@�Q�@�9X@��@�o@�v�@�X@�j@�@���@�ff@���@�/@�r�@��@��y@�x�@�j@�Z@��;@�33@�R@���@�j@���@�w@�S�@�ȴ@�%@���@�9@�S�@�l�@�"�@�=q@�7@�S�@�\)@���@⟾@⟾@��@�%@���@�(�@߾w@�dZ@�
=@��@�ff@���@ݡ�@�G�@�r�@��@���@��@�@�dZ@�S�@�J@�z�@��
@�33@�~�@�$�@�5?@�=q@�J@�@�x�@��@Դ9@��;@��y@җ�@��@�@�x�@�V@���@Ь@Ѓ@���@���@·+@Ͳ-@�X@̼j@��@�t�@�C�@��@�v�@�E�@��@�J@��#@�?}@��@��@��@�  @�t�@�
=@��@��@���@Ə\@��@���@Ł@��@��`@ă@�1@Ý�@�dZ@�"�@��@+@�5?@���@�hs@��`@�r�@�  @���@�t�@�ȴ@�5?@�J@�@�&�@�Ĝ@�9X@���@���@�t�@�o@�~�@���@�`B@�&�@�%@���@��j@�bN@�1'@�ƨ@���@�;d@��R@��+@�^5@�$�@�@���@�O�@���@�9X@�b@���@��
@���@�K�@��\@�E�@��@���@���@�O�@�bN@� �@���@�dZ@���@���@�M�@���@�p�@�/@���@�Ĝ@�Z@�b@���@��w@�+@�ȴ@�v�@��@��-@�x�@���@��9@�9X@��@���@�
=@�ȴ@�~�@�^5@�{@��#@��@��@��u@�9X@��@��
@�t�@�\)@�"�@���@�$�@�@�X@��@���@� �@���@��@��@�\)@�+@��@��T@���@��-@���@�p�@�7L@�%@���@�bN@�I�@� �@���@��P@�\)@�C�@�33@�"�@��@���@�$�@��#@�@��7@�G�@���@�I�@�b@�ƨ@�S�@�ȴ@�V@�$�@��@�{@��@�@�7L@�&�@�Z@��;@��@���@�K�@�@���@�v�@�E�@��@�@��@���@��-@�p�@�G�@�7L@���@��D@�I�@�  @���@��w@��P@�+@���@��\@�E�@�{@��T@�@��-@�x�@�X@�?}@�&�@��@���@��@�bN@� �@��@�ƨ@���@�+@���@���@��R@���@�v�@�E�@�$�@��@���@���@��h@�&�@���@�r�@�Q�@�I�@�b@��@�C�@��y@��R@�n�@��@��^@��h@�O�@��/@��j@��@��D@�z�@� �@�t�@�+@�o@��@���@��R@�~�@�V@��@���@��7@�%@��j@���@�z�@��@�P@+@~��@~�y@~�@~�+@~v�@~ff@~$�@}��@}O�@|�j@|9X@{�
@{��@{�@{C�@{o@z�!@z-@y��@x�`@w�P@v��@v5?@u��@u�-@u�@u`B@u/@t��@t�@s��@s"�@rJ@q��@p��@p  @o�;@o�w@o
=@n��@n@m��@m/@l9X@k��@k��@k�m@k�
@k�
@kƨ@k��@k��@k�@ko@j��@j�@i�7@i&�@h��@h��@h��@hr�@hQ�@hA�@h  @g��@g�@f�@f{@e��@e?}@eV@d9X@d�@ct�@b-@aX@`  @_\)@_;d@_�@^ȴ@^��@^{@]�h@]V@\�j@\(�@[ƨ@[S�@[C�@[@Z�\@Z~�@Zn�@Z-@Y�7@X��@X�u@Xb@W�@W|�@W+@V�y@V��@V$�@U�h@U/@Tz�@S��@St�@S"�@R�!@R^5@RM�@Q��@Q�7@QX@QX@QG�@P��@Pr�@P1'@P �@O�@O�;@O�w@O�@O|�@OK�@O�@N�y@Nȴ@N��@NE�@M�-@L�j@L9X@K��@K�
@K�F@K��@K�@KdZ@KS�@K33@Ko@J�!@J~�@J=q@J-@I��@IX@I�@H�`@HĜ@HbN@G��@GK�@Fȴ@FE�@F@E��@EO�@EV@D��@DI�@D9X@D1@C�
@CdZ@B�!@A�@A&�@@�`@@Q�@?�w@?;d@>��@>ff@>@=@=�@=p�@<��@<��@<z�@<Z@<(�@;�@;"�@;o@:�@:�!@:^5@:-@:J@9��@9�7@9&�@8�`@8��@8bN@8Q�@8Q�@8 �@7�@7l�@7�@6�y@6�+@6@5��@5O�@4�j@4��@4j@4I�@4(�@41@41@41@3�F@3dZ@2�@2~�@2^5@2M�@2�@1�#@1�^@1x�@1%@0�9@01'@/�@/\)@/+@.��@.ȴ@.�+@.5?@.$�@.{@.@-�@-@-��@-�@-?}@,�/@,�j@,�@,�D@,1@+�F@+��@+��@+��@+@*=q@)��@)�7@)X@)&�@(��@(bN@(A�@(  @'�@'�@'\)@';d@'+@'�@'
=@&��@&�y@&ȴ@&ȴ@&�R@&��@&v�@&{@&@%�T@%�-@%�h@%?}@%V@$��@$j@#��@#��@#dZ@#C�@#o@#@"�@"�H@"�H@"��@"�!@"��@"�\@"~�@"=q@!�@!��@ �9@ bN@  �@��@�@�@�A�1'A�/A�33A�/A�&�A�-A�-A�&�A�&�A�/A�+A�%A�bA�%A� �A�"�A�+A�"�A��A���A�  Aՙ�A՗�A՟�AՑhAՋDAՍPAգ�A՗�A՟�Aե�Aէ�A՗�A՗�A�\)A�O�A�JAԶFAԛ�AԑhA�jA�r�A�ZA�K�A�G�A�E�A�A�A�;dA�7LA�/A�1'A�+A�-A�(�A�(�A�$�A�&�A�$�A�$�A�"�A��A��A��A��A�{A��A�{A��A�bA�bA�VA�VA�JA�VA�
=A�JA�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               A�/A�(�A��A�"�A��/AՑhA՟�AՉ7A��#A�r�A�K�A�7LA�&�A��A�bA�
=A�A���A���A���A���A��A��A��A��yA��mA��`A��TA��HA��;A��/A��
A���A�ƨA���AӸRAӰ!Aӧ�Aӛ�AӋDA�|�A�hsA�^5A�`BA�\)A�XA�S�A�K�A�E�A�9XA�1'A�&�A�{A҉7A�hsA�I�AΑhA��/A��yA�ƨA��A�n�A�A���A�=qA��A��/A��DA��PA�hsA���A�E�A�K�A�hsA�A�\)A���A��TA�9XA�ĜA�`BA��A�VA���A�p�A�bNA�^5A�=qA��A�`BA��RA��A� �A���A��HA��jA���A���A�-A�M�A��/A��mA�A�A���A���A���A��uA��wA���A��hA�x�A{t�Ax��AvĜAt1'Aq��Ap�+An��AlM�Ag�-Ad�AdffAd{AbQ�A]�AY�AW|�ATbAP��AM%AJ�AI��AIAG/AD��AC�PAA|�A?dZA>A=A;�A;?}A:�uA9;dA8bNA6�/A6bNA5x�A5VA4�A4v�A3�A2�uA25?A1t�A0ZA.ĜA-��A-l�A,�uA+��A+l�A+hsA+O�A+/A*��A*��A*��A*v�A*I�A*E�A*$�A*A)��A)��A)7LA(�HA(��A'��A'C�A&�!A&v�A%��A%&�A$��A#��A"v�A"{A!��A!?}A!%A {A?}A�HA��A �A&�AJA��AA�uAJA �AZA��A��A��A7LA��A�DA�\Az�Av�A^5A�A�hA�7AK�A��AVAbA�AoA��A9XAAC�A�A�+A�mA�A�wA��A
=A	��Ap�A�A�/A�RA|�A~�A�A^5AAl�A  A�A1'A�A�;A �`A �DA  �@��@��T@���@�C�@�
=@�n�@��^@�@���@��7@��@��@�Q�@�9X@��@�o@�v�@�X@�j@�@���@�ff@���@�/@�r�@��@��y@�x�@�j@�Z@��;@�33@�R@���@�j@���@�w@�S�@�ȴ@�%@���@�9@�S�@�l�@�"�@�=q@�7@�S�@�\)@���@⟾@⟾@��@�%@���@�(�@߾w@�dZ@�
=@��@�ff@���@ݡ�@�G�@�r�@��@���@��@�@�dZ@�S�@�J@�z�@��
@�33@�~�@�$�@�5?@�=q@�J@�@�x�@��@Դ9@��;@��y@җ�@��@�@�x�@�V@���@Ь@Ѓ@���@���@·+@Ͳ-@�X@̼j@��@�t�@�C�@��@�v�@�E�@��@�J@��#@�?}@��@��@��@�  @�t�@�
=@��@��@���@Ə\@��@���@Ł@��@��`@ă@�1@Ý�@�dZ@�"�@��@+@�5?@���@�hs@��`@�r�@�  @���@�t�@�ȴ@�5?@�J@�@�&�@�Ĝ@�9X@���@���@�t�@�o@�~�@���@�`B@�&�@�%@���@��j@�bN@�1'@�ƨ@���@�;d@��R@��+@�^5@�$�@�@���@�O�@���@�9X@�b@���@��
@���@�K�@��\@�E�@��@���@���@�O�@�bN@� �@���@�dZ@���@���@�M�@���@�p�@�/@���@�Ĝ@�Z@�b@���@��w@�+@�ȴ@�v�@��@��-@�x�@���@��9@�9X@��@���@�
=@�ȴ@�~�@�^5@�{@��#@��@��@��u@�9X@��@��
@�t�@�\)@�"�@���@�$�@�@�X@��@���@� �@���@��@��@�\)@�+@��@��T@���@��-@���@�p�@�7L@�%@���@�bN@�I�@� �@���@��P@�\)@�C�@�33@�"�@��@���@�$�@��#@�@��7@�G�@���@�I�@�b@�ƨ@�S�@�ȴ@�V@�$�@��@�{@��@�@�7L@�&�@�Z@��;@��@���@�K�@�@���@�v�@�E�@��@�@��@���@��-@�p�@�G�@�7L@���@��D@�I�@�  @���@��w@��P@�+@���@��\@�E�@�{@��T@�@��-@�x�@�X@�?}@�&�@��@���@��@�bN@� �@��@�ƨ@���@�+@���@���@��R@���@�v�@�E�@�$�@��@���@���@��h@�&�@���@�r�@�Q�@�I�@�b@��@�C�@��y@��R@�n�@��@��^@��h@�O�@��/@��j@��@��D@�z�@� �@�t�@�+@�o@��@���@��R@�~�@�V@��@���@��7@�%@��j@���@�z�@��@�P@+@~��@~�y@~�@~�+@~v�@~ff@~$�@}��@}O�@|�j@|9X@{�
@{��@{�@{C�@{o@z�!@z-@y��@x�`@w�P@v��@v5?@u��@u�-@u�@u`B@u/@t��@t�@s��@s"�@rJ@q��@p��@p  @o�;@o�w@o
=@n��@n@m��@m/@l9X@k��@k��@k�m@k�
@k�
@kƨ@k��@k��@k�@ko@j��@j�@i�7@i&�@h��@h��@h��@hr�@hQ�@hA�@h  @g��@g�@f�@f{@e��@e?}@eV@d9X@d�@ct�@b-@aX@`  @_\)@_;d@_�@^ȴ@^��@^{@]�h@]V@\�j@\(�@[ƨ@[S�@[C�@[@Z�\@Z~�@Zn�@Z-@Y�7@X��@X�u@Xb@W�@W|�@W+@V�y@V��@V$�@U�h@U/@Tz�@S��@St�@S"�@R�!@R^5@RM�@Q��@Q�7@QX@QX@QG�@P��@Pr�@P1'@P �@O�@O�;@O�w@O�@O|�@OK�@O�@N�y@Nȴ@N��@NE�@M�-@L�j@L9X@K��@K�
@K�F@K��@K�@KdZ@KS�@K33@Ko@J�!@J~�@J=q@J-@I��@IX@I�@H�`@HĜ@HbN@G��@GK�@Fȴ@FE�@F@E��@EO�@EV@D��@DI�@D9X@D1@C�
@CdZ@B�!@A�@A&�@@�`@@Q�@?�w@?;d@>��@>ff@>@=@=�@=p�@<��@<��@<z�@<Z@<(�@;�@;"�@;o@:�@:�!@:^5@:-@:J@9��@9�7@9&�@8�`@8��@8bN@8Q�@8Q�@8 �@7�@7l�@7�@6�y@6�+@6@5��@5O�@4�j@4��@4j@4I�@4(�@41@41@41@3�F@3dZ@2�@2~�@2^5@2M�@2�@1�#@1�^@1x�@1%@0�9@01'@/�@/\)@/+@.��@.ȴ@.�+@.5?@.$�@.{@.@-�@-@-��@-�@-?}@,�/@,�j@,�@,�D@,1@+�F@+��@+��@+��@+@*=q@)��@)�7@)X@)&�@(��@(bN@(A�@(  @'�@'�@'\)@';d@'+@'�@'
=@&��@&�y@&ȴ@&ȴ@&�R@&��@&v�@&{@&@%�T@%�-@%�h@%?}@%V@$��@$j@#��@#��@#dZ@#C�@#o@#@"�@"�H@"�H@"��@"�!@"��@"�\@"~�@"=q@!�@!��@ �9@ bN@  �@��@�@�G�O�A�1'A�/A�33A�/A�&�A�-A�-A�&�A�&�A�/A�+A�%A�bA�%A� �A�"�A�+A�"�A��A���A�  Aՙ�A՗�A՟�AՑhAՋDAՍPAգ�A՗�A՟�Aե�Aէ�A՗�A՗�A�\)A�O�A�JAԶFAԛ�AԑhA�jA�r�A�ZA�K�A�G�A�E�A�A�A�;dA�7LA�/A�1'A�+A�-A�(�A�(�A�$�A�&�A�$�A�$�A�"�A��A��A��A��A�{A��A�{A��A�bA�bA�VA�VA�JA�VA�
=A�JA�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
HB
G�B
H�B
E�B
H�B
F?B
DgB
F?B
GB
FB
D�B
D�B
DgB
D�B
D�B
D�B
D�B
EB
E9B
EmB
E9B
EmB
E�B
E�B
E�B
F?B
FtB
GEB
G�B
IRB
J�B
N<B
QB
S�B
UgB
X�B
ZB
\)B
_;B
cTB
f�B
j�B
k�B
l�B
m�B
n�B
o B
n�B
m�B
m�B
l�B
k�B
i�B
ZQB
C�B
:^B
@�B
Z�B
rGB
�hB
��B
��B
��B
�mB
ʌB
�NB�B�B�BB.B33B;�BN�B\�BjBiDBm]BpoBq�Bv�Bw2BzB}�B|�B��B��B�B�4B��B�\B�hB��By>B��B�@B��B��Bx�Be`BYB^jB<�BkB
�B
�;B
��B
~�B
]/B
H�B
(�B
\B	��B	�QB	ӏB	�6B	��B	�B	��B	�4B	�~B	��B	�B	{�B	o5B	S�B	E�B	7�B	'�B	�B	�B	�B	1B	JB	�B	�B	$B	)�B	.}B	9$B	EB	B�B	A B	^5B	n�B	�B	�B	��B	�?B	��B	ĜB	�B	�/B	�B	�B	�8B	��B	��B	�	B	��B	��B
fB
�B
�B
�B
�B
�B
$@B
+�B
1�B
9$B
C�B
M�B
T�B
YKB
]/B
\�B
\]B
]�B
\�B
[�B
YB
W
B
R B
MjB
K^B
C�B
AUB
A�B
DgB
C�B
B�B
:�B
8B
9XB
7�B
:*B
5B
5?B
.}B
,B
)�B
1[B
<6B
?HB
>�B
>wB
B�B
D3B
GzB
N�B
P�B
Q�B
R�B
TaB
U2B
XyB
Y�B
YB
X�B
^5B
`�B
b�B
`B
_�B
^jB
[�B
YKB
U�B
QNB
FtB
=�B
7B
%B
hB	�ZB	��B	��B	�ZB	�B	�yB	�B	��B	�oB	��B	�B	��B	�%B	��B	�fB	�TB	�AB	�;B	�B	� B	�oB	��B	�B	��B	�B	��B	�	B	��B	��B	��B	��B	��B	�B	��B	��B	�VB	��B	��B	��B	�B	��B	�|B	�B	�oB	�5B	�vB	�B	�;B	�iB	��B	�"B	�B	��B	�B	�"B	�B	�WB	�/B	�B	��B	�2B	�B	��B	��B	��B
MB
_B
�B
�B
+B
	�B
�B
�B
�B
�B
�B
+B
�B
�B
�B
�B
�B
SB
�B
  B
�B
�B
+B
�B
�B
 �B	�.B	��B	��B	��B	��B
 �B
�B
�B
YB
�B
SB
MB
B
B
�B
�B
�B
�B
�B
�B
�B
YB
�B
 iB	��B	�VB	��B	�VB	��B	��B	��B	�]B	��B	�cB
oB
{B
{B
�B
SB
�B
_B
�B
	B
	�B
	�B
	lB
	lB
	�B
	7B
	�B
	�B
	7B
	lB

	B

�B
B
xB
xB
xB
xB
xB

�B
B

�B

�B

	B
	�B
	�B
	7B
�B
�B
	7B
	B
	�B
	lB
	lB
	�B
	lB

	B

rB

�B

�B

�B

�B
B
DB
B
B
B
�B
B
B
B
JB
JB
�B
PB
�B
�B
�B
�B
PB
PB
�B
�B
VB
VB
"B
�B
�B
�B
�B
�B
(B
.B
�B
�B
.B
�B
�B
 B
�B
�B
hB
4B
hB
B
oB
�B
�B
B
B
uB
uB
B
FB
�B
B
MB
�B
MB
B
SB
�B
�B
�B
�B
�B
�B
�B
_B
+B
�B
�B
7B
7B
7B
�B
kB
CB
xB
B
CB
xB
~B
IB
~B
�B
�B
�B
OB
OB
�B
B
�B
OB
B
OB
B
�B
~B
IB
~B
~B
B
�B
�B
�B
�B
B
�B
B
OB
OB
!B
�B
�B
�B
�B
�B
�B
VB
�B
!�B
 �B
 �B
 �B
!bB
!�B
!�B
"4B
"4B
"�B
"�B
"�B
"�B
"�B
#B
#B
"�B
#�B
$B
#�B
$�B
$tB
$@B
%B
%zB
%�B
%�B
%�B
&B
&LB
&LB
&�B
'B
'RB
'B
'�B
'�B
'�B
'�B
(XB
(�B
(�B
(�B
)*B
)�B
)�B
*0B
)�B
*0B
*eB
*0B
*0B
)�B
*0B
*eB
*eB
+B
+�B
+�B
+�B
+kB
,=B
-B
.B
.B
.IB
.�B
/OB
/OB
/OB
/�B
0�B
0UB
0!B
0�B
/�B
0�B
1[B
1�B
1�B
1�B
1�B
1�B
2-B
2-B
2�B
33B
33B
4nB
4�B
4�B
4�B
5�B
6FB
6FB
6zB
6FB
6FB
6�B
6�B
6zB
6�B
6�B
6�B
7�B
7�B
8RB
8RB
8RB
8�B
8�B
8�B
8�B
8�B
7�B
8�B
8�B
8�B
9�B
9XB
9�B
9�B
9�B
:�B
:�B
<�B
=<B
>�B
>BB
?�B
?}B
?�B
?�B
@�B
@�B
AUB
A�B
A�B
B�B
B�B
B[B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C-B
C-B
C�B
DgB
D3B
D�B
DgB
D�B
DgB
D�B
D3B
D�B
D3B
D3B
E9B
E�B
EmB
E9B
EB
F?B
E�B
F?B
GEB
F�B
IRB
H�B
H�B
H�B
IRB
IB
I�B
JXB
J�B
J�B
K�B
K�B
L�B
LdB
L�B
MB
MB
L�B
MB
M�B
M�B
M�B
NB
NB
NB
NpB
NpB
N�B
OB
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
RTB
R�B
S&B
S&B
R�B
R�B
S�B
S�B
S�B
S�B
T,B
S�B
T,B
TaB
TaB
TaB
T�B
T�B
T�B
T�B
T�B
U2B
U�B
U�B
VB
U�B
VB
V9B
V9B
VmB
VmB
VmB
V�B
V�B
V�B
W
B
V�B
W�B
W?B
WsB
W�B
W?B
XB
XEB
XyB
YKB
Y�B
Y�B
ZQB
Z�B
Z�B
Z�B
[#B
Z�B
Z�B
Z�B
[WB
[�B
\�B
]dB
\�B
]�B
]�B
^jB
^�B
^�B
_;B
^�B
_B
_B
_pB
_�B
_�B
_�B
_�B
`�B
aHB
a|B
aHB
a�B
bB
a�B
bB
a�B
bNB
b�B
b�B
b�B
c B
b�B
b�B
c B
cTB
cTB
c�B
c�B
dZB
dZB
dZB
e`B
e�B
e`B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
gB
g8B
gmB
g8B
g�B
g�B
g�B
h
B
hsB
h>B
iB
iyB
iyB
i�B
i�B
jB
jB
j�B
j�B
j�B
j�B
j�B
kB
kB
kQB
k�B
l"B
l"B
k�B
k�B
l�B
lWB
lWB
lWB
l"B
l�B
m)B
m]B
m]B
n/B
ncB
oiB
oiB
oiB
o�B
p;B
p;B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
p�B
p�B
p�B
qAB
qvB
qAB
qvB
q�B
qvB
rGB
rGB
r�B
r|B
s�B
sMB
s�B
s�B
s�B
tB
s�B
tB
tB
tB
tB
tB
s�B
tB
tTB
t�B
tTB
u%B
t�B
u%B
u�B
u�B
u�B
u�B
G�B
GzB
GzB
GEB
IB
F?B
HB
H�B
F�B
E�B
H�B
M6B
GEB
F�B
A�B
HKB
EB
G�B
C�B
3hB
J#B
L�B
F�B
DgB
G�B
F?B
@�B
CaB
F?B
A B
D�B
CaB
E9B
A�B
OB
C�B
E�B
IRB
F�B
D3B
H�B
C-B
D�B
D3B
D3B
D�B
C�B
E9B
C�B
EmB
C�B
EB
C�B
D�B
C�B
D�B
C�B
D�B
C�B
DgB
EmB
D3B
EmB
C�B
E�B
C�B
E9B
C�B
EB
DgB
EB
C�B
EB
C�B
EmB
C�B
EmG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               B
H+B
G�B
HpB
H�B
J�B
F�B
G B
L�B
J�B
G�B
E�B
EB
D�B
D�B
D�B
D�B
D�B
E$B
EMB
E�B
EZB
E�B
E�B
E�B
E�B
FRB
F�B
GYB
G�B
IiB
J�B
NmB
Q_B
S�B
U�B
X�B
Z^B
\�B
_�B
c�B
gB
kB
k�B
l�B
m�B
n�B
o9B
n�B
nPB
m�B
mB
l�B
l|B
]�B
F�B
?DB
EvB
]VB
xQB
�@B
�RB
��B
�B
ʒB
βB
�UBB	'B�B�B7B;6BJ@Be�Bl8Bo�BnBq�Bs�Bt�B{~By�B}�B�B}MB�B�+B�XB��B��B�iB��B��BB��B��B��B��B�TBi�Bb�By�BX�B$�B
�eB
��B
�[B
�B
nB
YdB
<B
PB	�^B	��B	��B	�5B	�[B	��B	�#B	��B	��B	�<B	��B	�B	|�B	\�B	SB	E"B	5�B	�B	�B	B	�B	!B	lB	)B	+�B	/wB	2�B	=�B	F�B	EiB	FB	aFB	s�B	��B	�aB	�$B	��B	��B	�B	�(B	ުB	��B	�UB	�B	�XB	�/B	�&B	�B	�|B
�B
TB
!B
sB
�B
iB
%�B
,9B
1�B
9�B
DB
NB
UZB
[DB
^^B
^B
_B
_�B
_B
\�B
[^B
YcB
TAB
P�B
O�B
E=B
B�B
CB
EoB
GB
E�B
<AB
9MB
;NB
;�B
=�B
6 B
7�B
0HB
-�B
)�B
0�B
=TB
?�B
?�B
@8B
DB
EB
G�B
N�B
QB
R>B
T�B
U�B
U�B
YB
[�B
Z�B
Z3B
`7B
b�B
deB
a�B
a�B
`KB
]XB
Z�B
XaB
UZB
J�B
A�B
>�B
1BB
`B	�:B	��B	��B	��B	�B	��B	�5B	�mB	�DB	��B	��B
 �B	�4B	��B	��B	��B	�B	��B	�B	�B	��B	�lB	��B	�B	�VB	�5B	��B	��B	��B	�MB	�&B	�+B	�vB	��B	�B	�SB	��B	�>B	�B	�B	�B	�]B	��B	�B	�B	�6B	�B	�B	�B	�GB	�B	�B	�B	�~B	�ZB	�bB	�%B	��B	�HB	��B	�3B	��B	��B	�xB	�gB
)B
EB
�B
UB
�B
�B
�B
 B
�B
�B
	gB
�B
�B
 B
)B
	�B

B
=B
bB	��B
9B
DB
�B

�B

KB
�B
 �B	��B	��B	��B	��B
\B
�B
�B
GB
�B
qB
�B
B
JB
oB
YB
�B
\B
CB
lB
�B
�B
B
bB	��B	��B	�"B	��B	�"B	��B	�>B	��B	� B	�QB
�B
�B
�B
�B
�B
	(B
fB
�B
	5B
	�B
	�B
	�B

;B

/B
	�B

�B

=B
	�B

PB

�B
B
�B
�B
;B
B
sB
�B
�B
�B
�B
MB

B

�B

�B
	�B
	dB
	�B
	�B
	�B

B
	�B

B

fB

{B
EB
DB
IB
B
B
B
�B
�B
�B
wB
�B
�B
�B
sB
�B
�B
�B
�B
�B
�B
CB
�B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
RB
�B
wB
}B
?B
YB
JB
uB
kB
1B
�B
�B
�B
�B
B
eB
�B
�B
B
B
rB
�B
B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
KB
�B
TB
�B
�B
?B
�B
mB
B
�B
BB
�B
8B
�B
�B
�B
B
�B
B
~B
�B
 B
3B
B
�B
5B
LB
gB
�B
�B
B
�B
[B
B
�B
�B
JB
�B
�B
9B
zB
dB
�B
�B
�B
�B
tB
�B
 B
�B
�B
�B
�B
B
 B
�B
!�B
"�B
!zB
!	B
!�B
"#B
"�B
"jB
"�B
"�B
"�B
"�B
# B
"�B
#9B
#gB
#FB
#�B
$�B
$�B
$�B
%B
$�B
$�B
&B
&lB
&<B
&�B
&cB
&�B
&�B
&�B
'	B
'oB
'�B
'jB
(B
(B
'�B
(�B
(�B
)B
)*B
)NB
*B
*?B
*\B
*pB
*CB
*�B
*�B
*�B
*RB
*SB
*�B
+B
+oB
,-B
,B
+�B
+�B
,	B
-)B
.B
.�B
.�B
/B
/B
0*B
/�B
0B
0�B
1B
0�B
0rB
0�B
0�B
2;B
2B
1�B
1�B
1�B
2B
2B
2�B
2�B
3xB
3�B
4`B
5B
4�B
5B
5�B
6fB
6�B
6�B
6�B
6hB
6�B
6�B
6�B
6�B
7B
7SB
7�B
8B
8'B
8�B
8�B
8�B
8�B
9B
9`B
9�B
9�B
9.B
:B
9mB
9gB
9�B
9�B
9�B
:B
:B
;;B
;�B
=�B
>�B
?SB
?kB
@�B
?�B
@/B
@�B
A~B
A�B
A�B
B3B
CB
CEB
C B
BzB
CB
B�B
B�B
C B
B�B
B�B
C'B
C�B
C�B
D{B
D�B
DvB
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E8B
F'B
F4B
E�B
E�B
E�B
F�B
F�B
G�B
HgB
H:B
JB
H�B
I B
I!B
I�B
I�B
JeB
J�B
KoB
K�B
LtB
LNB
L�B
L�B
MKB
MB
MB
MB
M�B
NOB
N$B
N'B
NkB
NBB
N^B
N�B
N�B
O1B
O�B
PNB
P�B
QkB
QoB
RB
R`B
RAB
R�B
R�B
R�B
SWB
S.B
SB
SNB
TB
TB
TB
T+B
TBB
TB
TFB
T�B
T�B
T�B
T�B
T�B
UB
U)B
U�B
V#B
VSB
VB
V*B
U�B
V'B
VPB
V^B
V�B
V�B
V�B
WB
WB
WB
W*B
W]B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
YB
Y�B
ZB
Z.B
Z�B
Z�B
Z�B
[IB
[CB
[/B
[=B
[�B
\8B
\�B
]lB
]�B
]�B
^zB
^vB
_!B
^�B
_B
_�B
_%B
_1B
_�B
_�B
`B
_�B
`+B
`�B
aNB
aeB
a�B
a�B
bB
bXB
bB
bmB
b@B
b�B
b�B
b�B
c5B
c6B
b�B
c2B
c�B
c�B
c�B
c�B
d
B
d�B
d�B
d�B
fB
e�B
e�B
e�B
e�B
f#B
e�B
fB
e�B
fpB
f�B
g�B
geB
g�B
g{B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
j"B
j(B
jpB
j�B
kB
kB
k B
j�B
k'B
kIB
kPB
k�B
k�B
lLB
l?B
l.B
l�B
l�B
loB
lbB
l�B
l�B
m�B
m�B
m�B
m�B
n{B
oB
o�B
o�B
o�B
p/B
pNB
p�B
q B
p�B
p�B
p�B
p�B
p�B
p�B
qB
p�B
qB
p�B
q�B
q�B
qrB
q�B
q�B
q�B
r�B
r�B
s^B
sB
s�B
s�B
s�B
s�B
tB
t7B
tB
t&B
t:B
tIB
t8B
t8B
tB
txB
t�B
uB
u\B
u�B
uKB
u�B
u�B
u�B
u�G�O�B
G�B
GzB
GzB
GEB
IB
F?B
HB
H�B
F�B
E�B
H�B
M6B
GEB
F�B
A�B
HKB
EB
G�B
C�B
3hB
J#B
L�B
F�B
DgB
G�B
F?B
@�B
CaB
F?B
A B
D�B
CaB
E9B
A�B
OB
C�B
E�B
IRB
F�B
D3B
H�B
C-B
D�B
D3B
D3B
D�B
C�B
E9B
C�B
EmB
C�B
EB
C�B
D�B
C�B
D�B
C�B
D�B
C�B
DgB
EmB
D3B
EmB
C�B
E�B
C�B
E9B
C�B
EB
DgB
EB
C�B
EB
C�B
EmB
C�B
EmG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Lx�<#�
<#�
<vZ�<���<��g<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<r�<N�a<m%A<��)<;Z<#�
<#�
<#�
<#�
<���<X��<#�
<,�(<�!�<偞<8��<#�
<�Y�=(L<�6<��J<�?�<��-</�a<#�
<'L�<%��<#�
<#�
<.L�<���<*��<#�
<#�
<#�
<�`�<c�-<#�
<_5�<fG^<j|j<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<PV�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021062003254220210620032542IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021063004004120210630040041QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021063004004120210630040041QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364820220422133648IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064420220426160644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064420220426160644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064420220426160644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                