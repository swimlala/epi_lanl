CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-07-20T05:20:31Z creation; 2022-04-26T16:06:59Z DMQC;      
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
_FillValue        G�O�     P  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  ZX   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     P  a�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  ~�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     P  �P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     P  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     P  �D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     P  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     P 
8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T '�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     P .�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T L,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     P S�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` p�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   q0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   w0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   }0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210720052031  20220426232407  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_025                 8138_008904_025                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @مH"��@@مH"��@11  @مHPH�@مHPH�@,[�#c�@,[�#c��d�l�&k��d�l�&k�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�\@B�\@}p�@��R@�p�@�p�@��RA  A\)A,(�A@��A`��A�Q�A�Q�A��A�\)A��A�  A�Q�A�Q�A��B�
B(�B  B   B(  B/�
B8  B@  BH(�BP(�BX  B`  Bg�
Bp  Bx(�B�{B�{B�{B�  B��B��B��B��B�  B�  B�  B�(�B�(�B�{B�{B�  B��B��B�  B�Q�B�  B�  B�  B�{B�  B�{B�{B�{B�  B�  B�(�B�  B��
C  C
=C  C  C	��C��C��C��C��C�C��C  C
=C  C��C 
=C"  C#��C&  C(
=C*
=C,{C.
=C0{C2{C4
=C5�C7��C9�C;��C=�C?��CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT
=CV  CW��CY��C\  C^  C`  Cb
=Cd  Cf  Ch  Cj  Cl  Cn
=Cp  Cr  Ct  Cv  Cx
=Cz
=C|  C}��C�  C�  C���C�  C�  C�  C�  C�  C�C�C�  C�  C�  C���C���C���C�  C�C�
=C�  C���C�  C�
=C�
=C�C�  C���C���C���C���C�  C�C�  C�  C�  C�  C�  C�C�  C���C���C���C���C�C�C�  C�  C���C�  C�C�C�  C�  C�  C�C�C�
=C�C���C���C�C�  C�C�  C�  C�C���C���C���C�C���C���C���C���C���C���C���C�  C�C�  C�  C�  C�  C�C�  C���C�  C�  C�  C���C���C�  C�C�C�  C�  C�C�  C�C�C�C�  C���C���C�  C�C�  C�  C���C���C���C�  C�  C���C�  C�  C�C�C�
=C�  C�  C�  C���C���C���C���C���C�  D �D ��D�D� D  D� D�qD}qD�qD� D�D� D��D� D  D}qD�qD}qD�qD	� D
  D
� D�D}qD  D��D  D}qD�qD� D�D��D  D� D  D� D  D��D�qDz�D��D}qD�qD� D  D}qD  D��D  Dz�D�qD� D  D��DD� D��D}qD�qD� D�D��D  D}qD �D � D!  D!}qD"  D"� D#  D#� D$�D$� D%  D%��D&  D&� D&�qD'��D(�D(� D)  D)}qD)��D*z�D*��D+� D,  D,��D-�D-� D-�qD.z�D.��D/z�D0  D0��D1  D1� D2�D2� D3  D3� D4  D4��D5�D5��D6�D6� D7  D7� D7�qD8� D9�D9� D9�qD:� D:�qD;��D<  D<}qD=  D=��D>  D>� D?  D?� D@�D@}qD@�qDA}qDA�qDB}qDB�qDC� DD�DD��DE�DE� DF  DF� DG  DG� DH  DH� DI  DI��DJ  DJ}qDK  DK� DL  DL� DM  DM� DM�qDN� DO�DO��DP�DP��DQ�DQ� DR  DR��DS  DS� DT�DT� DT��DU� DV�DV� DV�qDW}qDW�qDX}qDX�qDY� DZ  DZ}qDZ�qD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`}qD`�qDa��Db  Db� Db�qDc� Dd�Dd��De�De� De�qDf}qDf�qDg� Dh�Dh� Dh�qDi� Dj�Dj�Dk�Dk��Dl  Dl� Dl�qDm}qDn  Dn� Dn�qDo}qDp  Dp� Dq  Dq� Dr  Dr}qDs  Ds��DtDt� Du  Du��Dv  Dv}qDv�qDwz�Dx  Dx�DyDy�Dz�Dz� Dz�qD{� D|�D|� D}  D}� D~  D~� D  D� D�  D�@ D�~�D�� D�HD�@ D�~�D�� D���D�@ D��HD��HD�  D�@ D��HD�D�  D�=qD�}qD���D���D�@ D��HD�� D���D�@ D�~�D��HD�HD�>�D�}qD�� D�HD�AHD�� D��qD���D�AHD��HD�� D�  D�AHD��HD�D��D�@ D�}qD���D�  D�@ D��HD�� D�  D�@ D�� D�� D�  D�>�D�}qD�� D�HD�AHD���D�� D���D�>�D��HD��HD�HD�AHD��HD���D�  D�B�D��HD�� D���D�=qD�� D��HD�HD�B�D���D�D�  D�=qD�}qD�� D��D�@ D�}qD��qD���D�>�D�~�D���D�  D�@ D�� D���D���D�@ D�� D���D���D�>�D�� D�D��D�B�D���D�D�HD�AHD��HD�� D���D�@ D��HD��HD�HD�@ D�~�D�� D�HD�>�D�� D��HD�  D�>�D�� D�� D�  D�AHD��HD�� D�HD�AHD�� D�� D�  D�AHD��HD��HD�HD�@ D�� D��HD�  D�AHD��HD�� D�  D�>�D�� D�� D���D�>�D�� D�� D�HD�@ D�� D�� D�HD�@ D�� D�� D���D�@ D�� D�� D�HD�AHD���D��HD�  D�>�D�� D��HD�HD�@ D��HD�� D�  D�@ D�~�D�� D���D�@ D��HD��HD�  D�>�D�~�D�� D�  D�>�D�� D��HD�  D�@ D�~�D���D�  D�@ D��HD��HD���D�>�D�~�D���D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�@ D�~�D�� D�HD�@ D��HD�� D�HD�@ D�� D�� D�  D�>�D�~�D�� D�HD�AHD�� D��HD��D�B�D��HD�� D�HD�B�D�� D�� D��qD�@ D��HD�� D�  D�@ D�� D���D�  D�>�D�� D���D�  D�@ D��HD�� D�HD�>�D D�� D���D�>�DÁHD�� D���D�>�DĀ D�D�  D�>�Dŀ D�� D�HD�@ D�~�Dƾ�D���D�@ DǁHD�� D��qD�@ DȀ D��HD�HD�B�Dɀ Dɾ�D��qD�@ Dʀ D��HD�HD�@ Dˀ D˾�D��qD�>�D�~�D̽qD���D�AHD́HD�� D���D�>�D΀ Dξ�D�  D�@ DρHD�� D�  D�@ DЀ Dо�D�  D�B�Dр D�� D�  D�AHDҁHD��HD���D�@ DӀ D�� D���D�@ DԁHD�� D�  D�AHD�}qD�� D�  D�@ Dր D�� D�  D�>�D׀ D�� D�HD�AHD؀ D�� D�HD�@ D�~�Dپ�D�  D�@ Dڀ Dھ�D���D�@ DہHD��HD�HD�@ D܀ Dܾ�D���D�@ D݂�D�D�HD�AHDށHD��HD�  D�@ D߀ D߾�D���D�@ D�~�DྸD�  D�B�D�HD��HD�HD�AHD� D�� D�  D�@ D� D�� D�  D�@ D� D��HD�HD�AHD�HD��HD�HD�>�D� D�� D��qD�@ D�HD�� D�  D�>�D�~�D辸D��)?\)?8Q�?k�?�z�?���?�p�?��?�?�@
=q@
=@�R@.{@8Q�@B�\@L��@W
=@fff@n{@z�H@��\@��@���@�z�@�(�@��\@�ff@�\)@�z�@��H@�G�@Ǯ@У�@�
=@�(�@��@���@�33@��HAG�AA��Ap�AG�A�A��A(�A!G�A$z�A(Q�A,(�A0  A4z�A8Q�A:�HA@  AC�
AG�AL(�AP  ATz�AW�A[�A`  Ac�
Ag�Al(�An�RAs�
Aw�A{�A\)A��A�z�A�A�  A��\A�z�A��RA���A��HA��A�
=A�G�A�33A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ?�=q@�\@B�\@}p�@��R@�p�@�p�@��RA  A\)A,(�A@��A`��A�Q�A�Q�A��A�\)A��A�  A�Q�A�Q�A��B�
B(�B  B   B(  B/�
B8  B@  BH(�BP(�BX  B`  Bg�
Bp  Bx(�B�{B�{B�{B�  B��B��B��B��B�  B�  B�  B�(�B�(�B�{B�{B�  B��B��B�  B�Q�B�  B�  B�  B�{B�  B�{B�{B�{B�  B�  B�(�B�  B��
C  C
=C  C  C	��C��C��C��C��C�C��C  C
=C  C��C 
=C"  C#��C&  C(
=C*
=C,{C.
=C0{C2{C4
=C5�C7��C9�C;��C=�C?��CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT
=CV  CW��CY��C\  C^  C`  Cb
=Cd  Cf  Ch  Cj  Cl  Cn
=Cp  Cr  Ct  Cv  Cx
=Cz
=C|  C}��C�  C�  C���C�  C�  C�  C�  C�  C�C�C�  C�  C�  C���C���C���C�  C�C�
=C�  C���C�  C�
=C�
=C�C�  C���C���C���C���C�  C�C�  C�  C�  C�  C�  C�C�  C���C���C���C���C�C�C�  C�  C���C�  C�C�C�  C�  C�  C�C�C�
=C�C���C���C�C�  C�C�  C�  C�C���C���C���C�C���C���C���C���C���C���C���C�  C�C�  C�  C�  C�  C�C�  C���C�  C�  C�  C���C���C�  C�C�C�  C�  C�C�  C�C�C�C�  C���C���C�  C�C�  C�  C���C���C���C�  C�  C���C�  C�  C�C�C�
=C�  C�  C�  C���C���C���C���C���C�  D �D ��D�D� D  D� D�qD}qD�qD� D�D� D��D� D  D}qD�qD}qD�qD	� D
  D
� D�D}qD  D��D  D}qD�qD� D�D��D  D� D  D� D  D��D�qDz�D��D}qD�qD� D  D}qD  D��D  Dz�D�qD� D  D��DD� D��D}qD�qD� D�D��D  D}qD �D � D!  D!}qD"  D"� D#  D#� D$�D$� D%  D%��D&  D&� D&�qD'��D(�D(� D)  D)}qD)��D*z�D*��D+� D,  D,��D-�D-� D-�qD.z�D.��D/z�D0  D0��D1  D1� D2�D2� D3  D3� D4  D4��D5�D5��D6�D6� D7  D7� D7�qD8� D9�D9� D9�qD:� D:�qD;��D<  D<}qD=  D=��D>  D>� D?  D?� D@�D@}qD@�qDA}qDA�qDB}qDB�qDC� DD�DD��DE�DE� DF  DF� DG  DG� DH  DH� DI  DI��DJ  DJ}qDK  DK� DL  DL� DM  DM� DM�qDN� DO�DO��DP�DP��DQ�DQ� DR  DR��DS  DS� DT�DT� DT��DU� DV�DV� DV�qDW}qDW�qDX}qDX�qDY� DZ  DZ}qDZ�qD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`}qD`�qDa��Db  Db� Db�qDc� Dd�Dd��De�De� De�qDf}qDf�qDg� Dh�Dh� Dh�qDi� Dj�Dj�Dk�Dk��Dl  Dl� Dl�qDm}qDn  Dn� Dn�qDo}qDp  Dp� Dq  Dq� Dr  Dr}qDs  Ds��DtDt� Du  Du��Dv  Dv}qDv�qDwz�Dx  Dx�DyDy�Dz�Dz� Dz�qD{� D|�D|� D}  D}� D~  D~� D  D� D�  D�@ D�~�D�� D�HD�@ D�~�D�� D���D�@ D��HD��HD�  D�@ D��HD�D�  D�=qD�}qD���D���D�@ D��HD�� D���D�@ D�~�D��HD�HD�>�D�}qD�� D�HD�AHD�� D��qD���D�AHD��HD�� D�  D�AHD��HD�D��D�@ D�}qD���D�  D�@ D��HD�� D�  D�@ D�� D�� D�  D�>�D�}qD�� D�HD�AHD���D�� D���D�>�D��HD��HD�HD�AHD��HD���D�  D�B�D��HD�� D���D�=qD�� D��HD�HD�B�D���D�D�  D�=qD�}qD�� D��D�@ D�}qD��qD���D�>�D�~�D���D�  D�@ D�� D���D���D�@ D�� D���D���D�>�D�� D�D��D�B�D���D�D�HD�AHD��HD�� D���D�@ D��HD��HD�HD�@ D�~�D�� D�HD�>�D�� D��HD�  D�>�D�� D�� D�  D�AHD��HD�� D�HD�AHD�� D�� D�  D�AHD��HD��HD�HD�@ D�� D��HD�  D�AHD��HD�� D�  D�>�D�� D�� D���D�>�D�� D�� D�HD�@ D�� D�� D�HD�@ D�� D�� D���D�@ D�� D�� D�HD�AHD���D��HD�  D�>�D�� D��HD�HD�@ D��HD�� D�  D�@ D�~�D�� D���D�@ D��HD��HD�  D�>�D�~�D�� D�  D�>�D�� D��HD�  D�@ D�~�D���D�  D�@ D��HD��HD���D�>�D�~�D���D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�@ D�~�D�� D�HD�@ D��HD�� D�HD�@ D�� D�� D�  D�>�D�~�D�� D�HD�AHD�� D��HD��D�B�D��HD�� D�HD�B�D�� D�� D��qD�@ D��HD�� D�  D�@ D�� D���D�  D�>�D�� D���D�  D�@ D��HD�� D�HD�>�D D�� D���D�>�DÁHD�� D���D�>�DĀ D�D�  D�>�Dŀ D�� D�HD�@ D�~�Dƾ�D���D�@ DǁHD�� D��qD�@ DȀ D��HD�HD�B�Dɀ Dɾ�D��qD�@ Dʀ D��HD�HD�@ Dˀ D˾�D��qD�>�D�~�D̽qD���D�AHD́HD�� D���D�>�D΀ Dξ�D�  D�@ DρHD�� D�  D�@ DЀ Dо�D�  D�B�Dр D�� D�  D�AHDҁHD��HD���D�@ DӀ D�� D���D�@ DԁHD�� D�  D�AHD�}qD�� D�  D�@ Dր D�� D�  D�>�D׀ D�� D�HD�AHD؀ D�� D�HD�@ D�~�Dپ�D�  D�@ Dڀ Dھ�D���D�@ DہHD��HD�HD�@ D܀ Dܾ�D���D�@ D݂�D�D�HD�AHDށHD��HD�  D�@ D߀ D߾�D���D�@ D�~�DྸD�  D�B�D�HD��HD�HD�AHD� D�� D�  D�@ D� D�� D�  D�@ D� D��HD�HD�AHD�HD��HD�HD�>�D� D�� D��qD�@ D�HD�� D�  D�>�D�~�D辸G�O�?\)?8Q�?k�?�z�?���?�p�?��?�?�@
=q@
=@�R@.{@8Q�@B�\@L��@W
=@fff@n{@z�H@��\@��@���@�z�@�(�@��\@�ff@�\)@�z�@��H@�G�@Ǯ@У�@�
=@�(�@��@���@�33@��HAG�AA��Ap�AG�A�A��A(�A!G�A$z�A(Q�A,(�A0  A4z�A8Q�A:�HA@  AC�
AG�AL(�AP  ATz�AW�A[�A`  Ac�
Ag�Al(�An�RAs�
Aw�A{�A\)A��A�z�A�A�  A��\A�z�A��RA���A��HA��A�
=A�G�A�33A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aُ\Aُ\Aُ\AٍPAُ\Aُ\AًDAى7AٍPAّhAّhAُ\Aُ\A�x�A�M�A�E�A�A�A�oA��HA���A�ƨA�ĜA���AؼjAغ^AظRAظRAضFAضFAضFAضFAضFAش9Aز-Aذ!Aذ!Aذ!Aذ!AجAأ�Aؙ�A؋DA�x�A�
=A�?}A֙�AԑhA�^5A���A�x�A�(�A�ȴA�l�AάA�C�A��/A�S�Ḁ�A��A˛�A��A�A��AɶFA��A��A�l�A���A�S�A�9XA�%A�
=A�A�A��+A�ĜA���A�t�A�O�A�I�A�^5A�dZA�r�A�(�A��A��yA���A�1'A�l�A��jA���A�Q�A�n�A�~�A�dZA��A��A��`A�+A���A}�Av-Ao7LAjz�Ah^5AgO�AfAdI�AcVAbbNAa�
A` �A];dAUXAS�TAQ�#AM�^AH��AG��AGAF9XAA��A?C�A=hsA<��A9��A7K�A6Q�A6bA5\)A2�9A0�`A0�yA1O�A0v�A-G�A,�!A,�A,�HA,r�A+�A+oA*I�A)K�A(�9A(�+A)�mA)�A)�TA)��A(��A(Q�A'�A'A&z�A%x�A$ffA$9XA#�A"�/A"A�A!�A!+A �HA ��A ^5A�AhsAS�A��AffA��A��A33AȴAjA��A\)A�yA��Av�A�TA�A��AbNA�FA^5A�#A�-A��At�A+A��A��AZA-A��A�FA�AS�A"�A�A�AVA��A��A�9A��A~�A33A(�A�-A�AbNA��A�HA�yAffA�AO�A&�AbA
��A
r�A
A�yA��A��A	�A��A�!AE�AoA�A|�At�AhsA+A��AM�A �AJA��At�A�A��A�yAbA\)A ��A �!A n�A @�l�@���@�E�@��-@�  @�ff@���@���@��9@�bN@��@��P@�+@�V@��^@���@��7@�p�@�hs@���@�ƨ@�C�@�/@��H@�X@��@��m@���@�X@��@��@�P@�|�@�dZ@�+@���@�V@���@�&�@�I�@㝲@�K�@��@�+@�^5@�=q@��@��@��#@�7L@߮@�\)@��@�ȴ@�n�@�5?@�$�@�@���@ݙ�@�hs@��`@��;@�"�@ڰ!@�$�@�O�@ج@��H@�~�@�-@�@ա�@Ցh@�x�@���@���@ԃ@��@���@�|�@�@ҸR@җ�@�v�@�5?@�@Ѻ^@�&�@�z�@��@�C�@Ο�@���@́@��@��`@̣�@˕�@�o@���@��@Ɂ@�X@��@���@ȋD@�Q�@�1@Ǯ@�t�@�o@�n�@��@�X@�&�@ģ�@öF@�o@�~�@��^@�7L@���@�Q�@���@���@�\)@�o@��T@�G�@��@�%@���@���@�Ĝ@�z�@�Q�@�I�@�1'@�b@�  @��
@��@�M�@�J@��T@��@�Q�@�Q�@�I�@�9X@��;@��P@��@��R@���@�v�@�=q@��#@���@�@���@��@�O�@�&�@�%@���@��@���@���@��R@�V@��7@��D@�9X@�1@��@��;@��@���@�$�@�@���@��T@�`B@��`@��m@�S�@�"�@��@�@���@��\@�-@�@��@��@��T@���@���@���@�@��@�I�@�ƨ@�dZ@���@���@�=q@���@��T@��@� �@��;@�ƨ@��F@��@�@���@�ff@�$�@��7@��D@� �@���@��@��@�~�@�$�@��#@��^@��@�O�@�&�@���@���@���@�j@�A�@��@��@�\)@�$�@�x�@�V@�%@��@��`@�Ĝ@���@��D@�z�@�Q�@�A�@�9X@�1@���@��@��@��R@���@���@��\@�-@�/@��/@��u@�z�@�Z@�I�@�9X@��@��m@���@�K�@�
=@��!@�@�hs@��9@�z�@�Z@�|�@���@��R@��R@��R@�5?@���@�p�@��@��/@�Ĝ@���@�z�@�Q�@�(�@�1@��m@���@��F@���@�l�@��@���@�n�@��@���@�X@���@��@���@�K�@��!@��\@�V@�M�@�5?@�$�@��@�J@�J@�J@�J@�@��@��^@�x�@�G�@��@���@�Ĝ@���@�1'@�t�@�o@���@�v�@�M�@�E�@�E�@�$�@�J@��@��^@��@�p�@�O�@���@���@��@��@�I�@� �@�  @l�@~�y@~�@~�R@~�+@}��@|Z@{o@zn�@y�@y��@y��@yhs@yG�@y7L@y&�@y�@xĜ@xbN@x1'@w��@w��@v�R@u�h@t�D@t�@sƨ@s�F@s�F@s�F@sS�@s"�@r��@r�!@r~�@rM�@r-@r�@q�@qG�@p��@pĜ@p�@pQ�@p  @o��@o��@ol�@n��@n��@nV@m��@lz�@kS�@j-@h��@g�;@g�@g|�@g;d@g
=@f�R@e��@d�j@dI�@c�
@cS�@cC�@c"�@co@co@b�H@bn�@b=q@b�@a�@a�#@a�#@a��@a��@a��@a�^@aX@`A�@_l�@^E�@\��@\j@[��@[��@[t�@[33@Z�H@Z��@Z~�@Zn�@Z^5@ZM�@Y��@X��@X1'@W��@Wl�@V�@VE�@V5?@U�@U�-@Up�@T�@T�@Tj@T�@SdZ@So@R�H@R��@R��@Q�#@Qhs@PĜ@O�@O;d@O
=@N�R@Nv�@NV@NV@NE�@NE�@N{@N{@M�@L��@KdZ@J��@JJ@I��@Ix�@I�@H�@H �@G�@G�;@G�w@G�P@G\)@G;d@G
=@F�@F��@F5?@E�@EO�@D��@Dj@C�
@C��@C�@C33@B�H@B�!@B�\@B=q@A��@Ahs@@�9@@r�@@b@?�;@?�w@?�P@?l�@?K�@?+@?�@?
=@>�y@>�@>�+@=�T@=?}@<�/@<��@<Z@<�@;��@;�F@;33@:~�@9��@9�#@9��@9��@9�^@9��@9�@8��@8�u@8A�@7�@7�P@7�@6V@6@5�T@5�T@5��@5@5�-@5O�@5�@4��@4�/@4�j@4�@4�@4��@4�D@4Z@49X@41@3�m@3t�@3o@1��@0�u@0bN@01'@0 �@0  @/�@/�@/��@/�P@/;d@/
=@.��@.�@.�+@.V@.5?@.$�@.@-`B@,�@,��@,Z@+�
@+��@+t�@+"�@*�!@*~�@*n�@*M�@*�@)�@)��@)x�@)hs@)G�@)7L@)%@(�@(1'@'�@'K�@&�@&��@&v�@&@%�@%�@%��@%��@%�h@%�@%p�@%?}@$��@$�/@$�j@$I�@#�
@#ƨ@#�F@#��@#dZ@#S�@#o@"�H@"�H@"�H@"��@"��@"��@"J@!��@!��@!��@!hs@!G�@!�@ ��@ �`@ ��@ Ĝ@ �u@ A�@�@K�@�@ff@$�@$�@{@O�@z�@(�@33@��@��@��@�\@^5@-@J@J@J@��AّhAٍPAّhAًDAّhAٍPAّhAُ\AٍPAّhAٍPAّhAُ\AٍPAُ\AٍPAُ\AّhAٍPAُ\AًDAُ\Aُ\AٍPAّhAُ\AّhAّhAٍPAٓuAٍPAُ\AّhAٍPAٍPAه+Aه+AًDAى7Aى7AًDAًDAُ\AٍPAُ\AّhAُ\AّhAُ\AٓuAّhAُ\AٓuAٍPAٍPAّhAٍPAٓuAّhAُ\AٓuAٍPAُ\Aُ\AٍPAّhAى7Aى7Aه+AمAٍPAه+A�p�A�r�A�bNA�ZA�A�A�=qA�I�A�M�A�`BA�Q�A�G�A�M�A�K�A�G�A�I�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Aُ\Aُ\Aُ\AٍPAُ\Aُ\AًDAى7AٍPAّhAّhAُ\Aُ\A�x�A�M�A�E�A�A�A�oA��HA���A�ƨA�ĜA���AؼjAغ^AظRAظRAضFAضFAضFAضFAضFAش9Aز-Aذ!Aذ!Aذ!Aذ!AجAأ�Aؙ�A؋DA�x�A�
=A�?}A֙�AԑhA�^5A���A�x�A�(�A�ȴA�l�AάA�C�A��/A�S�Ḁ�A��A˛�A��A�A��AɶFA��A��A�l�A���A�S�A�9XA�%A�
=A�A�A��+A�ĜA���A�t�A�O�A�I�A�^5A�dZA�r�A�(�A��A��yA���A�1'A�l�A��jA���A�Q�A�n�A�~�A�dZA��A��A��`A�+A���A}�Av-Ao7LAjz�Ah^5AgO�AfAdI�AcVAbbNAa�
A` �A];dAUXAS�TAQ�#AM�^AH��AG��AGAF9XAA��A?C�A=hsA<��A9��A7K�A6Q�A6bA5\)A2�9A0�`A0�yA1O�A0v�A-G�A,�!A,�A,�HA,r�A+�A+oA*I�A)K�A(�9A(�+A)�mA)�A)�TA)��A(��A(Q�A'�A'A&z�A%x�A$ffA$9XA#�A"�/A"A�A!�A!+A �HA ��A ^5A�AhsAS�A��AffA��A��A33AȴAjA��A\)A�yA��Av�A�TA�A��AbNA�FA^5A�#A�-A��At�A+A��A��AZA-A��A�FA�AS�A"�A�A�AVA��A��A�9A��A~�A33A(�A�-A�AbNA��A�HA�yAffA�AO�A&�AbA
��A
r�A
A�yA��A��A	�A��A�!AE�AoA�A|�At�AhsA+A��AM�A �AJA��At�A�A��A�yAbA\)A ��A �!A n�A @�l�@���@�E�@��-@�  @�ff@���@���@��9@�bN@��@��P@�+@�V@��^@���@��7@�p�@�hs@���@�ƨ@�C�@�/@��H@�X@��@��m@���@�X@��@��@�P@�|�@�dZ@�+@���@�V@���@�&�@�I�@㝲@�K�@��@�+@�^5@�=q@��@��@��#@�7L@߮@�\)@��@�ȴ@�n�@�5?@�$�@�@���@ݙ�@�hs@��`@��;@�"�@ڰ!@�$�@�O�@ج@��H@�~�@�-@�@ա�@Ցh@�x�@���@���@ԃ@��@���@�|�@�@ҸR@җ�@�v�@�5?@�@Ѻ^@�&�@�z�@��@�C�@Ο�@���@́@��@��`@̣�@˕�@�o@���@��@Ɂ@�X@��@���@ȋD@�Q�@�1@Ǯ@�t�@�o@�n�@��@�X@�&�@ģ�@öF@�o@�~�@��^@�7L@���@�Q�@���@���@�\)@�o@��T@�G�@��@�%@���@���@�Ĝ@�z�@�Q�@�I�@�1'@�b@�  @��
@��@�M�@�J@��T@��@�Q�@�Q�@�I�@�9X@��;@��P@��@��R@���@�v�@�=q@��#@���@�@���@��@�O�@�&�@�%@���@��@���@���@��R@�V@��7@��D@�9X@�1@��@��;@��@���@�$�@�@���@��T@�`B@��`@��m@�S�@�"�@��@�@���@��\@�-@�@��@��@��T@���@���@���@�@��@�I�@�ƨ@�dZ@���@���@�=q@���@��T@��@� �@��;@�ƨ@��F@��@�@���@�ff@�$�@��7@��D@� �@���@��@��@�~�@�$�@��#@��^@��@�O�@�&�@���@���@���@�j@�A�@��@��@�\)@�$�@�x�@�V@�%@��@��`@�Ĝ@���@��D@�z�@�Q�@�A�@�9X@�1@���@��@��@��R@���@���@��\@�-@�/@��/@��u@�z�@�Z@�I�@�9X@��@��m@���@�K�@�
=@��!@�@�hs@��9@�z�@�Z@�|�@���@��R@��R@��R@�5?@���@�p�@��@��/@�Ĝ@���@�z�@�Q�@�(�@�1@��m@���@��F@���@�l�@��@���@�n�@��@���@�X@���@��@���@�K�@��!@��\@�V@�M�@�5?@�$�@��@�J@�J@�J@�J@�@��@��^@�x�@�G�@��@���@�Ĝ@���@�1'@�t�@�o@���@�v�@�M�@�E�@�E�@�$�@�J@��@��^@��@�p�@�O�@���@���@��@��@�I�@� �@�  @l�@~�y@~�@~�R@~�+@}��@|Z@{o@zn�@y�@y��@y��@yhs@yG�@y7L@y&�@y�@xĜ@xbN@x1'@w��@w��@v�R@u�h@t�D@t�@sƨ@s�F@s�F@s�F@sS�@s"�@r��@r�!@r~�@rM�@r-@r�@q�@qG�@p��@pĜ@p�@pQ�@p  @o��@o��@ol�@n��@n��@nV@m��@lz�@kS�@j-@h��@g�;@g�@g|�@g;d@g
=@f�R@e��@d�j@dI�@c�
@cS�@cC�@c"�@co@co@b�H@bn�@b=q@b�@a�@a�#@a�#@a��@a��@a��@a�^@aX@`A�@_l�@^E�@\��@\j@[��@[��@[t�@[33@Z�H@Z��@Z~�@Zn�@Z^5@ZM�@Y��@X��@X1'@W��@Wl�@V�@VE�@V5?@U�@U�-@Up�@T�@T�@Tj@T�@SdZ@So@R�H@R��@R��@Q�#@Qhs@PĜ@O�@O;d@O
=@N�R@Nv�@NV@NV@NE�@NE�@N{@N{@M�@L��@KdZ@J��@JJ@I��@Ix�@I�@H�@H �@G�@G�;@G�w@G�P@G\)@G;d@G
=@F�@F��@F5?@E�@EO�@D��@Dj@C�
@C��@C�@C33@B�H@B�!@B�\@B=q@A��@Ahs@@�9@@r�@@b@?�;@?�w@?�P@?l�@?K�@?+@?�@?
=@>�y@>�@>�+@=�T@=?}@<�/@<��@<Z@<�@;��@;�F@;33@:~�@9��@9�#@9��@9��@9�^@9��@9�@8��@8�u@8A�@7�@7�P@7�@6V@6@5�T@5�T@5��@5@5�-@5O�@5�@4��@4�/@4�j@4�@4�@4��@4�D@4Z@49X@41@3�m@3t�@3o@1��@0�u@0bN@01'@0 �@0  @/�@/�@/��@/�P@/;d@/
=@.��@.�@.�+@.V@.5?@.$�@.@-`B@,�@,��@,Z@+�
@+��@+t�@+"�@*�!@*~�@*n�@*M�@*�@)�@)��@)x�@)hs@)G�@)7L@)%@(�@(1'@'�@'K�@&�@&��@&v�@&@%�@%�@%��@%��@%�h@%�@%p�@%?}@$��@$�/@$�j@$I�@#�
@#ƨ@#�F@#��@#dZ@#S�@#o@"�H@"�H@"�H@"��@"��@"��@"J@!��@!��@!��@!hs@!G�@!�@ ��@ �`@ ��@ Ĝ@ �u@ A�@�@K�@�@ff@$�@$�@{@O�@z�@(�@33@��@��@��@�\@^5@-@J@J@JG�O�AّhAٍPAّhAًDAّhAٍPAّhAُ\AٍPAّhAٍPAّhAُ\AٍPAُ\AٍPAُ\AّhAٍPAُ\AًDAُ\Aُ\AٍPAّhAُ\AّhAّhAٍPAٓuAٍPAُ\AّhAٍPAٍPAه+Aه+AًDAى7Aى7AًDAًDAُ\AٍPAُ\AّhAُ\AّhAُ\AٓuAّhAُ\AٓuAٍPAٍPAّhAٍPAٓuAّhAُ\AٓuAٍPAُ\Aُ\AٍPAّhAى7Aى7Aه+AمAٍPAه+A�p�A�r�A�bNA�ZA�A�A�=qA�I�A�M�A�`BA�Q�A�G�A�M�A�K�A�G�A�I�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
33B
2�B
2�B
2�B
2�B
2�B
2�B
2aB
2�B
2aB
2�B
2�B
2�B
2aB
2�B
2aB
1�B
3�B
3�B
3�B
3�B
3�B
4B
4B
3�B
4B
3�B
4B
3�B
4B
3�B
3�B
3�B
3�B
3�B
3�B
3hB
33B
2�B
2aB
1�B
0�B
.B
*�B
 �B
�B
�B	��B	�oB	�B	�DB	�DB
	�B
�B
"�B
)�B
,B
9XB
S�B
d�B
{JB
��B
��B
��B
��B{BC�BVB[�Bk�BkBhsB��B�By	BrBbNBc�B`�B]dBZQBJ�B;dB0�B%�B�B�B�B�B
� B
��B
�B
�TB
�RB
��B
��B
�eB
g�B
F?B
_B	ޞB	��B	�:B	��B	��B	�B	�SB	~]B	y>B	t�B	m)B	c B	R�B	@�B	>B	/OB	,�B	&LB	#B	 �B	(�B	-�B	49B	3�B	7�B	=�B	B�B	K^B	P}B	CaB	8�B	>B	xlB	��B	�	B	��B	�_B	��B	��B	�B	�-B	��B	��B	��B	�B	�DB	�B	�B
DB
�B
qB
�B
 �B
�B
	B
:B
bB
�B
�B
%B
�B	��B	��B	��B	��B	��B	�xB	��B
�B
"B
�B
"�B
8�B
9�B
=qB
A�B
EmB
GEB
GzB
GzB
J#B
J�B
IRB
GB
HKB
F�B
IRB
I�B
IB
IRB
IRB
J�B
K)B
K)B
N�B
PB
T�B
YB
[#B
\�B
\�B
\�B
\�B
\�B
\�B
\]B
[�B
Z�B
ZB
P�B
O�B
I�B
DgB
HKB
M�B
N�B
QNB
K�B
I�B
FtB
A�B
2�B
/�B
/OB
)�B
'�B
-�B
6�B
<B
9�B
5tB
/�B
&LB
&LB
&B
%zB
%�B
%�B
%FB
$�B
&�B
*0B
(�B
,qB
+6B
*0B
+6B
'�B
&LB
$�B
%B
$@B
$B
"�B
"4B
�B
!�B
�B
 �B
�B
�B
OB
IB
B
B
�B
xB
�B
=B
�B
�B
�B
�B
B
YB
�B
�B
oB
:B
B
�B
hB
4B
\B
�B
�B
VB
�B
PB
PB
�B
B
B
�B
�B
B
xB
DB
B

�B

	B
~B
DB

=B

	B

	B
	lB
	7B
�B
�B
	B
	7B
fB
	B
�B
�B
%B
�B
�B
�B
�B
�B
SB
�B
�B
B
�B
B
{B
�B
GB
B
GB
{B
�B
AB
AB
B
oB
�B
uB
�B
B
GB
�B
B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
%B
�B
�B
�B
�B
�B
�B
%B
%B
�B
�B
�B
%B
YB
%B
�B
�B
�B
+B
�B
+B
�B
�B
�B
1B
fB
�B
1B
�B
1B
�B
fB
1B
1B
fB
�B
�B
�B
�B
fB
�B
	lB
	B
	B
�B
�B
	7B
�B

�B
	�B
	�B

	B
	�B

�B

rB

�B

�B
B
xB
B
xB
�B
~B
�B
�B
B
PB
"B
"B
�B
�B
�B
�B
�B
VB
�B
�B
�B
PB
"B
(B
.B
�B
.B
�B
bB
�B
hB
4B
�B
�B
�B
�B
hB
4B
4B
4B
hB
�B
�B
�B
�B
{B
�B
�B
B
SB
YB
$B
�B
�B
�B
�B
�B
+B
�B
�B
B
1B
�B
�B
�B
	B
�B
B
�B
�B
�B
�B
B
CB
xB
�B
CB
B
B
�B
�B
VB
�B
�B
�B
�B
!B
�B
�B
�B
 'B
�B
�B
�B
!-B
!-B
!bB
!bB
!-B
 �B
 �B
!-B
"hB
"�B
#:B
#B
#:B
"�B
"�B
#B
#:B
#�B
#nB
#:B
#nB
$@B
%B
&LB
&B
%zB
(�B
(�B
(�B
(�B
'�B
)*B
)_B
)�B
*eB
*0B
)�B
)�B
*0B
*0B
*eB
*0B
)�B
)�B
)�B
*0B
*0B
*0B
*0B
+B
+�B
,B
+�B
,=B
,B
.IB
.�B
.IB
.�B
/OB
/OB
/�B
/�B
/OB
/B
/B
/B
/�B
/B
.�B
/�B
/�B
/�B
/�B
/�B
0!B
0!B
1[B
1�B
33B
3�B
4B
4B
3�B
3�B
4nB
4�B
5?B
5?B
6B
5tB
5tB
6zB
6�B
7B
7B
7LB
7�B
8B
9$B
8�B
8B
8B
8B
9XB
:�B
:�B
:�B
:�B
;dB
;0B
;0B
;dB
;�B
;�B
;dB
<B
;�B
;�B
;dB
;0B
<�B
=�B
=�B
=�B
=�B
>B
=�B
=�B
>B
>B
>wB
>�B
>�B
>�B
>�B
>BB
>wB
?}B
?}B
?}B
?�B
?�B
?�B
@B
?�B
?�B
?�B
?�B
?�B
@B
@�B
A�B
B[B
D3B
C�B
D3B
DgB
D�B
D�B
D�B
EmB
F?B
F�B
FtB
GEB
GB
GEB
GzB
F�B
GzB
G�B
G�B
G�B
G�B
HB
G�B
HB
GzB
G�B
G�B
G�B
HKB
HKB
I�B
JXB
J�B
J�B
K)B
J�B
K)B
K)B
K^B
K�B
K^B
K^B
J�B
K�B
L0B
L�B
MB
MB
M�B
N<B
M�B
NpB
NpB
OB
OBB
O�B
O�B
PB
P�B
QB
P�B
P�B
PHB
Q�B
QB
RTB
S[B
S�B
S�B
S�B
T,B
TaB
S�B
T,B
S�B
T,B
S[B
S[B
T�B
U�B
VB
V�B
V�B
W
B
WsB
XB
W�B
XEB
XEB
XB
X�B
XyB
X�B
X�B
X�B
YB
YB
YKB
Y�B
ZB
ZB
Z�B
Z�B
Z�B
Z�B
[#B
[WB
[WB
[�B
[�B
\]B
\�B
]/B
]/B
]�B
]dB
]�B
]�B
]�B
]�B
^B
]�B
^B
]�B
^jB
^�B
_;B
_�B
_;B
`B
_�B
`B
_�B
`�B
a|B
a�B
a|B
a�B
a�B
a|B
a�B
bNB
b�B
b�B
b�B
b�B
cTB
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
e`B
e`B
e�B
e`B
e�B
e`B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
ffB
h>B
h�B
h�B
iB
iDB
iB
iyB
iB
iDB
iyB
i�B
jB
i�B
jB
jKB
jB
jKB
jB
jKB
k�B
kB
k�B
k�B
l�B
l"B
l�B
l�B
m]B
m]B
m)B
m�B
m]B
m�B
m�B
ncB
n/B
ncB
m�B
n/B
o B
o5B
o�B
o�B
poB
pB
p;B
p�B
p�B
p�B
qB
qAB
qAB
qB
qAB
qAB
q�B
qvB
q�B
rGB
r�B
rGB
r�B
r�B
r�B
r�B
sMB
sMB
sB
sMB
s�B
sB
s�B
s�B
tB
tTB
tTB
tTB
t�B
t�B
u%B
t�B
u%B
t�B
u%B
u�B
u�B
v�B
v`B
v�B
v�B
v�B
v�B
x8B
x�B
x�B
zB
zxB
zDB
z�B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
33B
3hB
2aB
4B
2�B
33B
1�B
2�B
4B
1�B
33B
2-B
2�B
3�B
2aB
2�B
2�B
2�B
33B
2�B
2�B
2�B
2�B
3hB
1�B
3hB
2�B
2aB
3�B
2-B
3hB
2�B
1�B
3hB
2aB
2�B
3hB
1�B
33B
2-B
1�B
2�B
1�B
2�B
1�B
1�B
2�B
2-B
2�B
2�B
2aB
33B
1�B
33B
2�B
1�B
2�B
1�B
2aB
2�B
1�B
2�B
2aB
1�B
2�B
1[B
4B
2�B
1�B
2�B
0�B
1�B
8RB
,�B
2aB
2aB
<jB
1�B
33B
33B
,�B
4�B
6zB
0�B
1�B
2�B
1�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   B
34B
2�B
2�B
2�B
2�B
2�B
2�B
2DB
2}B
2qB
2�B
2�B
3#B
3GB
2�B
2�B
2�B
4�B
4%B
3�B
3�B
3�B
4#B
4B
3�B
4B
3�B
4B
3�B
4B
3�B
3�B
3�B
3�B
3�B
3�B
3rB
3UB
39B
2�B
2*B
1rB
0�B
/�B
%>B
#SB
aB	��B	�B	�@B	�B
EB
kB
�B
%B
,�B
0B
<�B
UNB
gB
}5B
��B
��B
��B
�2B �BO^B_�Bq�B�Bq�B|�B��B��B��B�	Bn"Bi�Bk�Bl�Bd�BV�BBzB6tB+�B|B\B�B�B
�B
��B
�7B
ؒB
�4B
��B
��B
�eB
sXB
]�B
TB	�~B	��B	�*B	�>B	��B	��B	�B	�B	z�B	x�B	s�B	t#B	VkB	E�B	G#B	:4B	/�B	'�B	%B	*�B	.�B	2$B	5�B	9�B	>B	?�B	C�B	M$B	V$B	G�B	9)B	=�B	y�B	��B	��B	��B	�iB	��B	�B	�ZB	�5B	�CB	�B	�%B	��B	�B	�B	�_B
�B
#B
NB
 B
"�B
 fB
�B
?B
�B
�B
VB
YB
�B	��B	��B	�*B	�4B	��B	��B	��B
(B
�B
�B
$�B
:B
;�B
?�B
C~B
GB
HpB
HBB
I�B
MyB
K�B
J�B
JB
MB
H�B
I�B
I�B
I�B
JrB
JBB
L(B
LIB
LB
P(B
P�B
U�B
Y�B
[�B
\�B
\�B
]B
]�B
]JB
]:B
\�B
\�B
`<B
^,B
R�B
R B
LuB
C�B
GkB
M�B
Q*B
T�B
L�B
J�B
K	B
FzB
4�B
2"B
3`B
*�B
&�B
,�B
7{B
=FB
;�B
:}B
5�B
&�B
&|B
&sB
&�B
(B
'B
&B
%@B
(BB
*�B
*dB
-B
+�B
-�B
. B
)�B
'|B
%�B
&�B
%�B
%]B
#�B
#�B
#`B
$�B
!nB
!�B
�B
9B
B
xB
B
�B
�B
�B
�B
wB
�B
�B
GB
kB
�B
4B
�B
�B
�B
�B
8B
�B
B
B
�B
B
-B
PB
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
7B
�B
�B
B

�B

�B

�B
	�B
	pB
	B
	XB
	�B
	�B
	�B
fB

\B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
IB
�B
B
�B
iB
-B
B
�B
�B
�B
XB
RB
$B
]B
�B
�B
KB
HB
�B
B
�B
�B
:B
1B
�B
�B
>B
6B
hB
�B
�B
�B
�B
7B
LB
�B
B
3B
cB
�B
�B
]B
uB
�B
�B
�B
�B
�B
�B
�B
tB
�B
	�B
�B
�B
"B
?B
`B
�B
�B
�B
hB
rB
�B
#B
QB
	�B
	PB
�B
	vB

�B
	B
	B
�B
	zB
	�B
	�B

�B

B
	�B

B

�B

�B

�B

�B

�B
nB
�B
aB
CB
fB
�B
B
 B
�B
B
B
�B
�B
&B
�B
�B
B
CB
3B
B
B
fB
BB
B
UB
/B
QB
�B
�B
AB
(B
�B
�B
�B
�B
�B
pB
AB
mB
�B
�B
�B
�B
�B
cB
MB
8B
B
�B
/B
�B
_B
�B
6B
�B
YB
PB
�B
B
�B
�B
�B
MB
�B
�B
�B
nB
`B
WB
EB
6B
EB
mB
�B
 B
GB
�B
�B
�B
mB
 nB
 4B
�B
'B
B
@B
pB
�B
�B
 PB
 TB
 B
 NB
!B
"6B
!�B
!�B
!�B
!@B
!UB
!�B
#MB
#,B
#BB
#}B
#SB
#eB
"�B
#+B
#�B
#�B
$nB
$B
$/B
%B
%�B
&�B
&�B
&�B
'�B
*&B
(�B
(�B
(�B
)B
*B
*MB
*�B
*�B
*sB
)�B
*!B
*�B
*�B
*�B
*�B
*>B
*B
*QB
*�B
*�B
*�B
+&B
,B
,�B
,�B
,�B
,�B
-�B
0
B
0SB
.�B
/<B
/tB
/�B
/�B
/�B
/yB
/#B
/!B
/'B
/�B
/bB
/�B
0�B
0yB
0/B
0$B
0�B
0�B
1oB
36B
2�B
4AB
4WB
4hB
4"B
3�B
3�B
4�B
5B
5�B
5�B
6LB
5�B
6LB
7B
7B
7�B
7�B
7�B
7�B
8�B
9�B
8�B
8\B
8�B
9IB
;IB
<aB
;�B
;nB
;7B
;�B
;uB
;aB
;�B
;�B
;�B
;�B
<B
;�B
<SB
;�B
<�B
>aB
>�B
>zB
>AB
=�B
>B
=�B
>'B
>_B
>{B
>�B
>�B
>�B
?B
>�B
>�B
?QB
@B
?�B
?�B
@ B
@$B
@HB
@#B
@B
@SB
@8B
@;B
@=B
AZB
A�B
B�B
C�B
D�B
D6B
DhB
D�B
D�B
D�B
EyB
FmB
F�B
GB
F�B
GZB
G4B
GYB
G�B
GB
G�B
G�B
G�B
G�B
G�B
HB
G�B
HB
G�B
G�B
HB
H�B
IB
IlB
J�B
J�B
J�B
J�B
K\B
KB
KvB
KjB
K�B
K�B
KtB
K{B
K�B
L�B
L�B
M/B
MgB
M�B
NfB
N[B
N%B
N�B
N�B
O�B
O�B
O�B
PB
P�B
QCB
QSB
Q B
P�B
QNB
RFB
Q�B
SB
S�B
S�B
S�B
S�B
TRB
TeB
S�B
T4B
S�B
T7B
S�B
T�B
V�B
V�B
V�B
W)B
WB
W�B
XB
X~B
XB
X_B
XqB
XOB
X�B
X�B
YB
X�B
Y5B
Y�B
YyB
Z	B
Z~B
Z�B
Z�B
[8B
Z�B
[MB
[LB
[aB
[�B
[�B
\HB
\FB
]B
\�B
]�B
]oB
]�B
]�B
]�B
]�B
]�B
]�B
^B
]�B
^'B
^B
_+B
_dB
_�B
_�B
_�B
`WB
`B
`mB
`�B
a~B
bB
a�B
a�B
a�B
a�B
a�B
bNB
b�B
b�B
b�B
c#B
ckB
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e6B
elB
e�B
e�B
e�B
eyB
e�B
e|B
e�B
fB
e�B
f
B
f9B
f\B
f�B
g�B
i�B
i#B
iB
i-B
ilB
i(B
i�B
iCB
i�B
i�B
i�B
j0B
i�B
juB
j�B
j�B
jlB
j�B
kB
lB
k^B
lB
lRB
l�B
lXB
l�B
mB
m�B
mxB
mXB
m�B
m�B
nB
n6B
n{B
nYB
nB
nFB
n�B
olB
o�B
pB
pWB
p�B
pMB
p�B
p�B
p�B
p�B
qEB
qXB
qXB
q+B
q�B
q�B
q�B
q�B
r0B
r�B
r�B
rfB
r�B
r�B
sB
s B
s�B
sRB
s!B
seB
s�B
sWB
t$B
t8B
tHB
tqB
t�B
t�B
t�B
t�B
u;B
t�B
u@B
u5B
u�B
u�B
v�B
wB
v�B
wEB
w	B
v�B
w�B
y#B
y#B
y�B
z~B
z�B
zlB
z�B
z�B
z�B
{B
z�B
z�B
z�G�O�B
33B
3hB
2aB
4B
2�B
33B
1�B
2�B
4B
1�B
33B
2-B
2�B
3�B
2aB
2�B
2�B
2�B
33B
2�B
2�B
2�B
2�B
3hB
1�B
3hB
2�B
2aB
3�B
2-B
3hB
2�B
1�B
3hB
2aB
2�B
3hB
1�B
33B
2-B
1�B
2�B
1�B
2�B
1�B
1�B
2�B
2-B
2�B
2�B
2aB
33B
1�B
33B
2�B
1�B
2�B
1�B
2aB
2�B
1�B
2�B
2aB
1�B
2�B
1[B
4B
2�B
1�B
2�B
0�B
1�B
8RB
,�B
2aB
2aB
<jB
1�B
33B
33B
,�B
4�B
6zB
0�B
1�B
2�B
1�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<G�|<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<UVn<J�8<+�|<�XS<���<#�
<���<�#?<.t\<F�<���<Kz�<#�
<=�<�
^<8�&<Mtq<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<,��<#�
<#�
<#�
<#�
<&-O<���<I��<���<���<y�><3��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�)�<#�
<#�
<#�
<<�<#�
<#�
<#�
<+� <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021072005203120210720052031IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021073007010120210730070101QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021073007010120210730070101QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364820220422133648IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064520220426160645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064520220426160645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064520220426160645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                