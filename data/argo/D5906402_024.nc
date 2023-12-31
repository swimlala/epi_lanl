CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-07-10T05:24:19Z creation; 2022-04-26T16:06:59Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210710052419  20220426232407  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_024                 8138_008904_024                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @ق���SP@ق���SP11  @ق�C,�@ق�C,�@,@��>WT@,@��>WT�d��r�/Z�d��r�/Z11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�\@B�\@�  @�  @��R@޸RA   A  A\)A+�A@��A`  A�  A�  A�  A��A��AϮA߮A�  B (�B  B�
B  B (�B((�B0(�B8  B@(�BH(�BP  BW�
B_�Bg�
Bo�
Bx  B�
B��
B��B�  B�  B�  B�  B�{B�(�B��B��B�  B�  B�  B�  B�  B�  B�  B�{B�{B�{B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B��B��C��C��C  C  C
  C  C  C  C  C
=C
=C��C��C
=C  C��C"
=C$  C&  C(  C*  C,  C-��C0  C2
=C4  C5�C7��C:
=C<  C=�C?��CB  CD  CF  CH  CJ
=CL
=CN
=CP
=CR
=CT
=CV
=CX  CZ  C\  C^{C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs��Cu��Cx  Cz  C|  C}��C�C�C�C�  C���C�  C�C�  C���C���C�  C�  C���C���C�  C�C�  C���C���C���C�  C�  C���C���C�  C�  C�  C�  C�  C���C���C�  C�C�C�  C���C�  C�C���C�  C�C�  C�  C�  C�  C�  C�C�C�  C���C���C�  C�  C�  C�C�C�  C�  C���C�  C���C���C�  C�C�  C�  C�  C���C�  C�  C�  C�C�  C���C���C�  C�C�C�  C�  C�  C�  C���C���C�  C�  C���C���C���C���C�C�C���C�  C�C�  C�  C�  C�C�
=C�  C���C�  C�
=C���C���C�  C�  C���C�  C�  C�C�  C���C�  C�  C�C�  C���C�  C���C���C���C���C�  C�C�C�  D �D ��D�D��D�D��D�D��D  D}qD�D� D  D� D  D��D  D��D	  D	� D
�D
��D  D� D��D}qD  D� D�qD� D�D�D�D� D�D��D  D}qD��D� D�D� D�qD� D  D��DD� D�qD� DD��D  D}qD�qDz�D�qD� D  D� D  D� D  D��D   D }qD!  D!}qD"�D"��D#D#��D$�D$��D%D%� D%�qD&� D'  D'� D(�D(��D)�D)� D*�D*� D+  D+� D,  D,� D-  D-��D.�D.� D/  D/� D0  D0� D1  D1}qD2  D2� D2�qD3� D4  D4}qD4�qD5� D6D6��D7  D7� D7�qD8z�D8�qD9}qD:  D:� D;  D;� D<�D<� D=  D=� D>  D>� D>�qD?� D@  D@��DA�DA� DB�DB��DC�DC� DC�qDD� DE  DE}qDE�qDF� DG  DG��DH  DH}qDI  DI� DJ�DJ��DK�DK��DL  DL}qDM  DM� DN  DN}qDO  DO� DP  DP� DQ  DQ� DR  DR��DS  DS� DT  DT� DU  DU� DU�qDV}qDW�DW��DW�qDX� DY�DY� DZ�DZ��D[  D[� D\  D\��D]  D]� D^�D^� D_  D_� D`�D`�Da  Da� Db�Db� DcDc� Dd  Dd� De  De� Df  Df��Dg  Dg}qDh  Dh}qDi  Di� Di�qDj}qDk�Dk��Dl�Dl� Dl�qDm}qDn  Dn� Dn�qDo}qDo�qDp� Dq  Dq� Dr  Dr� Ds  Ds��Dt�Dt� Du  Du}qDu�qDv� Dw  Dw��Dx�Dx� Dx�qDy}qDz  Dz��D{�D{��D|  D|� D}  D}}qD}�qD~� D  D}qD�  D�>�D�~�D��qD�  D�@ D�~�D���D���D�>�D�~�D��qD���D�@ D���D�� D���D�>�D�}qD�� D�  D�AHD��HD�� D�  D�AHD�� D���D���D�@ D���D���D�  D�AHD��HD�� D�  D�AHD��HD�� D���D�>�D�� D��HD�  D�AHD��HD�� D���D�=qD�� D�� D�  D�@ D�~�D�� D�  D�=qD�~�D���D�  D�@ D�~�D�� D�HD�>�D�� D��HD�HD�AHD�~�D�� D�  D�>�D�~�D�� D�  D�@ D�~�D��HD�HD�@ D�� D���D���D�@ D�� D�� D�  D�AHD��HD��HD�  D�@ D��HD�� D���D�>�D�� D�� D��qD�>�D�� D��HD�  D�=qD�~�D���D�  D�@ D�~�D���D���D�>�D�� D��HD�  D�@ D�~�D��qD�  D�B�D��HD���D�  D�@ D�~�D���D�HD�@ D�� D��HD�HD�>�D�~�D���D���D�>�D�� D��HD�HD�@ D��HD�� D���D�@ D��HD�� D��qD�@ D�� D��HD�HD�AHD��HD�� D�HD�@ D�� D��HD�HD�AHD�� D�� D�HD�AHD��HD�� D�  D�>�D�~�D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�>�D�� D��HD�HD�AHD��HD��HD�HD�AHD�~�D�� D���D�>�D�~�D���D�  D�@ D�� D�� D�  D�AHD��HD�� D�HD�@ D�~�D���D���D�@ D�� D�� D�  D�@ D��HD��HD�HD�AHD�� D�� D�  D�@ D��HD�� D�  D�@ D�� D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�� D�HD�@ D�� D�� D�HD�@ D�� D�� D�  D�AHD�� D�� D�  D�@ D��HD��HD�HD�AHD�� D���D�HD�@ D�� D��HD�  D�@ D D�� D���D�@ DÁHD��HD�HD�AHDĀ D��HD�HD�AHDŀ D�� D�  D�@ D�~�Dƾ�D�  D�>�D�~�D�� D�  D�@ DȁHD��HD�  D�>�D�~�Dɾ�D�  D�>�D�~�D��HD�HD�@ Dˀ D��HD�HD�@ D�~�D�� D�HD�@ D�~�D;�D�  D�@ D΀ Dξ�D�  D�@ D�~�D�� D�  D�@ DЂ�D��HD�HD�@ D�~�DѾ�D���D�@ DҀ DҾ�D�  D�AHDӁHD��HD�HD�@ D�~�DԽqD���D�@ D�~�D�� D�HD�AHDր D�� D�HD�AHDׁHD��HD�  D�@ D؁HD�� D�  D�@ D�~�Dپ�D�  D�>�D�~�D�� D���D�>�Dۀ D۾�D�  D�@ D�~�D��HD�  D�>�D�~�D�� D�  D�>�D�~�D޾�D���D�>�D߁HD��HD�  D�>�D�~�DྸD�  D�AHD� DᾸD���D�>�D� D��HD�  D�@ D� D��HD�HD�AHD� D��HD�HD�AHD�HD�� D���D�@ D� D澸D�HD�AHD� D�� D�  D�AHD� D�� D���?��?8Q�?�  ?���?�p�?�@�\@z�@#�
@5@G�@W
=@fff@s33@��\@�=q@��@��H@�G�@��@��@��H@��
@˅@�@޸R@�ff@��@�Q�AG�A�A��A{A�A
=A�A�RA#�
A(Q�A,(�A0��A4z�A8��A<��AAG�AFffAI��AN{AR�\AUAZ�HA_\)Ac33Ag�Ak�Ap  Atz�Ax��A~{A�G�A�33A�A��A�=qA��
A�ffA���A��HA�p�A��A��A�(�A�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?�=q@�\@B�\@�  @�  @��R@޸RA   A  A\)A+�A@��A`  A�  A�  A�  A��A��AϮA߮A�  B (�B  B�
B  B (�B((�B0(�B8  B@(�BH(�BP  BW�
B_�Bg�
Bo�
Bx  B�
B��
B��B�  B�  B�  B�  B�{B�(�B��B��B�  B�  B�  B�  B�  B�  B�  B�{B�{B�{B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B��B��C��C��C  C  C
  C  C  C  C  C
=C
=C��C��C
=C  C��C"
=C$  C&  C(  C*  C,  C-��C0  C2
=C4  C5�C7��C:
=C<  C=�C?��CB  CD  CF  CH  CJ
=CL
=CN
=CP
=CR
=CT
=CV
=CX  CZ  C\  C^{C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs��Cu��Cx  Cz  C|  C}��C�C�C�C�  C���C�  C�C�  C���C���C�  C�  C���C���C�  C�C�  C���C���C���C�  C�  C���C���C�  C�  C�  C�  C�  C���C���C�  C�C�C�  C���C�  C�C���C�  C�C�  C�  C�  C�  C�  C�C�C�  C���C���C�  C�  C�  C�C�C�  C�  C���C�  C���C���C�  C�C�  C�  C�  C���C�  C�  C�  C�C�  C���C���C�  C�C�C�  C�  C�  C�  C���C���C�  C�  C���C���C���C���C�C�C���C�  C�C�  C�  C�  C�C�
=C�  C���C�  C�
=C���C���C�  C�  C���C�  C�  C�C�  C���C�  C�  C�C�  C���C�  C���C���C���C���C�  C�C�C�  D �D ��D�D��D�D��D�D��D  D}qD�D� D  D� D  D��D  D��D	  D	� D
�D
��D  D� D��D}qD  D� D�qD� D�D�D�D� D�D��D  D}qD��D� D�D� D�qD� D  D��DD� D�qD� DD��D  D}qD�qDz�D�qD� D  D� D  D� D  D��D   D }qD!  D!}qD"�D"��D#D#��D$�D$��D%D%� D%�qD&� D'  D'� D(�D(��D)�D)� D*�D*� D+  D+� D,  D,� D-  D-��D.�D.� D/  D/� D0  D0� D1  D1}qD2  D2� D2�qD3� D4  D4}qD4�qD5� D6D6��D7  D7� D7�qD8z�D8�qD9}qD:  D:� D;  D;� D<�D<� D=  D=� D>  D>� D>�qD?� D@  D@��DA�DA� DB�DB��DC�DC� DC�qDD� DE  DE}qDE�qDF� DG  DG��DH  DH}qDI  DI� DJ�DJ��DK�DK��DL  DL}qDM  DM� DN  DN}qDO  DO� DP  DP� DQ  DQ� DR  DR��DS  DS� DT  DT� DU  DU� DU�qDV}qDW�DW��DW�qDX� DY�DY� DZ�DZ��D[  D[� D\  D\��D]  D]� D^�D^� D_  D_� D`�D`�Da  Da� Db�Db� DcDc� Dd  Dd� De  De� Df  Df��Dg  Dg}qDh  Dh}qDi  Di� Di�qDj}qDk�Dk��Dl�Dl� Dl�qDm}qDn  Dn� Dn�qDo}qDo�qDp� Dq  Dq� Dr  Dr� Ds  Ds��Dt�Dt� Du  Du}qDu�qDv� Dw  Dw��Dx�Dx� Dx�qDy}qDz  Dz��D{�D{��D|  D|� D}  D}}qD}�qD~� D  D}qD�  D�>�D�~�D��qD�  D�@ D�~�D���D���D�>�D�~�D��qD���D�@ D���D�� D���D�>�D�}qD�� D�  D�AHD��HD�� D�  D�AHD�� D���D���D�@ D���D���D�  D�AHD��HD�� D�  D�AHD��HD�� D���D�>�D�� D��HD�  D�AHD��HD�� D���D�=qD�� D�� D�  D�@ D�~�D�� D�  D�=qD�~�D���D�  D�@ D�~�D�� D�HD�>�D�� D��HD�HD�AHD�~�D�� D�  D�>�D�~�D�� D�  D�@ D�~�D��HD�HD�@ D�� D���D���D�@ D�� D�� D�  D�AHD��HD��HD�  D�@ D��HD�� D���D�>�D�� D�� D��qD�>�D�� D��HD�  D�=qD�~�D���D�  D�@ D�~�D���D���D�>�D�� D��HD�  D�@ D�~�D��qD�  D�B�D��HD���D�  D�@ D�~�D���D�HD�@ D�� D��HD�HD�>�D�~�D���D���D�>�D�� D��HD�HD�@ D��HD�� D���D�@ D��HD�� D��qD�@ D�� D��HD�HD�AHD��HD�� D�HD�@ D�� D��HD�HD�AHD�� D�� D�HD�AHD��HD�� D�  D�>�D�~�D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�>�D�� D��HD�HD�AHD��HD��HD�HD�AHD�~�D�� D���D�>�D�~�D���D�  D�@ D�� D�� D�  D�AHD��HD�� D�HD�@ D�~�D���D���D�@ D�� D�� D�  D�@ D��HD��HD�HD�AHD�� D�� D�  D�@ D��HD�� D�  D�@ D�� D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�� D�HD�@ D�� D�� D�HD�@ D�� D�� D�  D�AHD�� D�� D�  D�@ D��HD��HD�HD�AHD�� D���D�HD�@ D�� D��HD�  D�@ D D�� D���D�@ DÁHD��HD�HD�AHDĀ D��HD�HD�AHDŀ D�� D�  D�@ D�~�Dƾ�D�  D�>�D�~�D�� D�  D�@ DȁHD��HD�  D�>�D�~�Dɾ�D�  D�>�D�~�D��HD�HD�@ Dˀ D��HD�HD�@ D�~�D�� D�HD�@ D�~�D;�D�  D�@ D΀ Dξ�D�  D�@ D�~�D�� D�  D�@ DЂ�D��HD�HD�@ D�~�DѾ�D���D�@ DҀ DҾ�D�  D�AHDӁHD��HD�HD�@ D�~�DԽqD���D�@ D�~�D�� D�HD�AHDր D�� D�HD�AHDׁHD��HD�  D�@ D؁HD�� D�  D�@ D�~�Dپ�D�  D�>�D�~�D�� D���D�>�Dۀ D۾�D�  D�@ D�~�D��HD�  D�>�D�~�D�� D�  D�>�D�~�D޾�D���D�>�D߁HD��HD�  D�>�D�~�DྸD�  D�AHD� DᾸD���D�>�D� D��HD�  D�@ D� D��HD�HD�AHD� D��HD�HD�AHD�HD�� D���D�@ D� D澸D�HD�AHD� D�� D�  D�AHD� D�� G�O�?��?8Q�?�  ?���?�p�?�@�\@z�@#�
@5@G�@W
=@fff@s33@��\@�=q@��@��H@�G�@��@��@��H@��
@˅@�@޸R@�ff@��@�Q�AG�A�A��A{A�A
=A�A�RA#�
A(Q�A,(�A0��A4z�A8��A<��AAG�AFffAI��AN{AR�\AUAZ�HA_\)Ac33Ag�Ak�Ap  Atz�Ax��A~{A�G�A�33A�A��A�=qA��
A�ffA���A��HA�p�A��A��A�(�A�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A׼jA���A׺^A׸RA״9A׶FA׮Aק�A׮A׬Aף�A�r�A��A��A��`A��/A��
A���A���A�ȴA�A־wAֺ^Aֲ-A֮A֥�Aև+A�M�A�9XA�-A� �A�  Aՙ�A�x�A�I�A�/A�
=A��A���A�AӇ+A��AсA�ĜA�dZA�{A�z�A�n�A͕�A�-A��A̙�A�Q�A�1'A���A�E�AʮA��A�r�A�XAƃA��AüjA�r�A���A��#A�  A���A�ZA��A�+A��A��A��A��uA�/A���A��A��A�&�A�ĜA�v�A�v�A�
=A�S�A�~�A�1'A��`A�M�A�jA��A���A���A�-A�bNA�K�A�33A���A���A�`BA�Q�A�n�A�A���A~ĜA{�;Ay��At�ArjAq�hApbAo&�Am33Ai33AfbNAbr�A` �A^ffA[��AV��AS�AR1'AP�RAM�^AK`BAG�ACC�AA��A?x�A>$�A=�A;?}A8�jA8^5A8-A7A6E�A4�yA3�mA2n�A1XA0(�A0�A0�uA/�wA/33A.VA,Q�A++A*�HA*{A(�DA'l�A&��A%G�A!�PA I�A ^5A ��A!�TA �`A ~�A -Al�A�!A  A�A?}A33A&�A
=A�`A�RA�\A-A�-AhsA%A��A�A�A&�AoA��A��An�A1'A��AƨA�hA�A��AM�A9XA5?AA�Ax�A�A�;A�7A7LA�yA�9A-A�AO�A�yA�RAz�A�hA�`Ar�A5?A�#A��A��A��A�PA33A��AbA-A9XA��AK�A
�HA
��A
9XA	%A�!A�!A�HA��A��AK�A33A�AoA%A��AƨA&�A��A1At�A+A��AVA 1@���@�+@���@���@�V@���@�C�@��@�@��H@�S�@�S�@��^@���@���@���@�1@�
=@�ff@��\@�V@�x�@��j@�A�@�ƨ@�;d@���@�-@�hs@�O�@��@�b@��m@�F@@�l�@�+@�?}@�9X@��@��@��@�o@�-@��#@��@�h@���@�b@��;@畁@�33@�
=@�$�@�x�@��`@� �@�l�@���@��T@�7L@�r�@��;@ߥ�@�\)@��y@�@�Ĝ@�I�@���@ۍP@�K�@�
=@ڗ�@�M�@�p�@���@�\)@�o@��@���@�v�@�{@��#@ղ-@�`B@�%@�Q�@ӥ�@�n�@ѩ�@���@�Ĝ@Ѓ@��@��;@ϥ�@�l�@�S�@�@�V@�O�@��@��/@�ƨ@���@�~�@ə�@�O�@���@�j@�Z@�I�@�b@��@�X@�V@���@ēu@�r�@�I�@�9X@� �@�  @��;@�ƨ@Õ�@�t�@�\)@�C�@�"�@�
=@\@�x�@�%@���@��@��@� �@�t�@��@���@�^5@�$�@���@��`@��u@�r�@�Q�@�I�@�9X@�1@���@�+@�
=@��@���@�ff@�5?@�{@��7@�bN@�A�@�  @��w@���@��@�x�@�Ĝ@��u@�bN@�A�@� �@��;@�|�@�C�@�
=@���@�v�@��#@�O�@��@��j@��@�I�@��@���@��@��\@�@���@��@���@�t�@�S�@��@�ȴ@�v�@�=q@��@��7@�&�@���@���@�dZ@��H@�E�@���@�hs@�O�@���@��D@�(�@���@��w@�+@��y@���@�{@���@��7@�V@���@��9@�r�@�I�@�(�@�  @��F@�t�@�S�@�C�@�33@�"�@���@��y@��y@�ȴ@�{@��h@�p�@��j@�9X@���@�@�V@�{@��^@�?}@���@���@��u@�r�@�I�@�b@��@��;@���@��@�t�@�ȴ@��^@�Z@���@��@��@�"�@��R@�^5@�M�@���@�p�@�X@�X@�O�@�&�@��@��/@��9@�bN@�1@��;@��@�l�@�
=@�~�@�$�@���@���@�&�@��/@���@��j@��@�bN@��;@���@�l�@�S�@�;d@��H@��!@���@���@��+@�n�@�E�@�@��T@���@��^@���@��7@��@�hs@�?}@��@��9@�1'@�  @�ƨ@�|�@�S�@�;d@�"�@�
=@��y@��+@�M�@�{@��@��^@���@�X@��@���@�z�@�Z@�I�@�9X@�  @��;@���@�\)@�33@���@�v�@�-@��@�@��h@���@�(�@��@�P@|�@;d@
=@~�y@~�@~��@~�+@~5?@}�-@}O�@|��@|�@|��@|�D@|9X@|1@{�m@{�
@{ƨ@{�F@{��@{S�@y��@y��@yG�@x�@x �@wl�@v��@vȴ@v�R@vff@v5?@v$�@v@u��@u?}@t��@s�m@s�@s@r�\@q��@q��@q�7@qX@q�@pr�@ol�@n��@nv�@m��@m�@l�/@j�H@j~�@i��@iX@i�@h��@h�9@hbN@h  @gl�@g�@f��@f5?@d�@b��@a��@a&�@a%@`  @_+@^�@^��@^$�@]�@\Z@[�@[33@["�@Z~�@ZM�@Y�@Y��@Y��@Y��@X��@X�9@X �@W\)@W;d@V�y@VV@U��@U@U�-@U?}@Tj@T�@SC�@R�!@Q��@QG�@Q%@P�`@P�u@PA�@O�P@N{@M�@L��@L�D@LI�@K��@KdZ@J�@J�@Ihs@HĜ@H�9@H�u@Hr�@HQ�@HA�@H  @G�P@Fȴ@FV@F$�@E�-@Dz�@C�m@C��@C��@Ct�@CS�@C33@C@B�H@B��@A�7@@�9@?�@?��@?l�@?;d@>��@>�R@>�+@>v�@>ff@>ff@>ff@>5?@>{@>@=�T@=��@=�-@=�h@=�@=`B@=O�@=?}@=?}@=�@<��@<��@<��@<�D@<9X@<�@<I�@<Z@<I�@<I�@<�@<�@<�@<1@;�
@;ƨ@;�F@;��@;C�@;"�@;o@:�H@:��@:�\@:=q@:�@9��@9X@9%@8��@8�`@8Ĝ@8�9@8��@8�@8A�@81'@8  @7�P@7�@6�@6�+@6{@5`B@4�/@4I�@3�
@3��@3�@3�@3�@3dZ@3dZ@3dZ@3dZ@333@3"�@2�H@2~�@2n�@2^5@2-@2-@2-@1��@1�^@1x�@1X@1&�@1%@0��@0�9@0�u@0A�@/��@/l�@.V@-@-`B@-`B@-O�@-O�@-?}@-/@-V@,�@,�@,�@,��@,z�@,(�@,�@,1@,1@+��@+�m@+ƨ@+�F@+��@+�@+�@+t�@+dZ@+dZ@+@*��@*M�@)�@)��@)��@)�^@)��@)�7@)x�@)x�@)x�@)x�@)X@)&�@)�@(��@(�9@( �@'�P@'\)@'K�@'�@&v�@%�T@%`B@$�/@$9X@#�
@#@"n�@"^5@"^5@"M�@"-@"J@!�#@!��@!G�@!%@ �`@ Ĝ@ �9@ ��@ �u@ �@ bN@ A�@  �@��@\)@�R@��@��@v�@ff@E�@@�@�A�ĜA���A���A׾wA׾wA���A׶FA���A׺^A�A׾wA�ĜA�A׾wA׺^A״9A׼jA״9A״9A׺^Aײ-Aײ-A״9Aײ-A׸RA׺^A׶FA׬Aק�A׮Aײ-Aש�Aץ�Aש�A׮A׬A׮Aװ!Aש�A׬A׬Aש�Aש�Aן�Aץ�Aכ�Aכ�Aס�Aץ�Aץ�A׏\AׁA�G�A�E�A�G�A�M�A�+A� �A�(�A��A���A��A��A��A��A��A��yA��mA��yA��`A��mA��`A��`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 A���A׼jA���A׺^A׸RA״9A׶FA׮Aק�A׮A׬Aף�A�r�A��A��A��`A��/A��
A���A���A�ȴA�A־wAֺ^Aֲ-A֮A֥�Aև+A�M�A�9XA�-A� �A�  Aՙ�A�x�A�I�A�/A�
=A��A���A�AӇ+A��AсA�ĜA�dZA�{A�z�A�n�A͕�A�-A��A̙�A�Q�A�1'A���A�E�AʮA��A�r�A�XAƃA��AüjA�r�A���A��#A�  A���A�ZA��A�+A��A��A��A��uA�/A���A��A��A�&�A�ĜA�v�A�v�A�
=A�S�A�~�A�1'A��`A�M�A�jA��A���A���A�-A�bNA�K�A�33A���A���A�`BA�Q�A�n�A�A���A~ĜA{�;Ay��At�ArjAq�hApbAo&�Am33Ai33AfbNAbr�A` �A^ffA[��AV��AS�AR1'AP�RAM�^AK`BAG�ACC�AA��A?x�A>$�A=�A;?}A8�jA8^5A8-A7A6E�A4�yA3�mA2n�A1XA0(�A0�A0�uA/�wA/33A.VA,Q�A++A*�HA*{A(�DA'l�A&��A%G�A!�PA I�A ^5A ��A!�TA �`A ~�A -Al�A�!A  A�A?}A33A&�A
=A�`A�RA�\A-A�-AhsA%A��A�A�A&�AoA��A��An�A1'A��AƨA�hA�A��AM�A9XA5?AA�Ax�A�A�;A�7A7LA�yA�9A-A�AO�A�yA�RAz�A�hA�`Ar�A5?A�#A��A��A��A�PA33A��AbA-A9XA��AK�A
�HA
��A
9XA	%A�!A�!A�HA��A��AK�A33A�AoA%A��AƨA&�A��A1At�A+A��AVA 1@���@�+@���@���@�V@���@�C�@��@�@��H@�S�@�S�@��^@���@���@���@�1@�
=@�ff@��\@�V@�x�@��j@�A�@�ƨ@�;d@���@�-@�hs@�O�@��@�b@��m@�F@@�l�@�+@�?}@�9X@��@��@��@�o@�-@��#@��@�h@���@�b@��;@畁@�33@�
=@�$�@�x�@��`@� �@�l�@���@��T@�7L@�r�@��;@ߥ�@�\)@��y@�@�Ĝ@�I�@���@ۍP@�K�@�
=@ڗ�@�M�@�p�@���@�\)@�o@��@���@�v�@�{@��#@ղ-@�`B@�%@�Q�@ӥ�@�n�@ѩ�@���@�Ĝ@Ѓ@��@��;@ϥ�@�l�@�S�@�@�V@�O�@��@��/@�ƨ@���@�~�@ə�@�O�@���@�j@�Z@�I�@�b@��@�X@�V@���@ēu@�r�@�I�@�9X@� �@�  @��;@�ƨ@Õ�@�t�@�\)@�C�@�"�@�
=@\@�x�@�%@���@��@��@� �@�t�@��@���@�^5@�$�@���@��`@��u@�r�@�Q�@�I�@�9X@�1@���@�+@�
=@��@���@�ff@�5?@�{@��7@�bN@�A�@�  @��w@���@��@�x�@�Ĝ@��u@�bN@�A�@� �@��;@�|�@�C�@�
=@���@�v�@��#@�O�@��@��j@��@�I�@��@���@��@��\@�@���@��@���@�t�@�S�@��@�ȴ@�v�@�=q@��@��7@�&�@���@���@�dZ@��H@�E�@���@�hs@�O�@���@��D@�(�@���@��w@�+@��y@���@�{@���@��7@�V@���@��9@�r�@�I�@�(�@�  @��F@�t�@�S�@�C�@�33@�"�@���@��y@��y@�ȴ@�{@��h@�p�@��j@�9X@���@�@�V@�{@��^@�?}@���@���@��u@�r�@�I�@�b@��@��;@���@��@�t�@�ȴ@��^@�Z@���@��@��@�"�@��R@�^5@�M�@���@�p�@�X@�X@�O�@�&�@��@��/@��9@�bN@�1@��;@��@�l�@�
=@�~�@�$�@���@���@�&�@��/@���@��j@��@�bN@��;@���@�l�@�S�@�;d@��H@��!@���@���@��+@�n�@�E�@�@��T@���@��^@���@��7@��@�hs@�?}@��@��9@�1'@�  @�ƨ@�|�@�S�@�;d@�"�@�
=@��y@��+@�M�@�{@��@��^@���@�X@��@���@�z�@�Z@�I�@�9X@�  @��;@���@�\)@�33@���@�v�@�-@��@�@��h@���@�(�@��@�P@|�@;d@
=@~�y@~�@~��@~�+@~5?@}�-@}O�@|��@|�@|��@|�D@|9X@|1@{�m@{�
@{ƨ@{�F@{��@{S�@y��@y��@yG�@x�@x �@wl�@v��@vȴ@v�R@vff@v5?@v$�@v@u��@u?}@t��@s�m@s�@s@r�\@q��@q��@q�7@qX@q�@pr�@ol�@n��@nv�@m��@m�@l�/@j�H@j~�@i��@iX@i�@h��@h�9@hbN@h  @gl�@g�@f��@f5?@d�@b��@a��@a&�@a%@`  @_+@^�@^��@^$�@]�@\Z@[�@[33@["�@Z~�@ZM�@Y�@Y��@Y��@Y��@X��@X�9@X �@W\)@W;d@V�y@VV@U��@U@U�-@U?}@Tj@T�@SC�@R�!@Q��@QG�@Q%@P�`@P�u@PA�@O�P@N{@M�@L��@L�D@LI�@K��@KdZ@J�@J�@Ihs@HĜ@H�9@H�u@Hr�@HQ�@HA�@H  @G�P@Fȴ@FV@F$�@E�-@Dz�@C�m@C��@C��@Ct�@CS�@C33@C@B�H@B��@A�7@@�9@?�@?��@?l�@?;d@>��@>�R@>�+@>v�@>ff@>ff@>ff@>5?@>{@>@=�T@=��@=�-@=�h@=�@=`B@=O�@=?}@=?}@=�@<��@<��@<��@<�D@<9X@<�@<I�@<Z@<I�@<I�@<�@<�@<�@<1@;�
@;ƨ@;�F@;��@;C�@;"�@;o@:�H@:��@:�\@:=q@:�@9��@9X@9%@8��@8�`@8Ĝ@8�9@8��@8�@8A�@81'@8  @7�P@7�@6�@6�+@6{@5`B@4�/@4I�@3�
@3��@3�@3�@3�@3dZ@3dZ@3dZ@3dZ@333@3"�@2�H@2~�@2n�@2^5@2-@2-@2-@1��@1�^@1x�@1X@1&�@1%@0��@0�9@0�u@0A�@/��@/l�@.V@-@-`B@-`B@-O�@-O�@-?}@-/@-V@,�@,�@,�@,��@,z�@,(�@,�@,1@,1@+��@+�m@+ƨ@+�F@+��@+�@+�@+t�@+dZ@+dZ@+@*��@*M�@)�@)��@)��@)�^@)��@)�7@)x�@)x�@)x�@)x�@)X@)&�@)�@(��@(�9@( �@'�P@'\)@'K�@'�@&v�@%�T@%`B@$�/@$9X@#�
@#@"n�@"^5@"^5@"M�@"-@"J@!�#@!��@!G�@!%@ �`@ Ĝ@ �9@ ��@ �u@ �@ bN@ A�@  �@��@\)@�R@��@��@v�@ff@E�@@�G�O�A�ĜA���A���A׾wA׾wA���A׶FA���A׺^A�A׾wA�ĜA�A׾wA׺^A״9A׼jA״9A״9A׺^Aײ-Aײ-A״9Aײ-A׸RA׺^A׶FA׬Aק�A׮Aײ-Aש�Aץ�Aש�A׮A׬A׮Aװ!Aש�A׬A׬Aש�Aש�Aן�Aץ�Aכ�Aכ�Aס�Aץ�Aץ�A׏\AׁA�G�A�E�A�G�A�M�A�+A� �A�(�A��A���A��A��A��A��A��A��yA��mA��yA��`A��mA��`A��`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
4B
hB
�B
4B
 B
�B
�B
 B
�B
hB
hB
4B
B
{B
�B
MB
MB
B
B
�B
{B
FB
FB
FB
�B
B
:B
hB
�B
B
JB
�B
�B
7B
�B
hB
(B
	B
�B	�cB	�B	�`B	��B	ʌB	�2B	�B
+B
�B
B�B
u%B
�	B
��B
�-B
�B
�OB
�B
�<B
��B
��B�BOB^By�B�MB��B�{B�B��B��B�=B�+B��B�RB��B��B��B��BxlBn�B��B�;Br�B|PBq�BiDBN<B6�B+�B�B@BuB
�`B
�;B
�B
چB
ҽB
�mB
�6B
��B
�.B
��B
.B
c�B
O�B
)*B

�B	�%B	�B	�sB	�B	��B	�*B	��B	�qB	��B	��B	�oB	rGB	k�B	VB	NpB	@�B	:*B	8�B	6�B	0UB	6�B	<6B	C-B	V�B	_�B	^�B	b�B	e,B	l�B	rGB	��B	��B	��B	��B	�B	�B	ںB	��B	��B	�8B	�(B
�B
B
�B
�B
 \B
$tB
�B
�B
YB
hB
�B
�B
CB
0UB
/�B
0�B
1'B
-wB
)�B
&B
&LB
(�B
)�B
*eB
+�B
.�B
3�B
7�B
8�B
=qB
>wB
<�B
:*B
7B
3�B
2�B
2�B
3hB
4�B
5tB
5tB
6�B
9�B
?HB
?�B
@�B
A�B
B�B
D�B
GEB
HKB
HB
E�B
D3B
D3B
CaB
C�B
E9B
GB
EmB
F?B
D�B
E�B
E9B
D�B
B'B
A�B
C�B
E9B
GB
L0B
O�B
P�B
QNB
PB
K)B
L�B
M�B
NB
J�B
J�B
D�B
EmB
>�B
<�B
=qB
AUB
CaB
B�B
?}B
>wB
=�B
=qB
<�B
=<B
:�B
7�B
5B
/B
'RB
!�B
B
�B
B
�B
�B
�B
�B
 �B	�"B	�B	��B	�PB	��B	��B
	7B
B	��B
�B

=B
DB
DB

	B
DB
�B
B
�B
.B
�B
~B
B
	�B
�B
�B
1B
1B
�B
+B
YB
�B
�B
�B
AB
�B
�B
B
_B
�B
+B
�B
�B
�B
GB
AB
B
B
 iB
  B	��B	��B	��B	�8B	�8B	�lB	�B	��B	��B	�xB	�B	�B	��B	�.B	��B	��B	�.B	�cB	�cB
  B	�cB
AB
AB
�B
�B
�B
B
GB
{B
{B
GB
GB
B
�B
GB
�B
B
SB
SB
�B
�B
�B
�B
%B
�B
%B
%B
�B
%B
�B
�B
�B
YB
�B
YB
�B
�B
YB
�B
�B
�B
�B
�B
+B
+B
+B
+B
+B
_B
_B
_B
�B
�B
�B
_B
�B
_B
�B
�B
�B
�B
�B
1B
1B
�B
�B
�B
�B
1B
�B
1B
1B
fB
1B
1B
�B
�B
�B
1B
1B
�B
1B
�B
1B
�B
_B
fB
	B
1B
fB
�B
�B
1B

=B

�B

=B

�B

�B

�B

�B
B

�B

�B

�B
B
�B
�B
�B
�B
�B
"B
�B
�B
�B
�B
�B
4B
B
:B
oB
B
oB
oB
�B
�B
B
B
@B
uB
{B
{B
FB
B
B
�B
B
B
�B
�B
�B
SB
YB
YB
�B
�B
�B
�B
�B
�B
_B
�B
�B
_B
�B
�B
�B
1B
eB
1B
�B
�B
1B
�B
1B
�B
=B
�B
�B
�B
B
�B
�B
B
�B
 \B
 �B
 �B
 �B
 �B
 �B
 �B
 \B
 �B
 'B
�B
�B
 'B
!bB
#�B
#:B
#:B
#:B
#nB
$tB
#�B
#nB
$�B
$�B
$B
$@B
$tB
$B
$@B
$@B
#�B
$B
$tB
$tB
$B
$tB
$�B
%�B
&LB
&�B
&�B
'�B
($B
($B
($B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
+6B
+B
*�B
*eB
*�B
+B
+kB
+kB
+�B
+�B
,B
,B
,B
+�B
+�B
+�B
+�B
,�B
-CB
-CB
.B
.�B
/B
/B
/OB
/B
/OB
0!B
0�B
0�B
0�B
1[B
0�B
1�B
1�B
2�B
2�B
2�B
33B
3hB
3�B
3�B
49B
49B
49B
5B
5?B
6B
6�B
6B
6zB
8B
7�B
8�B
8�B
8RB
8�B
8�B
8�B
8�B
8�B
8�B
9XB
9�B
9�B
:^B
:�B
:^B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
<jB
<6B
;�B
<jB
<�B
=B
=qB
=B
=B
=<B
=qB
=qB
=<B
=qB
=�B
=�B
>BB
>B
>�B
>�B
?B
?HB
?HB
?HB
?}B
?�B
@B
@OB
@�B
@�B
@OB
@B
AUB
A B
AUB
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B'B
B'B
B[B
C�B
D3B
E9B
E�B
EB
F?B
FB
F�B
F�B
F�B
F�B
HB
H�B
IB
H�B
IRB
IRB
J#B
I�B
I�B
I�B
J�B
J�B
K^B
K�B
K�B
MB
MjB
M�B
MjB
MjB
NB
NpB
N�B
OBB
OB
PB
PHB
PB
PHB
PB
OvB
PB
QB
QB
QB
Q�B
Q�B
R�B
R�B
S�B
T�B
VB
V�B
V�B
VmB
VmB
V�B
V�B
V�B
V�B
XB
XyB
XB
XEB
YB
Y�B
ZQB
ZQB
ZB
ZB
Z�B
Z�B
Z�B
Z�B
[�B
\]B
\�B
\�B
\�B
]�B
]�B
]�B
^B
^5B
^jB
^B
^B
^jB
^�B
^�B
^�B
_B
^�B
^�B
_B
_B
^�B
_B
_;B
_�B
_pB
_;B
_�B
`BB
`BB
`vB
`B
_�B
_�B
`BB
`BB
_�B
_�B
`BB
`�B
`�B
`BB
`BB
`vB
`�B
aB
aB
`�B
`�B
a|B
a|B
bNB
bB
bNB
bNB
bNB
b�B
b�B
b�B
b�B
c B
b�B
b�B
c�B
c�B
c�B
c�B
dZB
e`B
e`B
f2B
f�B
f�B
f�B
f�B
f�B
f�B
ffB
f�B
ffB
f2B
f�B
gB
f�B
f�B
g8B
g8B
f�B
gB
gmB
g�B
g�B
h
B
hsB
h>B
h�B
h�B
hsB
h�B
iB
iDB
j�B
k�B
kB
kB
k�B
kQB
kB
kQB
kB
k�B
k�B
kQB
k�B
kQB
k�B
k�B
l"B
k�B
k�B
k�B
k�B
lWB
l�B
lWB
l"B
lWB
lWB
l"B
l�B
l�B
m]B
m]B
m�B
m]B
m�B
n/B
n/B
m�B
m�B
m�B
m�B
ncB
n/B
n/B
o B
n�B
o�B
pB
pB
pB
pB
p;B
qvB
qAB
q�B
rGB
r�B
s�B
s�B
s�B
s�B
s�B
tB
tB
s�B
tB
t�B
u%B
u%B
u�B
u%B
u%B
u%B
uZB
u�B
u�B
u�B
u�B
v+B
v�B
v�B
v`B
v�B
v�B
v�B
w�B
w�B
x�B
\B
:B
�B
�B
4B
�B
�B
�B
�B
.B
�B
�B
�B
oB
�B
oB
�B
:B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
oB
�B
B
�B
�B
4B
 B
oB
B
�B
�B
bB
hB
�B
�B
hB
�B
�B
hB
.B
�B
�B
_B
7B
oB
B
�B
PB
�B
�B
oB
$B
�B
B
MB
B
B
{B
�B
�B
�B
B
�B
MB
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 B
ZB
aB
B
WB
,B
�B
�B
<B
�B
�B
�B
�B
B
wB
�B
�B
zB
2B
EB
�B
�B
kB
lB
B
�B
TB
B
�B
EB
pB
�B
�B
B
�B
�B
B
�B
	�B
�B
�B	�B	�B	�MB	�~B	�YB	��B
	2B
&B
EnB
vIB
��B
��B
�B
�lB
�B
��B
�7B
�B
� BBQ�Be;B��B�+B��B�B��B�B��B�AB�B�B��B�GB��B��B��B�B��B�DB��B�%B�NBv=By�BY�B?|B4pB(_B�B�B
�WB
��B
��B
�PB
��B
�B
�SB
�;B
��B
�]B
��B
t�B
d�B
9B
]B	�yB	��B	�JB	�oB	�B	��B	�uB	��B	�B	��B	�}B	x0B	t#B	eBB	WnB	F~B	?OB	B.B	>�B	<�B	DB	AKB	J�B	Z�B	c1B	e,B	i�B	f�B	m�B	vFB	�B	�B	�B	��B	��B	��B	�;B	�=B	�B	�'B
?B
SB
�B
�B
!�B
%WB
'�B
 tB
#"B
"�B
pB
�B
�B
�B
3B
1)B
1�B
3�B
/�B
,B
'�B
'5B
)$B
)�B
*�B
,&B
/QB
4<B
8�B
:tB
>�B
@B
>B
=kB
8�B
5?B
3IB
3�B
4^B
5�B
6qB
6dB
7�B
:�B
A;B
A�B
BB
BB
B�B
E�B
H�B
I�B
K�B
H�B
E�B
E�B
D�B
EB
G\B
HBB
HB
G�B
E�B
G4B
H�B
G^B
DB
B�B
ElB
F	B
F�B
L;B
QB
R�B
S�B
R1B
J�B
L�B
PqB
OQB
LAB
K�B
GB
J
B
@KB
<�B
<�B
B�B
F-B
ECB
@B
>�B
>B
=�B
>�B
@�B
=�B
9oB
8B
1�B
(�B
$�B
$�B
8B
�B
	�B
�B
�B

B
�B	�YB	��B	��B	�pB	��B	��B
fB
 B	��B
B
�B
�B
�B
	�B
4B
B
�B
B
kB
�B
�B
�B
YB
	cB
	�B
	<B
�B
B
�B
'B
	B
	�B
B
�B
�B
CB
>B
	bB
�B
�B
PB
�B
[B
�B
B
B
�B
}B
�B	�B	��B	�tB	��B	��B	�!B	��B	�0B	��B	�VB	��B	��B
B
 ^B	��B	��B	��B
 "B
 �B
B
�B
�B
eB
�B
?B
1B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
&B
�B
�B
$B
MB
B
IB
TB
AB
,B
�B
�B
�B
#B
%B
SB
�B
=B
"B
RB
�B
|B
XB
WB
�B
sB
RB
�B
dB
mB
LB
WB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
	�B
	�B
	B
�B
�B
	
B
	B
	�B
�B
yB
�B
cB
	�B
�B
�B
vB
LB
-B
=B
�B
	B
�B
lB
�B
DB
�B
-B
�B

�B
	\B
�B
�B
yB
	B
B
�B
B

�B
#B

�B
9B
�B
�B
[B
`B
�B
OB
�B
�B
�B
B
eB
�B
B
B
�B
JB
pB
�B
�B
�B
�B
�B
 B
B
%B
�B
�B
�B
_B
B
oB
�B
�B
^B
�B
B
!B
�B
NB
`B
B
sB
�B
�B
]B
gB
�B
�B
B
�B
�B
UB
�B
�B
�B
�B
B
YB
�B
^B
SB
�B
CB
3B
�B
�B
�B
B
�B
B
WB
 	B
OB
�B
 �B
!>B
!]B
 �B
!B
!VB
!mB
!B
 �B
 �B
 {B
 �B
!B
"wB
$B
$�B
#�B
#�B
$0B
$yB
%<B
$&B
$HB
&B
$�B
$B
$gB
$�B
$�B
$~B
$�B
$�B
$�B
$�B
$�B
$�B
%}B
&#B
&�B
'\B
'>B
'�B
(�B
(HB
(iB
(mB
(�B
)�B
*sB
*4B
*B
*TB
*�B
+�B
+#B
*�B
*�B
*�B
+wB
,B
+�B
,B
,
B
,UB
,6B
,*B
,B
,DB
,SB
,�B
.B
-�B
-�B
.�B
/B
/`B
/^B
/�B
/�B
0:B
0�B
1B
1QB
1�B
1�B
1�B
2+B
2�B
3�B
3TB
30B
3qB
3�B
40B
47B
4�B
4�B
5B
5�B
5�B
6�B
71B
6�B
8�B
91B
8RB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9%B
9_B
9�B
:iB
:\B
:�B
:�B
:�B
:�B
;B
;%B
;B
;B
;B
;1B
;EB
<9B
<�B
<�B
<�B
<�B
=qB
=�B
=�B
=,B
=eB
=vB
=�B
=�B
=�B
=�B
>EB
>�B
>�B
>�B
?:B
?RB
?uB
?zB
?�B
?�B
@NB
@�B
@�B
@�B
AvB
AhB
ABB
B;B
A�B
A�B
B
B
BBB
A�B
BB
B[B
BqB
B�B
BVB
BcB
B�B
DB
D�B
D�B
E�B
E�B
E�B
F�B
FRB
F�B
GB
GjB
G�B
H�B
H�B
I4B
I7B
I�B
I�B
J>B
I�B
I�B
JtB
J�B
K@B
K�B
K�B
LDB
MxB
M�B
M�B
M�B
M�B
N�B
N�B
OYB
O�B
O�B
P�B
P�B
P7B
P�B
P`B
PB
QCB
Q�B
QcB
QVB
Q�B
RpB
R�B
S<B
TnB
U�B
V�B
V�B
V�B
V�B
V�B
V�B
W%B
WZB
W�B
X�B
X�B
X�B
YzB
ZB
Z/B
ZhB
ZwB
ZEB
ZFB
Z�B
Z�B
Z�B
[�B
\�B
]#B
]B
] B
]5B
^B
^B
^B
^B
^IB
^nB
^
B
^6B
^�B
^�B
^�B
^�B
_,B
^�B
^�B
_*B
_B
^�B
_B
_aB
_�B
_{B
_vB
_�B
`�B
`aB
`KB
`B
_�B
_�B
`rB
`FB
_�B
_�B
`uB
`�B
`�B
`sB
`�B
`�B
`�B
aIB
a0B
`�B
a<B
a�B
bB
b�B
bsB
bgB
bfB
bvB
cB
cB
b�B
c B
c<B
c0B
c<B
dB
dCB
d#B
dHB
e$B
e�B
fB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
fmB
f�B
f�B
fTB
f�B
gjB
f�B
f�B
glB
g=B
f�B
gBB
g�B
g�B
g�B
hDB
h�B
hxB
h�B
h�B
h�B
ibB
i�B
jlB
k�B
k�B
k&B
k2B
k�B
khB
k6B
kyB
kDB
k�B
k�B
k�B
k�B
k�B
lB
lB
l(B
lB
k�B
k�B
lB
lqB
l�B
l^B
l;B
lmB
ljB
l�B
l�B
m{B
m�B
m�B
m�B
mwB
m�B
nVB
nDB
m�B
m�B
nB
n(B
n�B
nQB
n�B
o;B
o�B
ptB
pEB
p'B
pXB
p�B
p�B
rB
q�B
rhB
r�B
s�B
t$B
s�B
s�B
tB
tB
tMB
t`B
tAB
t�B
u	B
uPB
uOB
u�B
u>B
u=B
u@B
u�B
u�B
u�B
u�B
v%B
v�B
v�B
v�B
v�B
v�B
w5B
wdB
w�B
xFG�O�B
\B
:B
�B
�B
4B
�B
�B
�B
�B
.B
�B
�B
�B
oB
�B
oB
�B
:B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
oB
�B
B
�B
�B
4B
 B
oB
B
�B
�B
bB
hB
�B
�B
hB
�B
�B
hB
.B
�B
�B
_B
7B
oB
B
�B
PB
�B
�B
oB
$B
�B
B
MB
B
B
{B
�B
�B
�B
B
�B
MB
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<$<#�
<�j�<���<o��<#�
<#�
<�@�<���<�W�<p6�<#�
<#�
<��<E%I<#�
<#�
<7=�<I%<#�
<#�
<#�
<$,<#�
<#�
<g�<}r�<#�
<#�
<#�
<S4�<�}(<�5<�5�<)$�<#�
<�p�<#�
<#�
<#�
<#�
<#�
<b��<3��<W`�<#�
<#�
<#�
<~�<#�
<#�
<#�
<'��<#�
<UC�<^�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<H9!<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021071005241920210710052419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021072007022620210720070226QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021072007022620210720070226QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364820220422133648IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064420220426160644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064420220426160644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064420220426160644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                