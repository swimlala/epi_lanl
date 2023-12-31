CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:14:09Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        (  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ?�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  A$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  GL   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  H�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  O    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  U(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  V�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ^h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  d�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  j�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  lD   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  rl   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  s�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  z    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    zP   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    }P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �P   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141409  20181024141409  5904969 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               .A   AO  6784                            2B  A   APEX                            7725                            111215                          846 @�����I1   @���)�,@2�33333�c϶E���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      .A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3D � D  D� D  D� D��D� DfD�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� Dy�RD�(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B�L�B�L�B��gB��B��B��B��B��B��B��B��B��B��B�L�B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C�C�C�C&gC�C�C �C"�C$�C%�3C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C���D �3D3D�3D3D�3D��D�3D	�D��D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3Dy��D�*>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A��
A��
A���A���A�ƨA���AݑhA�t�A�l�A�\)A�A�A�  A��/A�|�A�z�A�|�A�ĜA�K�A�A�A��A�E�A��HAִ9A֝�A֍PA�7LA��TAոRA՛�A�^5A�M�A��A��#AԴ9A�|�A�K�A�p�A��
A��A�C�AΕ�A�t�A��AˮA�$�A�E�A�hsA�bAǴ9A�ĜA�M�A��Aě�A���A�/A�ffA���A�XA�+A�bA�G�A�ȴA��-A�Q�A��A�K�A��A��RA�{A�5?A���A�^5A�oA��HA���A��hA���A�VA�VA�=qA��HA���A�33A��TA��A��mA���A�x�A���A��A���A��wA��A�{A�x�A��A�G�A�(�A�"�A��HA���A�-A���A���A��HA��A���A�A�+A�"�A�VA}O�A{
=Ay�;At�Ar�Am��AlI�Ak33Aj��Aj  Ah��AgAbĜAa33A`�A`1'A]�AY�AY"�AW��AUASK�APv�AN��AN$�AM�hAK��AI�7AG��AE?}AB�AA��A@��A?+A=�7A<JA9��A7XA5��A3ƨA2��A2��A2VA1�A0{A.��A.{A-�7A,�yA,I�A,bA+�A*�jA)A(��A'�^A&�\A#�A#l�A"�!A!G�A!�A �Az�A�AjA+AA�A��AE�A��A&�A�A��A��A�A�HA�AA�\A�A?}A��A�A	`BAp�A��AJA	33A�A�@��\@��@�dZ@�$�@���@�O�@�9X@��@��@���@�j@�ff@��P@���@�7L@��@��+@���@�Ĝ@�Q�@�@�(�@��@㕁@��@��
@�K�@�A�@ۍP@ڰ!@؃@ם�@ش9@�$�@���@�9X@�S�@���@ج@�A�@�;d@�~�@���@�\)@��@ʟ�@��y@�^5@�p�@��@ź^@�C�@�n�@�ff@�%@�@��@�ff@�l�@�t�@��@ɲ-@�&�@ȋD@�ƨ@���@ċD@¸R@���@�7L@���@���@��@�J@őh@�O�@��@��@��@���@���@�bN@�|�@��;@� �@�Q�@�z�@Ĵ9@Ĵ9@ă@�G�@��@ēu@�$�@��@�1@�S�@�S�@�n�@�J@�v�@�j@�@���@��7@�Ĝ@��D@�&�@��@�C�@���@���@�hs@��D@���@��@�&�@��m@���@��m@���@���@�E�@�J@��T@���@�x�@��@�V@�$�@�V@�
=@�bN@���@�A�@�  @���@�z�@��D@�bN@��m@�"�@���@���@��y@��\@��+@��j@��H@�@�n�@���@�p�@�7L@��\@�$�@�`B@�p�@��@��@�hs@���@�^5@��@��#@���@��7@�X@���@�Ĝ@��@�j@�bN@�bN@�Z@�Z@�9X@��@��@��;@�dZ@��\@�~�@�v�@�E�@���@��T@��@�O�@��@}/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��
A��
A��
A���A���A�ƨA���AݑhA�t�A�l�A�\)A�A�A�  A��/A�|�A�z�A�|�A�ĜA�K�A�A�A��A�E�A��HAִ9A֝�A֍PA�7LA��TAոRA՛�A�^5A�M�A��A��#AԴ9A�|�A�K�A�p�A��
A��A�C�AΕ�A�t�A��AˮA�$�A�E�A�hsA�bAǴ9A�ĜA�M�A��Aě�A���A�/A�ffA���A�XA�+A�bA�G�A�ȴA��-A�Q�A��A�K�A��A��RA�{A�5?A���A�^5A�oA��HA���A��hA���A�VA�VA�=qA��HA���A�33A��TA��A��mA���A�x�A���A��A���A��wA��A�{A�x�A��A�G�A�(�A�"�A��HA���A�-A���A���A��HA��A���A�A�+A�"�A�VA}O�A{
=Ay�;At�Ar�Am��AlI�Ak33Aj��Aj  Ah��AgAbĜAa33A`�A`1'A]�AY�AY"�AW��AUASK�APv�AN��AN$�AM�hAK��AI�7AG��AE?}AB�AA��A@��A?+A=�7A<JA9��A7XA5��A3ƨA2��A2��A2VA1�A0{A.��A.{A-�7A,�yA,I�A,bA+�A*�jA)A(��A'�^A&�\A#�A#l�A"�!A!G�A!�A �Az�A�AjA+AA�A��AE�A��A&�A�A��A��A�A�HA�AA�\A�A?}A��A�A	`BAp�A��AJA	33A�A�@��\@��@�dZ@�$�@���@�O�@�9X@��@��@���@�j@�ff@��P@���@�7L@��@��+@���@�Ĝ@�Q�@�@�(�@��@㕁@��@��
@�K�@�A�@ۍP@ڰ!@؃@ם�@ش9@�$�@���@�9X@�S�@���@ج@�A�@�;d@�~�@���@�\)@��@ʟ�@��y@�^5@�p�@��@ź^@�C�@�n�@�ff@�%@�@��@�ff@�l�@�t�@��@ɲ-@�&�@ȋD@�ƨ@���@ċD@¸R@���@�7L@���@���@��@�J@őh@�O�@��@��@��@���@���@�bN@�|�@��;@� �@�Q�@�z�@Ĵ9@Ĵ9@ă@�G�@��@ēu@�$�@��@�1@�S�@�S�@�n�@�J@�v�@�j@�@���@��7@�Ĝ@��D@�&�@��@�C�@���@���@�hs@��D@���@��@�&�@��m@���@��m@���@���@�E�@�J@��T@���@�x�@��@�V@�$�@�V@�
=@�bN@���@�A�@�  @���@�z�@��D@�bN@��m@�"�@���@���@��y@��\@��+@��j@��H@�@�n�@���@�p�@�7L@��\@�$�@�`B@�p�@��@��@�hs@���@�^5@��@��#@���@��7@�X@���@�Ĝ@��@�j@�bN@�bN@�Z@�Z@�9X@��@��@��;@�dZ@��\@�~�@�v�@�E�@���@��T@��@�O�@��@}/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�\B
�DB
y�B
`BB
O�B
Q�B
S�B
O�B
P�B
Q�B
P�B
T�B
T�B
T�B
VB
]/B
hsB
o�B
x�B
�B
�1B
�1B
�oB
��B
��B
��B
��B
��B
�wB
ĜB
��B
�B
�BB(�B1'BH�Bm�Bw�B� B�\B��B�'B��B�B�mB�B��B��BB	7B�B�B�B�BoB\BPBPB	7B
=B
=BPB1B��B�B�5B��B�'B��B��B��B��B�BaHBaHBcTBO�BA�B/B/B�B,B�BVBB
�B
�NB
ȴB
�3B
��B
�7B
iyB
ZB
N�B
=qB
�B
hB
  B	�B	�ZB	��B	�jB	��B	�{B	�PB	�=B	�B	}�B	s�B	cTB	T�B	Q�B	M�B	F�B	1'B	,B	%�B	�B	hB	+B��B��B��B�B�B�sB�NB�5B�B�
B��B��BȴB��BĜBƨBǮBǮBƨBƨBȴBǮBɺBɺBǮBɺBɺBȴBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Bx�B{�B�B��B��B��B��B�?B�!B�RB�wB�^B�FB��B��B��B�\B��B�uB�{B�BÖB�RB�Bl�B_;B_;B\)B[#B^5B~�B�B��B��B��B�hB��B�-B�9B�B��B��B��B�\B�PB�VBm�BdZBe`Be`B[#BN�BK�BM�BP�BVB_;Bo�Bx�Bw�Bx�B�B�PB�JB�1B�B~�B{�Bz�B`BB]/B[#Bn�Bp�BffBo�Bm�Bk�Bw�B��B�B�?B�jB�}BÖB��B��B��B��B��B�RB�!B��B��B�9B��B��B�5B�`B�`B�TB�ZB�fB�sB�yB�B�B��B��B	B	B	%B	PB	bB	�B	&�B	%�B	�B	�B	�B	�B	 �B	"�B	'�B	"�B	-B	D�B	E�B	B�B	A�B	A�B	G�B	J�B	D�B	E�B	>wB	1'B	2-B	+B	&�B	%�B	&�B	)�B	+B	,B	2-B	6FB	7LB	9XB	9XB	;dB	=qB	B�B	K�B	N�B	R�B	^5B	_;B	W
B	W
B	YB	^5B	aHB	cTB	iyB	l�B	{�B	{�B	|�B	|�B	�B	}�B	|�B	z�B	�B	�B	�B	�+B	��B	��B	��B	��B	��B	�B	�B	�-B	�XB	�^B	�dB	�jB	�jB	�jB	�jB	�wB	�}B	�}B	��B	B	ÖB	ĜB	ĜB	ĜB	ƨB	ǮB	ȴB	ɺB	ɺB	ȴB	ȴB	ȴB	ȴB	ɺB	��B
 B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�\B
�DB
y�B
`BB
O�B
Q�B
S�B
O�B
P�B
Q�B
P�B
T�B
T�B
T�B
VB
]/B
hsB
o�B
x�B
�B
�1B
�1B
�oB
��B
��B
��B
��B
��B
�wB
ĜB
��B
�B
�BB(�B1'BH�Bm�Bw�B� B�\B��B�'B��B�B�mB�B��B��BB	7B�B�B�B�BoB\BPBPB	7B
=B
=BPB1B��B�B�5B��B�'B��B��B��B��B�BaHBaHBcTBO�BA�B/B/B�B,B�BVBB
�B
�NB
ȴB
�3B
��B
�7B
iyB
ZB
N�B
=qB
�B
hB
  B	�B	�ZB	��B	�jB	��B	�{B	�PB	�=B	�B	}�B	s�B	cTB	T�B	Q�B	M�B	F�B	1'B	,B	%�B	�B	hB	+B��B��B��B�B�B�sB�NB�5B�B�
B��B��BȴB��BĜBƨBǮBǮBƨBƨBȴBǮBɺBɺBǮBɺBɺBȴBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Bx�B{�B�B��B��B��B��B�?B�!B�RB�wB�^B�FB��B��B��B�\B��B�uB�{B�BÖB�RB�Bl�B_;B_;B\)B[#B^5B~�B�B��B��B��B�hB��B�-B�9B�B��B��B��B�\B�PB�VBm�BdZBe`Be`B[#BN�BK�BM�BP�BVB_;Bo�Bx�Bw�Bx�B�B�PB�JB�1B�B~�B{�Bz�B`BB]/B[#Bn�Bp�BffBo�Bm�Bk�Bw�B��B�B�?B�jB�}BÖB��B��B��B��B��B�RB�!B��B��B�9B��B��B�5B�`B�`B�TB�ZB�fB�sB�yB�B�B��B��B	B	B	%B	PB	bB	�B	&�B	%�B	�B	�B	�B	�B	 �B	"�B	'�B	"�B	-B	D�B	E�B	B�B	A�B	A�B	G�B	J�B	D�B	E�B	>wB	1'B	2-B	+B	&�B	%�B	&�B	)�B	+B	,B	2-B	6FB	7LB	9XB	9XB	;dB	=qB	B�B	K�B	N�B	R�B	^5B	_;B	W
B	W
B	YB	^5B	aHB	cTB	iyB	l�B	{�B	{�B	|�B	|�B	�B	}�B	|�B	z�B	�B	�B	�B	�+B	��B	��B	��B	��B	��B	�B	�B	�-B	�XB	�^B	�dB	�jB	�jB	�jB	�jB	�wB	�}B	�}B	��B	B	ÖB	ĜB	ĜB	ĜB	ƨB	ǮB	ȴB	ɺB	ɺB	ȴB	ȴB	ȴB	ȴB	ɺB	��B
 B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141409                              AO  ARCAADJP                                                                    20181024141409    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141409  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141409  QCF$                G�O�G�O�G�O�0               