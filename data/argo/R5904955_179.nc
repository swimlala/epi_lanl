CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:07:40Z creation      
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
resolution        =���   axis      Z        0  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ?�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  A,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  G\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  H�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  O   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  UH   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  V�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ^�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  d�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  j�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  l|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  r�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  t8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  zh   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    z�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    }�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20181024140740  20181024140740  5904955 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6558                            2B  A   APEX                            7469                            062512                          846 @��$����1   @��%So�@4��Q��c�1&�x�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @@  @y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  Dy{�D�2�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@@  @y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  Dy{�D�2�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�l�A�ffA�dZA�l�A�v�A�z�A҉7AҋDA҇+A҃A�|�A�|�A�~�A�~�A�|�A�v�A�r�A�dZA�S�A�VA�E�A�1'A�oA���A��A��mA��#A�5?Aϣ�Aϥ�Aϥ�A�|�A�A�A͛�ÁA�r�A�`BA�VA�?}A�A�A��A��A�t�A�ĜA�
=A�Aŕ�AuA�&�A���A���A�/A�+A��mA��PA�&�A���A� �A���A��A�I�A��+A��
A�-A�r�A��mA��^A�1'A���A��A�x�A���A�bNA�5?A���A�%A�ffA��A��DA�\)A�1A�+A��\A��mA��hA�I�A�p�A�t�A���A�7LA��A�I�A���A�%A��A�?}A��^A��7A� �A���A�M�A�A�ƨA��wA�x�A�%A�bNA�I�A� �A�9XA��^A���A��A~�9A|�\Ay�7At$�Apv�Ak7LAf��Ae�^AcA_l�A^1A\I�A[�AY�wAY
=AXr�AW�AWO�AV�AU�AUK�ATQ�ASt�AR��ARQ�AQt�AP��AO�hAI�^AG&�AE�PADz�ACt�AAp�A@$�A>�+A>(�A=�A=C�A<�A:�A:�RA:VA8ZA25?A.=qA*bA)��A'�^A$�/A!�mAp�A��AƨA��A`BA��A��AJA��A��A��A�-A��A��A��A  AG�AA�RA^5A�A?}A��A?}A{A�A
�uA�\AA?}A%A�AffA{A�A(�A�FA\)A+AVA �A �A �@�ff@��@��F@��F@��
@��;@��
@�ƨ@��@�V@���@���@�t�@���@�{@��j@��@�M�@�x�@�bN@�b@��;@@�\)@�v�@홚@�hs@�u@���@�E�@���@�p�@�O�@蛦@�Q�@� �@�o@�O�@�(�@�F@�;d@��@�=q@�Ĝ@�5?@���@�^5@���@�33@���@���@�
=@�O�@�
=@ҟ�@�J@Ѻ^@�p�@ЋD@��@Ώ\@�E�@��T@��#@͉7@��@�Q�@�dZ@ʏ\@�O�@���@�1'@�+@ƸR@�5?@ź^@�hs@ě�@�r�@�Z@� �@���@ÍP@�\)@+@���@���@�bN@�|�@�-@���@���@��h@�/@��9@�1'@� �@� �@� �@�b@���@��
@�t�@�`B@�Z@���@���@��\@��+@�v�@�=q@��/@� �@��@���@��@��@�O�@�/@�%@�9X@�1@��;@�dZ@��\@�M�@��T@��h@�Ĝ@���@�j@�9X@���@��@�K�@�"�@�ȴ@�~�@�M�@��h@��D@�1@�+@��\@�v�@�v�@�n�@�M�@�M�@�V@�^5@�V@�V@�V@�E�@�$�@��-@��@�`B@��-@�$�@��!@��y@���@�ȴ@��@��H@���@��+@�M�@��7@��j@��F@��!@���@�v�@�@��D@���@��@��@�\)@�l�@�\)@�ƨ@���@��
@��@|N�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�l�A�ffA�dZA�l�A�v�A�z�A҉7AҋDA҇+A҃A�|�A�|�A�~�A�~�A�|�A�v�A�r�A�dZA�S�A�VA�E�A�1'A�oA���A��A��mA��#A�5?Aϣ�Aϥ�Aϥ�A�|�A�A�A͛�ÁA�r�A�`BA�VA�?}A�A�A��A��A�t�A�ĜA�
=A�Aŕ�AuA�&�A���A���A�/A�+A��mA��PA�&�A���A� �A���A��A�I�A��+A��
A�-A�r�A��mA��^A�1'A���A��A�x�A���A�bNA�5?A���A�%A�ffA��A��DA�\)A�1A�+A��\A��mA��hA�I�A�p�A�t�A���A�7LA��A�I�A���A�%A��A�?}A��^A��7A� �A���A�M�A�A�ƨA��wA�x�A�%A�bNA�I�A� �A�9XA��^A���A��A~�9A|�\Ay�7At$�Apv�Ak7LAf��Ae�^AcA_l�A^1A\I�A[�AY�wAY
=AXr�AW�AWO�AV�AU�AUK�ATQ�ASt�AR��ARQ�AQt�AP��AO�hAI�^AG&�AE�PADz�ACt�AAp�A@$�A>�+A>(�A=�A=C�A<�A:�A:�RA:VA8ZA25?A.=qA*bA)��A'�^A$�/A!�mAp�A��AƨA��A`BA��A��AJA��A��A��A�-A��A��A��A  AG�AA�RA^5A�A?}A��A?}A{A�A
�uA�\AA?}A%A�AffA{A�A(�A�FA\)A+AVA �A �A �@�ff@��@��F@��F@��
@��;@��
@�ƨ@��@�V@���@���@�t�@���@�{@��j@��@�M�@�x�@�bN@�b@��;@@�\)@�v�@홚@�hs@�u@���@�E�@���@�p�@�O�@蛦@�Q�@� �@�o@�O�@�(�@�F@�;d@��@�=q@�Ĝ@�5?@���@�^5@���@�33@���@���@�
=@�O�@�
=@ҟ�@�J@Ѻ^@�p�@ЋD@��@Ώ\@�E�@��T@��#@͉7@��@�Q�@�dZ@ʏ\@�O�@���@�1'@�+@ƸR@�5?@ź^@�hs@ě�@�r�@�Z@� �@���@ÍP@�\)@+@���@���@�bN@�|�@�-@���@���@��h@�/@��9@�1'@� �@� �@� �@�b@���@��
@�t�@�`B@�Z@���@���@��\@��+@�v�@�=q@��/@� �@��@���@��@��@�O�@�/@�%@�9X@�1@��;@�dZ@��\@�M�@��T@��h@�Ĝ@���@�j@�9X@���@��@�K�@�"�@�ȴ@�~�@�M�@��h@��D@�1@�+@��\@�v�@�v�@�n�@�M�@�M�@�V@�^5@�V@�V@�V@�E�@�$�@��-@��@�`B@��-@�$�@��!@��y@���@�ȴ@��@��H@���@��+@�M�@��7@��j@��F@��!@���@�v�@�@��D@���@��@��@�\)@�l�@�\)@�ƨ@���@��
@��@|N�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BH�BH�BI�BH�BH�BH�BH�BH�BH�BI�BJ�BK�BK�BK�BK�BL�BM�BO�BQ�BQ�BR�BVBYB]/B_;B_;B`BBn�B�^B��BBȴB�NB�B�B�B�B�B�B�B�B�B�B1'B9XB=qB+B)�B:^BH�BH�BG�BG�BL�BO�BP�BP�BT�BYB\)BcTBdZB`BB\)BVBK�B?}B9XB5?B33B/B+B%�B �B�B�BoB  B��B��B��B��B�sB�B��B��B�}B��B�+B=qBJ�B-B%B
�`B
��B
�qB
�-B
�?B
�'B
�B
��B
��B
��B
��B
��B
�7B
n�B
dZB
M�B
YB
W
B
,B
�B
#�B
hB	�B	�-B	�bB	bNB	T�B	\)B	\)B	F�B	:^B	0!B	+B	(�B	)�B	/B	/B	/B	.B	,B	'�B	#�B	�B	�B	�B	\B	
=B	B�B�B��BǮBB�qB�dB�^B�jB�jB�wB�XB�9B�9B�9B�B��B��B�JB�1B�B{�Bv�Bq�Bl�BiyBffBe`BffBq�Bv�Bs�Bt�Bz�B{�B|�B|�B{�By�Bx�By�By�Bx�Bw�Bw�Bw�By�B{�B{�B{�B~�B~�B~�B}�B}�B|�Bz�Bu�Bu�Bw�Bw�Bx�Bx�By�B{�Bz�By�Bx�B{�B~�B�B�B�B�B�B�B�B�B�+B�1B�1B�7B�DB�JB�PB�VB�VB�VB�VB�VB�\B�hB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�LB�RB�XB�qB�wBÖBƨBȴBĜBĜBƨBȴBȴB��B��B��B��B��B��B��B��B��B�
B�B�5B�;B�NB�mB�sB�B�B�B�B�B�B�B��B��B��B��B��B	B	%B	
=B	uB	�B	�B	�B	�B	�B	�B	$�B	%�B	&�B	'�B	(�B	)�B	,B	2-B	6FB	8RB	<jB	=qB	=qB	<jB	=qB	E�B	H�B	I�B	I�B	L�B	R�B	XB	ZB	]/B	aHB	aHB	`BB	aHB	cTB	dZB	ffB	gmB	k�B	k�B	l�B	m�B	n�B	q�B	r�B	r�B	t�B	w�B	y�B	z�B	}�B	~�B	�B	�+B	�7B	�7B	�7B	�DB	�JB	�PB	�PB	�VB	�VB	�VB	�VB	�VB	�\B	�uB	��B	��B	��B	��B	��B	�B	�-B	�9B	�?B	�FB	�FB	�FB	�?B	�3B	�'B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�9B	�9B	�FB	�LB	�LB	��B
 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BH�BH�BI�BH�BH�BH�BH�BH�BH�BI�BJ�BK�BK�BK�BK�BL�BM�BO�BQ�BQ�BR�BVBYB]/B_;B_;B`BBn�B�^B��BBȴB�NB�B�B�B�B�B�B�B�B�B�B1'B9XB=qB+B)�B:^BH�BH�BG�BG�BL�BO�BP�BP�BT�BYB\)BcTBdZB`BB\)BVBK�B?}B9XB5?B33B/B+B%�B �B�B�BoB  B��B��B��B��B�sB�B��B��B�}B��B�+B=qBJ�B-B%B
�`B
��B
�qB
�-B
�?B
�'B
�B
��B
��B
��B
��B
��B
�7B
n�B
dZB
M�B
YB
W
B
,B
�B
#�B
hB	�B	�-B	�bB	bNB	T�B	\)B	\)B	F�B	:^B	0!B	+B	(�B	)�B	/B	/B	/B	.B	,B	'�B	#�B	�B	�B	�B	\B	
=B	B�B�B��BǮBB�qB�dB�^B�jB�jB�wB�XB�9B�9B�9B�B��B��B�JB�1B�B{�Bv�Bq�Bl�BiyBffBe`BffBq�Bv�Bs�Bt�Bz�B{�B|�B|�B{�By�Bx�By�By�Bx�Bw�Bw�Bw�By�B{�B{�B{�B~�B~�B~�B}�B}�B|�Bz�Bu�Bu�Bw�Bw�Bx�Bx�By�B{�Bz�By�Bx�B{�B~�B�B�B�B�B�B�B�B�B�+B�1B�1B�7B�DB�JB�PB�VB�VB�VB�VB�VB�\B�hB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�LB�RB�XB�qB�wBÖBƨBȴBĜBĜBƨBȴBȴB��B��B��B��B��B��B��B��B��B�
B�B�5B�;B�NB�mB�sB�B�B�B�B�B�B�B��B��B��B��B��B	B	%B	
=B	uB	�B	�B	�B	�B	�B	�B	$�B	%�B	&�B	'�B	(�B	)�B	,B	2-B	6FB	8RB	<jB	=qB	=qB	<jB	=qB	E�B	H�B	I�B	I�B	L�B	R�B	XB	ZB	]/B	aHB	aHB	`BB	aHB	cTB	dZB	ffB	gmB	k�B	k�B	l�B	m�B	n�B	q�B	r�B	r�B	t�B	w�B	y�B	z�B	}�B	~�B	�B	�+B	�7B	�7B	�7B	�DB	�JB	�PB	�PB	�VB	�VB	�VB	�VB	�VB	�\B	�uB	��B	��B	��B	��B	��B	�B	�-B	�9B	�?B	�FB	�FB	�FB	�?B	�3B	�'B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�9B	�9B	�FB	�LB	�LB	��B
 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.00 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140740                              AO  ARCAADJP                                                                    20181024140740    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140740  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140740  QCF$                G�O�G�O�G�O�0               