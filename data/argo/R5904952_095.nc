CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:25Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  =\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >X   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  BD   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  G,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  K   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  L   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  P    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  P�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  X�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  ]�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  b�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    b�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    e�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    h�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  k�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    l    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    lP   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    l`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ld   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         lt   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         lx   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        l|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    l�Argo profile    3.1 1.2 19500101000000  20181005190525  20181005190525  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               _A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�˪�nr�1   @�˫��C0@1��1'�chj~��#1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      _A   A   A   @�ff@���A   A!��AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B ffB(  B/��B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C��C�  C��C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�\)@�A z�A"{AB{A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B/�RB8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B��)B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4!HC6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT!HCV�CW�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��
C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��
C��C��C��C��C��C��C��C��C��
C��C��C��C��
C��C��C��C��C��C��
C��
C��C��C��C��C��C��
C��C��C��C��C��C��
C��C��C��C��
C��
C��C��C��C��C��C��C��C��C��C��C��
C��
C��
C��C��C��C��C��
C��C��C��C��C��
C��C��C��C��C��C�qC���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�5?A�5?A�1'A�-A�+A�/A�/A�1'A�7LA�9XA�9XA�9XA�7LA�VA�`BA�hsA�jA�l�A�jA�jA�jA�hsA�ffA�dZA�`BA�XA�M�A�5?A��HA��A�l�A�  A֋DA�`BA�M�A�-A��/AЕ�A�"�AϓuA��/A��;A̬AˬA�7LA��A� �A��A�G�A�JA��TA�A�A��HA�&�Aá�A�G�A�x�A�9XA�ƨA��yA�S�A���A��A��FA�1A�A��A�z�A��#A���A�;dA��A���A�VA��A�A��A�  A�hsA�`BA�dZA���A�l�A�O�A�(�A�l�A�bNA��yA�JA���A�?}A�;dA��FA�(�A�C�A�{A�$�A�l�A�ƨA��A��A��A~=qA}�A}\)A}�Azn�AxJAv�Ar5?Ao&�AmC�AkoAh  Af�yAe7LAaA_C�A\ȴAZM�AX5?AV~�AQG�AP=qAOXAL�AK�AI/AF�AF1AEx�AD�9A@�/A>1'A<bA;33A9�A7hsA5G�A3�^A3/A2=qA0�A/�hA-�PA*�HA(��A&�A%��A%33A$$�A#S�A"��A"A�A!��A!\)A ��A�mA;dAQ�A�A�Al�A+Ar�A�At�A&�A�9AA�A�-A��A%A��A�-A��AffAJA�A�^A`BA33A��A9XA��A�AS�AĜA�TA
�\A	ƨA�jA=qA��A+A~�A��Ap�A �A��A��A r�@���@�{@�z�@��@�ȴ@�&�@�r�@�P@�ȴ@�(�@��@�P@陚@�z�@��@�-@�x�@��/@�I�@�9X@�C�@��@�(�@��@ݡ�@�5?@އ+@ޟ�@���@ݑh@ܴ9@�C�@�-@أ�@��@�  @���@׍P@��y@�5?@�@���@�r�@�  @�l�@�^5@�7L@��`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1'A�5?A�5?A�1'A�-A�+A�/A�/A�1'A�7LA�9XA�9XA�9XA�7LA�VA�`BA�hsA�jA�l�A�jA�jA�jA�hsA�ffA�dZA�`BA�XA�M�A�5?A��HA��A�l�A�  A֋DA�`BA�M�A�-A��/AЕ�A�"�AϓuA��/A��;A̬AˬA�7LA��A� �A��A�G�A�JA��TA�A�A��HA�&�Aá�A�G�A�x�A�9XA�ƨA��yA�S�A���A��A��FA�1A�A��A�z�A��#A���A�;dA��A���A�VA��A�A��A�  A�hsA�`BA�dZA���A�l�A�O�A�(�A�l�A�bNA��yA�JA���A�?}A�;dA��FA�(�A�C�A�{A�$�A�l�A�ƨA��A��A��A~=qA}�A}\)A}�Azn�AxJAv�Ar5?Ao&�AmC�AkoAh  Af�yAe7LAaA_C�A\ȴAZM�AX5?AV~�AQG�AP=qAOXAL�AK�AI/AF�AF1AEx�AD�9A@�/A>1'A<bA;33A9�A7hsA5G�A3�^A3/A2=qA0�A/�hA-�PA*�HA(��A&�A%��A%33A$$�A#S�A"��A"A�A!��A!\)A ��A�mA;dAQ�A�A�Al�A+Ar�A�At�A&�A�9AA�A�-A��A%A��A�-A��AffAJA�A�^A`BA33A��A9XA��A�AS�AĜA�TA
�\A	ƨA�jA=qA��A+A~�A��Ap�A �A��A��A r�@���@�{@�z�@��@�ȴ@�&�@�r�@�P@�ȴ@�(�@��@�P@陚@�z�@��@�-@�x�@��/@�I�@�9X@�C�@��@�(�@��@ݡ�@�5?@އ+@ޟ�@���@ݑh@ܴ9@�C�@�-@أ�@��@�  @���@׍P@��y@�5?@�@���@�r�@�  @�l�@�^5@�7L@��`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
>wB
<jB
<jB
<jB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
F�B
J�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
Q�B
P�B
O�B
N�B
S�B
XB
 �B
%B	��B	�B	�5B	��B	�fB	��B
JB
)�B
P�B
��B
�5B%B$�B;dBS�Be`Bq�By�B�=B�oB��B�B�!B�?BǮB�B��B	7BJBhB�B�B#�B,B0!B6FBB�BL�BL�BG�BA�B33B�sB��B�DBgmB]/BW
BF�B�B%B
�B
�BB
��B
�RB
m�B
ffB
W
B
:^B
2-B
%�B
�B
%�B
|�B
}�B
0!B	��B	��B	��B	�=B	��B	��B	�B	�B	��B	�}B	��B	��B	�PB	�1B	�JB	�+B	|�B	k�B	]/B	N�B	@�B	1'B	%�B	\B	+B	  B��B�B�B�sB�ZB�NB�/B�B��B��BȴBÖB�jB�LB�B��B��B��B��B��B��B��B��B��B��B�FB�?B�3B�?B�XB�RB�XB�^B�FB�3B�-B�?B�?B�9B�'B�B��B��B�B�B�B�!B�dB�jB�^B�RB�dB�}B�RB�XB�XB�XB�dB�jB�qB�}B��BÖBŢBŢBĜBÖB��B��B�}B�wB�qB�^B�RB�9B�'B�B�B�B�B�B�B�B�B�B�B�-B�3B�9B�RB�XB�LB�RB�LB�LB�XB�dB�jB�qB��B��BǮB�B�#B�HB�NB�mB�sB�sB�mB�`B�sB�B�B�B�B�B�B��B��B��B��B��B��B��22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
>wB
<jB
<jB
<jB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
F�B
J�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
Q�B
P�B
O�B
N�B
S�B
XB
 �B
%B	��B	�B	�5B	��B	�fB	��B
JB
)�B
P�B
��B
�5B%B$�B;dBS�Be`Bq�By�B�=B�oB��B�B�!B�?BǮB�B��B	7BJBhB�B�B#�B,B0!B6FBB�BL�BL�BG�BA�B33B�sB��B�DBgmB]/BW
BF�B�B%B
�B
�BB
��B
�RB
m�B
ffB
W
B
:^B
2-B
%�B
�B
%�B
|�B
}�B
0!B	��B	��B	��B	�=B	��B	��B	�B	�B	��B	�}B	��B	��B	�PB	�1B	�JB	�+B	|�B	k�B	]/B	N�B	@�B	1'B	%�B	\B	+B	  B��B�B�B�sB�ZB�NB�/B�B��B��BȴBÖB�jB�LB�B��B��B��B��B��B��B��B��B��B��B�FB�?B�3B�?B�XB�RB�XB�^B�FB�3B�-B�?B�?B�9B�'B�B��B��B�B�B�B�!B�dB�jB�^B�RB�dB�}B�RB�XB�XB�XB�dB�jB�qB�}B��BÖBŢBŢBĜBÖB��B��B�}B�wB�qB�^B�RB�9B�'B�B�B�B�B�B�B�B�B�B�B�-B�3B�9B�RB�XB�LB�RB�LB�LB�XB�dB�jB�qB��B��BǮB�B�#B�HB�NB�mB�sB�sB�mB�`B�sB�B�B�B�B�B�B��B��B��B��B��B��B��22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190525                              AO  ARCAADJP                                                                    20181005190525    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190525  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190525  QCF$                G�O�G�O�G�O�8000            