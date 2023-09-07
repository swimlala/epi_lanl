CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  F   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:43Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       ?�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  D�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       F0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       KH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  P`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Q�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  V�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ]    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  b8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  h�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       i�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  n�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    o(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    r(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    u(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  x(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    xT   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    xX   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    x\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    x`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  xd   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    x�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    x�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    x�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         x�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         x�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        x�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    x�Argo profile    3.1 1.2 19500101000000  20181024140843  20181024140843  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @���w(s1   @����~f@56ȴ9X�d ě��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A�  A�33A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C��C�  C��C�  C��C��C��C��C�  C�  D   D � D  D�fDfD� D  D� D  D� DfD� D��D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� Dy��D�FfD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@��HAp�A!p�AAp�Aap�A��RA��A��RA��A��RAиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�aGB�.B�.B�.B�.B�.B�.B�.C 
C
C
C
C
C

C
C
C
C
C
C
C
C
C
C
C 
C"
C$
C%�pC(
C*
C,
C.
C0
C2
C4
C6
C8
C:
C<
C>
C@
CB
CD
CF
CH
CJ
CL
CN
CP
CR
CT
CV
CX
CZ
C\
C^
C`0�Cb
Cd
Cf
Ch0�Cj
Cl
Cn
Cp
Cr
Ct
Cv
Cx
Cz
C|
C~
C�pC���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C�RC��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC�RC�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C���C��C�RC�RC��C�RC��C�RC�RC�RC�RC��C��D �D ��D�D�)D)D��D�D��D�D��D)D��D�]D��D�D��D�D��D	)D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�]D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"]D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��Dy��D�IGD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�5?A�7LA�7LA�9XA�;dA�=qA�=qA�=qA�?}A�?}A�A�A�C�A�C�A�C�A�E�A�G�A�E�A�G�A�I�A�K�A�K�A�M�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�E�A���A�~�A�A���A��;A�^5A�/A���A��A��7A���A� �A�A��\A���A�=qA��mA�G�A���A�ƨA�z�A�1A�dZA�t�A��A�M�A��DA���A�|�A��A��A��A�"�A��+A�bA�jA�
=A��A���A�z�A��A�XA��A���A�ffA��RA�l�A��A���A��A�r�A� �A��A�-A{p�Azn�Ax�+Au;dAo�Ai�7AgAcdZAa7LA^VAV�RATffAR�9AP�uAO�AN1'ALbAG��ADACoAB-AAx�A@~�A@$�A?
=A=�A<v�A8�uA7�-A7t�A7+A6��A6�`A6�!A57LA3�#A2��A1&�A0(�A/t�A.��A,�A,jA,1'A+�;A+��A+&�A*ȴA*�A)dZA(ȴA(JA&��A& �A%XA$�`A$r�A#�TA#�A#|�A#hsA"�A!��A!A!dZA ��A�`A��A�uAE�Al�AA��AbA�#AjA{AXAVA�A-A�wA�`A9XA9XAJA33A�\A�#A�A/AI�A+A~�A(�A|�A33AȴAA
�A	�A��AXA��A��A�9A{A�jAffA �\@��#@�;d@�E�@��#@��h@�O�@���@���@��m@�M�@���@�@� �@���@@�K�@�=q@��-@���@�9@�A�@��@�h@���@�x�@��T@��@�dZ@���@�V@���@ܬ@�{@�r�@�  @�K�@��@�-@ՙ�@�?}@�Ĝ@ԋD@�1@�l�@�~�@�r�@�  @�+@Ώ\@�-@�j@˶F@�dZ@ʸR@�O�@�Q�@�+@���@�ȴ@��@�(�@���@�+@���@�$�@��-@�O�@���@�Q�@���@���@�x�@��@��@���@��j@��j@��D@�1@��!@�&�@��`@��@���@��@���@�J@�O�@��@��@�t�@��@��@�7L@�z�@�@��\@�n�@�M�@�M�@�ff@���@�+@�33@�|�@��@��@�%@��@�E�@�$�@��@�-@�5?@��@��@�/@���@�  @��y@�ȴ@���@�~�@�M�@�{@��#@��7@��@���@�0U@�P@eIR11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�5?A�7LA�7LA�9XA�;dA�=qA�=qA�=qA�?}A�?}A�A�A�C�A�C�A�C�A�E�A�G�A�E�A�G�A�I�A�K�A�K�A�M�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�E�A���A�~�A�A���A��;A�^5A�/A���A��A��7A���A� �A�A��\A���A�=qA��mA�G�A���A�ƨA�z�A�1A�dZA�t�A��A�M�A��DA���A�|�A��A��A��A�"�A��+A�bA�jA�
=A��A���A�z�A��A�XA��A���A�ffA��RA�l�A��A���A��A�r�A� �A��A�-A{p�Azn�Ax�+Au;dAo�Ai�7AgAcdZAa7LA^VAV�RATffAR�9AP�uAO�AN1'ALbAG��ADACoAB-AAx�A@~�A@$�A?
=A=�A<v�A8�uA7�-A7t�A7+A6��A6�`A6�!A57LA3�#A2��A1&�A0(�A/t�A.��A,�A,jA,1'A+�;A+��A+&�A*ȴA*�A)dZA(ȴA(JA&��A& �A%XA$�`A$r�A#�TA#�A#|�A#hsA"�A!��A!A!dZA ��A�`A��A�uAE�Al�AA��AbA�#AjA{AXAVA�A-A�wA�`A9XA9XAJA33A�\A�#A�A/AI�A+A~�A(�A|�A33AȴAA
�A	�A��AXA��A��A�9A{A�jAffA �\@��#@�;d@�E�@��#@��h@�O�@���@���@��m@�M�@���@�@� �@���@@�K�@�=q@��-@���@�9@�A�@��@�h@���@�x�@��T@��@�dZ@���@�V@���@ܬ@�{@�r�@�  @�K�@��@�-@ՙ�@�?}@�Ĝ@ԋD@�1@�l�@�~�@�r�@�  @�+@Ώ\@�-@�j@˶F@�dZ@ʸR@�O�@�Q�@�+@���@�ȴ@��@�(�@���@�+@���@�$�@��-@�O�@���@�Q�@���@���@�x�@��@��@���@��j@��j@��D@�1@��!@�&�@��`@��@���@��@���@�J@�O�@��@��@�t�@��@��@�7L@�z�@�@��\@�n�@�M�@�M�@�ff@���@�+@�33@�|�@��@��@�%@��@�E�@�$�@��@�-@�5?@��@��@�/@���@�  @��y@�ȴ@���@�~�@�M�@�{@��#@��7@��@���@�0U@�P@eIR11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�By�By�Bz�Bz�Bz�B{�Bz�Bz�Bz�Bz�B{�B{�Bz�Bz�B{�B{�Bz�B{�B|�B}�B|�B|�B|�B|�B|�B{�Bz�Bz�B{�B�7Br�Bp�Bl�BgmBdZBbNBe`Bq�Bn�BN�B>wB6FB-B)�B!�B�B�B{BPBB�B�fB�;B��BŢB�FB��B�bB�7Bm�Be`BbNBYBO�BG�B=qB9XB5?B#�BPBB
��B
�B
�`B
�5B
�yB
�B
�B
�B
��B
��B
ŢB
�B
�hB
o�B
e`B
S�B
49B	��B	��B	�3B	��B	��B	�1B	W
B	D�B	9XB	0!B	,B	&�B	�B	\B	B��B��B��B�B�B�B�ZB�)B��B��B��B��BɺBȴBŢBB�wB�XB�9B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�\B�VB�PB�PB�JB�DB�DB�7B�1B�%B�B�B�B� B~�B}�B|�Bz�Bx�Bu�Bu�Bt�Bt�Br�Bq�Bp�Bo�Bo�Bo�Bn�Bn�Bn�Bm�Bl�Bk�BiyBhsBgmBffBhsBjBk�BiyBu�Bw�Bt�Bn�Bl�Bl�Bt�B}�B~�B|�Bw�Br�Bk�BaHBaHBbNBdZBe`BgmBhsBiyBjBjBjBl�Bm�Bn�Bp�By�B� B�%B�B�B�B�hB��B��B��B��B��B��B��B��B��B�B�!B�'B�3B�9B�FB�RB�RB�^B�^B�dB�jB�qBĜBŢBǮBȴBɺB��B��B��B��B��B��B��B�)B�NB�B�B�B�B�B��B��B��B��B��B	B	%B	+B	+B	1B		7B	
=B	JB	JB	JB	JB	DB	
=B	+B	B��B�B�B�sB�mB�fB�TB�ZB�ZB�ZB�mB�B��B��B	B	%B	DB	{B	�B	!�B	%�B	7LB	H�B	YB	hsB	l�B	n�B	p�B	p�B	r�B	r�B	r�B	s�B	r�B	p�B	m�B	m�B	m�B	m�B	l�B	k�B	k�B	k�B	m�B	q�B
�B
�B
011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  By�By�Bz�Bz�Bz�B{�Bz�Bz�Bz�Bz�B{�B{�Bz�Bz�B{�B{�Bz�B{�B|�B}�B|�B|�B|�B|�B|�B{�Bz�Bz�B{�B�7Br�Bp�Bl�BgmBdZBbNBe`Bq�Bn�BN�B>wB6FB-B)�B!�B�B�B{BPBB�B�fB�;B��BŢB�FB��B�bB�7Bm�Be`BbNBYBO�BG�B=qB9XB5?B#�BPBB
��B
�B
�`B
�5B
�yB
�B
�B
�B
��B
��B
ŢB
�B
�hB
o�B
e`B
S�B
49B	��B	��B	�3B	��B	��B	�1B	W
B	D�B	9XB	0!B	,B	&�B	�B	\B	B��B��B��B�B�B�B�ZB�)B��B��B��B��BɺBȴBŢBB�wB�XB�9B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�\B�VB�PB�PB�JB�DB�DB�7B�1B�%B�B�B�B� B~�B}�B|�Bz�Bx�Bu�Bu�Bt�Bt�Br�Bq�Bp�Bo�Bo�Bo�Bn�Bn�Bn�Bm�Bl�Bk�BiyBhsBgmBffBhsBjBk�BiyBu�Bw�Bt�Bn�Bl�Bl�Bt�B}�B~�B|�Bw�Br�Bk�BaHBaHBbNBdZBe`BgmBhsBiyBjBjBjBl�Bm�Bn�Bp�By�B� B�%B�B�B�B�hB��B��B��B��B��B��B��B��B��B�B�!B�'B�3B�9B�FB�RB�RB�^B�^B�dB�jB�qBĜBŢBǮBȴBɺB��B��B��B��B��B��B��B�)B�NB�B�B�B�B�B��B��B��B��B��B	B	%B	+B	+B	1B		7B	
=B	JB	JB	JB	JB	DB	
=B	+B	B��B�B�B�sB�mB�fB�TB�ZB�ZB�ZB�mB�B��B��B	B	%B	DB	{B	�B	!�B	%�B	7LB	H�B	YB	hsB	l�B	n�B	p�B	p�B	r�B	r�B	r�B	s�B	r�B	p�B	m�B	m�B	m�B	m�B	l�B	k�B	k�B	k�B	m�B	q�B
�B
�B
011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140843                              AO  ARCAADJP                                                                    20181024140843    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140843  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140843  QCF$                G�O�G�O�G�O�0               