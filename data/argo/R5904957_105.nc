CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:23Z creation      
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
_FillValue                  �  =$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  A�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Fl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  J    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  K   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  N�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  O�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Sh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  W   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  [�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  \�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  `d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    `�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    c�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    f�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  i�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    i�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  i�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    j   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    j    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    j$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         j4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         j8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        j<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    j@Argo profile    3.1 1.2 19500101000000  20181024140823  20181024140823  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               iA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @���ޠZ1   @���ffy@3�hr�!�c�=p��
1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      iA   A   A   @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"�C$�C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CQ�fCS�fCV  CX  CZ  C\  C^  C`  Ca�fCd  Cf�Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C��C��C�  C�  Dyu�D�C3D�M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @@��@�Q�@ÅAA!AAAaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�B�
B p�B(p�B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bx
>B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B���B�B�8RB�8RB�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�B�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C�C)C)C
)C)C)C)C)C)C)C)C�C)C)C )C"5�C$5�C&)C()C*)C,)C.)C05�C2)C4)C6)C8)C:)C<)C>)C@)CB)CD5�CF)CH)CJ)CL)CN)CP)CR�CT�CV)CX)CZ)C\)C^)C`)Cb�Cd)Cf5�Ch)Cj)Cl5�Cn)Cp)Cr)Ct)Cv)Cx)Cz�C|)C~)C�C�C�C�C�C�C�C�C�C�C�C��C��C��C�C�C�C�C�C�C�GC�GC�GC�C�C�C�GC�C�C�C�C�C��C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�GC�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�GC�C�C�C�C�C�C�C�C�C�C�GC�C�C�C�C��C��C�C�C�C��C��C�C�Dy|�D�F�D�QH111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bA�bA�bA�bA�oA�{A��A��A��A��A��A��A��A��A��A��A��A��A߾wAߓuA�p�A��A���A�
=A���A� �A�C�AҁA�jA��#A���A���A�x�A�XA�\)A�\)A�O�A��mA�v�A�{A�ĜA�;dA�`BA���A��A�O�A��#A�XA��A���A���A���A�\)A�=qA�+A�S�A��A�  A��#A�33A�A���A�G�A��PA���A�p�A�n�A�`BA�K�A�K�A�`BA�{A�VA���A��A��TA�~�A�A�VA��9A�M�A���A��uA�p�A���A�A�A��-A�VA��A��+A�bNA�E�A�{A���A�  A��A�%A�9XA��A���A�
=A���A�A�hsA�{A~��A|Q�Aw��Ar�+An�9Am;dAl��AlbAj=qAgO�Ae;dAc\)AbI�Aa�^A`��A_��A^��A^9XA];dA[\)AS�^AP=qAO/AMAK�;AJ�/AH$�AE�ADr�AC��A?�mA<JA;/A:�A9�;A8��A6�9A5�A5��A4��A3�hA1|�A0-A//A-�A,bA*=qA)A'��A'�
A'��A&ZA%`BA$bA#�A"��A!AA�A��A��AĜA��A�PA��A�Ax�A�TA��Az�AQ�A�mAbNA�PA&�A��A�AjAA�AXA�Ax�A"�A
��A
9XA	�A�yA~�A5?A�A33A%A��A"�AJA��AXA ��A 1@�J@�7L@�b@�
=@�O�@��;@�V@�~�@��#@��@�n�@��T@�b@�j@�=q@���@�l�@��@�O�@�t�@�`B@�1@���@ە�@�l�@�ȴ@���@�t�@�+@��@�E�@z��@n6�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�bA�bA�bA�bA�oA�{A��A��A��A��A��A��A��A��A��A��A��A��A߾wAߓuA�p�A��A���A�
=A���A� �A�C�AҁA�jA��#A���A���A�x�A�XA�\)A�\)A�O�A��mA�v�A�{A�ĜA�;dA�`BA���A��A�O�A��#A�XA��A���A���A���A�\)A�=qA�+A�S�A��A�  A��#A�33A�A���A�G�A��PA���A�p�A�n�A�`BA�K�A�K�A�`BA�{A�VA���A��A��TA�~�A�A�VA��9A�M�A���A��uA�p�A���A�A�A��-A�VA��A��+A�bNA�E�A�{A���A�  A��A�%A�9XA��A���A�
=A���A�A�hsA�{A~��A|Q�Aw��Ar�+An�9Am;dAl��AlbAj=qAgO�Ae;dAc\)AbI�Aa�^A`��A_��A^��A^9XA];dA[\)AS�^AP=qAO/AMAK�;AJ�/AH$�AE�ADr�AC��A?�mA<JA;/A:�A9�;A8��A6�9A5�A5��A4��A3�hA1|�A0-A//A-�A,bA*=qA)A'��A'�
A'��A&ZA%`BA$bA#�A"��A!AA�A��A��AĜA��A�PA��A�Ax�A�TA��Az�AQ�A�mAbNA�PA&�A��A�AjAA�AXA�Ax�A"�A
��A
9XA	�A�yA~�A5?A�A33A%A��A"�AJA��AXA ��A 1@�J@�7L@�b@�
=@�O�@��;@�V@�~�@��#@��@�n�@��T@�b@�j@�=q@���@�l�@��@�O�@�t�@�`B@�1@���@ە�@�l�@�ȴ@���@�t�@�+@��@�E�@z��@n6�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B"�B"�B#�B#�B"�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B!�B�B�B�B  B
�`B
��B�BZBt�Bz�By�B|�B�1B�uB��B��B��B��B�B�?B�B�TB�B1B\B�B(�B2-B/B49B:^B:^B;dB>wBJ�B]/B_;BVB<jB	7B��B��B��B  B��B��B49BhsBn�Bo�Bp�Bs�Bw�B� Br�BjBffBbNB]/BH�B:^B2-B,B"�B{BB�B�)B��BȴB�jB��BgmBQ�B;dB
��B
��B
B
�?B
�B
��B
��B
��B
�hB
�B
n�B
]/B
E�B
0!B
�B	�B	�B	��B	ȴB	��B	�-B	��B	�uB	�B	|�B	x�B	{�B	z�B	r�B	n�B	e`B	R�B	+B	!�B	�B	�B	PB	+B��B�B�B�yB�/B��B��B��B��BɺBƨBĜBB��B�jB�RB�?B�-B�B�B�B�B�9B�XB�wB�jB�qB�qB�dB�XB�9B��B��B��B�\B�bB��B��B��B��B��B��B��B��B��B�oB�VB�bB�{B�{B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�VB�bB�\B�\B�hB�hB��B��B��B��B��B��B��B��B��B��B��B��B�'B�LB�jB
�B
!�B
-]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B"�B"�B#�B#�B"�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B!�B�B�B�B  B
�`B
��B�BZBt�Bz�By�B|�B�1B�uB��B��B��B��B�B�?B�B�TB�B1B\B�B(�B2-B/B49B:^B:^B;dB>wBJ�B]/B_;BVB<jB	7B��B��B��B  B��B��B49BhsBn�Bo�Bp�Bs�Bw�B� Br�BjBffBbNB]/BH�B:^B2-B,B"�B{BB�B�)B��BȴB�jB��BgmBQ�B;dB
��B
��B
B
�?B
�B
��B
��B
��B
�hB
�B
n�B
]/B
E�B
0!B
�B	�B	�B	��B	ȴB	��B	�-B	��B	�uB	�B	|�B	x�B	{�B	z�B	r�B	n�B	e`B	R�B	+B	!�B	�B	�B	PB	+B��B�B�B�yB�/B��B��B��B��BɺBƨBĜBB��B�jB�RB�?B�-B�B�B�B�B�9B�XB�wB�jB�qB�qB�dB�XB�9B��B��B��B�\B�bB��B��B��B��B��B��B��B��B��B�oB�VB�bB�{B�{B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�VB�bB�\B�\B�hB�hB��B��B��B��B��B��B��B��B��B��B��B��B�'B�LB�jB
�B
!�B
-]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140823                              AO  ARCAADJP                                                                    20181024140823    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140823  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140823  QCF$                G�O�G�O�G�O�0               