CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-04-06T02:00:49Z creation      
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
resolution        >�EȠ     
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ     
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BT   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ip   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       RT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Yp   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       [8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  bT   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       d   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       k8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  rT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       t   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  {8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       }    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160815180054  20191118095935  1901006 US ARGO PROJECT                                                 BRECK OWENS                                                     PRES            TEMP            PSAL               �A   AO  3381                            2C  D   SOLO_W                          946                             1.20                            851 @��UC� `1   @��Ul���C�����@D��`�1   IRIDIUM Primary sampling: averaged [data averaged with equal weights into irregular pressure bins                                                                                                                                                                          A   A   A   @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D0@ D1� D2� D4  D5@ D6� D7� D9  D:@ D;� D<� D>  D?@ D@� DA� DC  DD@ DE� DF� DH  DI@ DJ� DK� DM  DN@ DO� DP� DR  DS@ DT� DU� DW  DX@ DY� DZ� D\  D]@ D^� D_� Da  Db@ Dc� Dd� Df  Dg@ Dh� Di� Dk  Dl@ Dm� Dn� Dp  Dq@ Dr� Ds� Du  Dv@ Dw� Dx� Dz  D{@ D|� D}� D  D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�@ D�� D�� D�  D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D0@ D1� D2� D4  D5@ D6� D7� D9  D:@ D;� D<� D>  D?@ D@� DA� DC  DD@ DE� DF� DH  DI@ DJ� DK� DM  DN@ DO� DP� DR  DS@ DT� DU� DW  DX@ DY� DZ� D\  D]@ D^� D_� Da  Db@ Dc� Dd� Df  Dg@ Dh� Di� Dk  Dl@ Dm� Dn� Dp  Dq@ Dr� Ds� Du  Dv@ Dw� Dx� Dz  D{@ D|� D}� D  D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�@ D�� D�� D�  D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Ae�;Ae�#Ae�mAe�Ae�Ae��Ae��Ae�Ae�Ae�Ae�Ae�TAe�Ae�Ae��Ac�FA`$�A\1AU\)AS��AS��AT�AU;dAU��AU�;ATbNAS�AS"�AR�uAQ�mAQt�APĜAO�
AN�DAM��AM7LAL�AL�9AL�AN�AM��AM��AM��AM��AM�AM�-AM7LALr�AK�AJ��AJffAI&�AH��AH�9AIAI�AI�AI+AI&�AI
=AH��AH��AH�AF�HAB�jAB�\ABA�AA�;AA�AA�A@=qA?�^A?�7A?��A?x�A?l�A?l�A?��A?�A?�
A?�A?�7A?C�A?;dA?A>�9A>$�A=�7A<�`A<1A;��A;;dA:��A8ZA7��A6�HA5hsA5�A4�A4~�A4 �A3`BA2��A2�`A2�uA1�#A1;dA0��A0ZA0�A/�TA/��A.r�A-�7A-�A,M�A+��A+`BA+;dA*�HA*z�A)�TA)A)t�A(��A(�A'�A'�^A'��A'�A%�;A%hsA%&�A$��A$�A#��A#%A"�9A"��A"ffA"$�A"bA"�A!��A!�mA!�;A!dZA!A �jA bNA   A�
Al�A;dA%A�uA5?A�PA�+A  A�7A�!AVA1A��AhsA�Av�A��A�A  A��A�A��AffA��A"�A?}AO�A�A�PA�hA�hA��AAx�Al�AXA&�AȴAn�A$�A �A�#A�hA�yAn�A�
A�AoA�yAv�A��A33Az�A�#Al�A/A�AoA�`A~�A�A
=qA	S�A	A�HA�9A��A��A^5A�AA�FA�A�A��AS�A�A9XA�A�A�^A+A{A7LA {@��
@�ƨ@��P@�o@��@���@�ff@�G�@��
@��H@�V@��h@��@�bN@��@��\@��\@�E�@��@��^@�V@�bN@�Z@�D@�V@��@�I�@��y@�v�@��@�@�@�V@��y@���@��;@�@���@�X@㕁@�x�@�G�@�?}@�?}@�Ĝ@�Ĝ@߾w@�b@�V@��m@�@��@�\)@с@��@�Ĝ@ˍP@��@˅@��@���@�n�@�;d@��`@�ȴ@�=q@�O�@�1'@�ȴ@�$�@� �@�5?@�O�@��`@�I�@��;@�hs@��@���@�  @�(�@���@�z�@�Z@�1'@��F@���@�{@��P@��T@�5?@��@��-@�x�@�$�@�{@��7@��@�O�@���@��`@��@�t�@���@��@�l�@��@��@�E�@�O�@�r�@��F@��@�v�@�ƨ@���@���@�J@��@��@��@���@�-@���@��9@�+@�&�@�1'@�~�@��@�r�@��@�+@�J@�hs@���@���@���@��j@�  @�;d@�n�@���@��@�b@��R@��@��D@�1'@��w@�;d@���@��\@�n�@��#@�`B@�%@�z�@�bN@
=@|��@z��@yhs@v��@v{@w;d@vE�@u�h@v@v�+@uO�@sC�@n��@l�j@l�D@lZ@l(�@kt�@j�@h1'@h �@h1'@h  @g�w@g�;@g
=@e�-@d�@dZ@d1@cS�@a��@a�7@`�`@_K�@^{@]�T@]/@\�j@\j@[�m@[dZ@Z^5@Yx�@Z~�@Z-@Yhs@X��@V�+@W�@Wl�@W
=@V$�@T��@U�@Q��@Lj@K��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Ae�;Ae�#Ae�mAe�Ae�Ae��Ae��Ae�Ae�Ae�Ae�Ae�TAe�Ae�Ae��Ac�FA`$�A\1AU\)AS��AS��AT�AU;dAU��AU�;ATbNAS�AS"�AR�uAQ�mAQt�APĜAO�
AN�DAM��AM7LAL�AL�9AL�AN�AM��AM��AM��AM��AM�AM�-AM7LALr�AK�AJ��AJffAI&�AH��AH�9AIAI�AI�AI+AI&�AI
=AH��AH��AH�AF�HAB�jAB�\ABA�AA�;AA�AA�A@=qA?�^A?�7A?��A?x�A?l�A?l�A?��A?�A?�
A?�A?�7A?C�A?;dA?A>�9A>$�A=�7A<�`A<1A;��A;;dA:��A8ZA7��A6�HA5hsA5�A4�A4~�A4 �A3`BA2��A2�`A2�uA1�#A1;dA0��A0ZA0�A/�TA/��A.r�A-�7A-�A,M�A+��A+`BA+;dA*�HA*z�A)�TA)A)t�A(��A(�A'�A'�^A'��A'�A%�;A%hsA%&�A$��A$�A#��A#%A"�9A"��A"ffA"$�A"bA"�A!��A!�mA!�;A!dZA!A �jA bNA   A�
Al�A;dA%A�uA5?A�PA�+A  A�7A�!AVA1A��AhsA�Av�A��A�A  A��A�A��AffA��A"�A?}AO�A�A�PA�hA�hA��AAx�Al�AXA&�AȴAn�A$�A �A�#A�hA�yAn�A�
A�AoA�yAv�A��A33Az�A�#Al�A/A�AoA�`A~�A�A
=qA	S�A	A�HA�9A��A��A^5A�AA�FA�A�A��AS�A�A9XA�A�A�^A+A{A7LA {@��
@�ƨ@��P@�o@��@���@�ff@�G�@��
@��H@�V@��h@��@�bN@��@��\@��\@�E�@��@��^@�V@�bN@�Z@�D@�V@��@�I�@��y@�v�@��@�@�@�V@��y@���@��;@�@���@�X@㕁@�x�@�G�@�?}@�?}@�Ĝ@�Ĝ@߾w@�b@�V@��m@�@��@�\)@с@��@�Ĝ@ˍP@��@˅@��@���@�n�@�;d@��`@�ȴ@�=q@�O�@�1'@�ȴ@�$�@� �@�5?@�O�@��`@�I�@��;@�hs@��@���@�  @�(�@���@�z�@�Z@�1'@��F@���@�{@��P@��T@�5?@��@��-@�x�@�$�@�{@��7@��@�O�@���@��`@��@�t�@���@��@�l�@��@��@�E�@�O�@�r�@��F@��@�v�@�ƨ@���@���@�J@��@��@��@���@�-@���@��9@�+@�&�@�1'@�~�@��@�r�@��@�+@�J@�hs@���@���@���@��j@�  @�;d@�n�@���@��@�b@��R@��@��D@�1'@��w@�;d@���@��\@�n�@��#@�`B@�%@�z�@�bN@
=@|��@z��@yhs@v��@v{@w;d@vE�@u�h@v@v�+@uO�@sC�@n��@l�j@l�D@lZ@l(�@kt�@j�@h1'@h �@h1'@h  @g�w@g�;@g
=@e�-@d�@dZ@d1@cS�@a��@a�7@`�`@_K�@^{@]�T@]/@\�j@\j@[�m@[dZ@Z^5@Yx�@Z~�@Z-@Yhs@X��@V�+@W�@Wl�@W
=@V$�@T��@U�@Q��@Lj@K��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��BȴB��Bp�BC�B��B�B�B1B{B�B�B�BPB	7BB��B��B�B�NB��BȴBÖB�}B�}BȴB��B��B��B��B��B��B��B��B��B�B�HB�NB�B��B�B�`B�mB�mB�yB�sB�`B�BB�;B�/BɺB�bB�\B�hB�uB�hB��B��B��B��B��B��B��B�!BB��B��B�)B�B�B�B�B��B��BƨB�}B�FB�-B�B��B�hB�JB�Bx�Bv�Bt�Br�Bo�Bl�Bk�BjBgmBaHB[#BW
BS�BQ�BQ�BO�BJ�BD�BA�B<jB6FB49B49B1'B/B)�B)�B'�B"�B �B�B�B�B�B\BJB
=B+BB
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
�B
�B
�B
�B
�B
�B
�B
�B
�mB
�HB
�#B
�
B
��B
��B
��B
��B
��B
��B
��B
��B
ȴB
ɺB
��B
��B
ȴB
��B
��B
��B
�B
�#B
�)B
�;B
�BB
�BB
�BB
�)B
�B
��B
��B
��B
��B
��B
��B
ȴB
ȴB
B
ÖB
�wB
�dB
�RB
�LB
�9B
�-B
�!B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�oB
�DB
�B
�B
�B
�B
� B
~�B
|�B
t�B
p�B
o�B
o�B
o�B
n�B
l�B
iyB
e`B
dZB
bNB
_;B
ZB
T�B
P�B
L�B
K�B
K�B
J�B
I�B
G�B
F�B
D�B
@�B
;dB
7LB
5?B
33B
0!B
0!B
,B
)�B
(�B
'�B
%�B
$�B
"�B
"�B
#�B
%�B
'�B
-B
+B
(�B
'�B
 �B
�B
�B
�B
bB
JB
DB
	7B
%B
  B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�#B	��B	ƨB	�jB	�9B	�B	��B	��B	��B	��B	��B	��B	�PB	�B	y�B	q�B	o�B	m�B	jB	hsB	l�B	y�B	�%B	�B	�B	�B	~�B	x�B	z�B	�B	�PB	�bB	�hB	��B	��B	��B	�{B	�hB	�PB	�B	~�B	�B	�B	�B	�1B	�JB	�PB	�DB	�DB	�JB	�JB	�DB	�=B	�%B	�B	� B	�B	�B	�B	�B	�B	� B	� B	~�B	|�B	y�B	v�B	u�B	w�B	x�B	w�B	v�B	w�B	x�B	y�B	y�B	z�B	{�B	y�B	w�B	v�B	u�B	t�B	r�B	q�B	o�B	m�B	l�B	l�B	k�B	k�B	m�B	o�B	o�B	p�B	p�B	q�B	r�B	t�B	t�B	v�B	y�B	z�B	|�B	� B	�B	�B	�B	�7B	�VB	�bB	�bB	�VB	�JB	�VB	�hB	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�3B	�LB	�dB	�jB	�qB	��B	B	ŢB	ǮB	ɺB	��B	��B	��B	�B	�)B	�BB	�ZB	�sB	�B	�B	��B	��B	��B	��B	��B	��B
1B
JB
hB
hB
hB
�B
�B
�B
�B
 �B
'�B
%�B
#�B
)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B�B��B��B��B�B��B��BσB�VB��B|xBUB+B��B�yB�B?BAB#�B�BnB
�B�B�8B��B�/B��BւB��BěB��B�rB��B��B��B�B��B��B�B SB�B�FB�B�B�B�vB��B�^B��B�XB�DB�B��B�zB�?B�rB�eBԺB��B�CB�{B�yB��B��B��B�!B��B��B��B��B�6B�#B��B��B�3B��B�7BحB��B�~BΌBȁB��B��B�ZB�B��B��B��B�By�BwXBu�Bs�Bq�Bm�Bk�BktBizBcB\�BW�BT�BR�BR�BSBMTBFBC�B>5B7B4�B52B2ZB0�B*mB*�B)�B#�B"NB`BBEB�B�BB�B�B�B �B
��B
�@B
��B
��B
�B
��B
�;B
�!B
�B
�:B
��B
��B
��B
��B
�2B
��B
�;B
�?B
��B
�B
�`B
�-B
��B
�|B
�hB
�	B
��B
ԬB
�B
�>B
�>B
�B
�yB
�B
��B
�WB
�*B
�TB
��B
�B
��B
��B
ۧB
�B
�6B
�bB
�~B
�$B
��B
�*B
�3B
҃B
��B
��B
˕B
��B
�wB
�yB
�pB
��B
�B
�RB
��B
��B
�B
��B
�JB
�B
��B
�B
��B
��B
��B
�BB
��B
�pB
�"B
��B
�B
�}B
��B
�>B
�(B
�B
��B
v�B
q�B
o�B
o�B
o�B
oiB
nqB
j�B
e�B
d�B
b�B
`�B
]OB
W�B
T'B
MhB
K�B
LB
KyB
JB
HB
GB
FFB
B�B
<�B
81B
6fB
4/B
1B
25B
,�B
*B
)kB
(lB
&JB
%�B
#�B
"�B
#�B
%)B
'sB
.�B
-B
)�B
*�B
#BB
�B
rB
�B
OB
�B
�B

XB
iB
�B	��B	�*B	��B	�B	�TB	�B	��B	�TB	�BB	�B	�6B	ֳB	��B	�XB	�B	�YB	��B	�uB	�IB	��B	��B	�jB	�.B	��B	}
B	r�B	qB	oZB	l�B	idB	i�B	v�B	��B	��B	�B	��B	��B	yHB	y�B	?B	��B	��B	��B	��B	��B	�IB	��B	��B	�B	��B	~�B	�;B	��B	��B	�HB	�pB	�!B	�jB	��B	��B	�tB	��B	�B	��B	��B	��B	�B	��B	�*B	��B	�fB	�2B	�*B	VB	~�B	{�B	xB	vGB	w�B	y|B	x�B	w�B	x$B	y3B	z~B	z�B	|0B	|�B	z�B	x�B	w$B	v B	uBB	soB	rB	p�B	n`B	m B	m[B	lB	lB	nB	o�B	p6B	qYB	q�B	rzB	sYB	uB	uB	w(B	z=B	z�B	}B	�gB	�oB	�^B	�B	�RB	��B	�!B	�
B	��B	�4B	��B	�B	��B	��B	��B	��B	�jB	��B	��B	��B	�B	�B	�B	�FB	��B	��B	�[B	�dB	��B	��B	��B	��B	�B	�
B	��B	��B	�B	�cB	�:B	�fB	��B	��B	�B	��B	��B	��B	��B	�B	�<B	�@B	��B
RB
�B
�B
B
JB
kB
�B
�B
�B
 �B
(�B
&�B
#�B
*11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�0<#ٛ<#�<#�<#�C<#�<#�C<#�{<#�<<#�<#��<#��<#׎<$/%<:��<`b[<zQj<��I<7�F<$/%<'�T<%��<%�<#�)</Dj<(>�<$��<%�L<&e�<%<�<&�R<)3-<-nV<(�-<%<$�j<#��<#�i<.u�<#�<$B�<#�E<#ף<#�8<#�0<%@�<'T�<(�\<'�<%B�<,}p<%MY<#�X<$7�<#�W<#�c<#�+<#׎<#�<$�<#�<#�E<7Ձ<pt�<%$<$y�<$�2<$��<$�J<(j�<%gB<$�<#�C<#�&<#�8<#�<<$<<#��<#��<#ܯ<$�k<$F9<#ۮ<$"2<$q@<%��<&4p<&|V<()+<%S<$�<&�3<>��<'uq<'��</`�<$�;<$f<$�<$��<'.<$�(<#�5<$��<'�<&R`<&<$aD<$Gd<$)
<$�<+��<(�<%it<'�<&U"<$Z�<$<<$��<$��<%�y<#��<$}�<&]p<$��<%��<$3U<#��<%�~<,P�<%{@<$^�<%͍<%^�<%�#<%��<$}�<#�5<$k<$?[<#��<#�c<#�<#�<#ܯ<%@�<$ѩ<$g�<$��<$Ş<$r<$��<$�<$,<%<$�(<&�+<*B�<%�d<%v�<(T�<$�	<$r�<$9�<$�J<%@�<%gB<%>�<#�<#��<$��<%�<$�<%�!<$�k<$ub<#�N<#��<$
�<#ܯ<#�{<#�*<'��<*��<$��<#��<#��<$�<$��<$�k<$_�<#�r<$J�<$}�<&/<%��<&y<$��<%<$�<%2?<%�!<'�|<'^m<&ke<% <$B�<#��<#�<$F<%
�<*9�<.3�<)K?<$��<#��<$�<#��<#��<$�b<4�U<'!]<$��<#ڑ<#�0<#�<$\"<&��<%,#<#�W<$<$5w<&�<+��<);-<+�<$ K<#ۮ<#�"<$><<#��<#�<#�a<%�@<'^m<%v�<$v�<$��<$�e<$}�<'.<$)
<#�<$ �<$�<#�g<$��<$�<#؄<#�<$@|<$�<&�<&��<$_�<)k�<(��<'��<$<<+��<*e<&\<$$<$�(<'�c<)w�<+"�<$ <#��<#��<$5w<#�r<%\\<#�E<)g�<-��<+n<4�<7Ձ<*i�</-�<,7�<&��<$5w<#��<'1;<&|V</�<5g�<-�`<+�X<$��<%�!<&O�<'T�<$��<)�]<*e<%Q�<$8�<$�Q<$e.<-��<#�a<% <)۟<#��<#�&<$/%<#�8<#�<$P�<$�!<%��<.O:<(�-<#�m<$T�<%�n<$ <$|d<#�r<$\"<#�r<#��<$�<#�l<$�<&��<)�<)��<$Y�<#�<<$x+<$Ş<%�R<%Oz<$��<$�.<#��<%�!<&��<%*<$.<#ڑ<$,<$Y�<$=<#�<#��<$(<$�<%,#<$+<$�<$��<#�N<#�m<$�<$F9<#��<$�	<$Y�<$ �<$Z�<$r<$r<$�<#�<$}<$:�<$ub<$Z�<$.<#�<#�<#�<#�W<#�D<#ڑ<#�g<#�<#�&<#�(<#�D<$F<$F9<$-<$�<$}�<#�<#�<#�4<#�&<#׎<#�8<#�	<$/%<%�M<$3U<#�o<#��<#��<#�&<$�<$&<#׺<#�
<#��<#��<#�<#�4<$ <#��<#�r<#�*<#�<$<<#�l<#�e<$<#��<#��<#�U<#�8<#ڑ<#ܯ<#��<#�N<#�<#�<#�]<#��<#��<$8�<#��<#؄<#�r<#�M<#�l<#ا<$)
<$e.<#�]<#�&PRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment                                                                                                                                                                            SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;                                                                                                               TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                   PSAL_ADJUSTED_ERR set to magnitude of thermal mass adjustment      PSAL_ADJ_ERR: max(0.01, CTM + resolution error)                                                                                                                                              201805250000002018052500000020180525000000  AO  ARGQQCPL                                                                    20160815180054  QCP$                G�O�G�O�G�O�5F006           AO  ARGQQCPL                                                                    20160815180054  QCF$                G�O�G�O�G�O�0               WHOIARSQWHQCV0.5                                                                20180525000000  QC                  G�O�G�O�G�O�                WHOIARSQ CTMV1.0                                                                20180525000000  IP                  G�O�G�O�G�O�                WHOIARSQCLIMN/A CSIRO,ArgoCARS2016                                              20180525000000  IP                  G�O�G�O�G�O�                WHOIARDUV_31V1.0                                                                20191118095935  UP                  G�O�G�O�G�O�                