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
resolution        =���   axis      Z           9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        BX   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ix   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        K@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        R`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        [H   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  bh   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        d0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        kP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  rp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        t8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  {X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        }    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160805180057  20191118095932  1901006 US ARGO PROJECT                                                 BRECK OWENS                                                     PRES            TEMP            PSAL               �A   AO  3381                            2C  D   SOLO_W                          946                             1.20                            851 @����N1   @���""" �C�H��@D�ɴ�c1   IRIDIUM Primary sampling: averaged [data averaged with equal weights into irregular pressure bins                                                                                                                                                                          A   A   A   @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D0@ D1� D2� D4  D5@ D6� D7� D9  D:@ D;� D<� D>  D?@ D@� DA� DC  DD@ DE� DF� DH  DI@ DJ� DK� DM  DN@ DO� DP� DR  DS@ DT� DU� DW  DX@ DY� DZ� D\  D]@ D^� D_� Da  Db@ Dc� Dd� Df  Dg@ Dh� Di� Dk  Dl@ Dm� Dn� Dp  Dq@ Dr� Ds� Du  Dv@ Dw� Dx� Dz  D{@ D|� D}� D  D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�@ D�� D�� D�  D�@ D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D0@ D1� D2� D4  D5@ D6� D7� D9  D:@ D;� D<� D>  D?@ D@� DA� DC  DD@ DE� DF� DH  DI@ DJ� DK� DM  DN@ DO� DP� DR  DS@ DT� DU� DW  DX@ DY� DZ� D\  D]@ D^� D_� Da  Db@ Dc� Dd� Df  Dg@ Dh� Di� Dk  Dl@ Dm� Dn� Dp  Dq@ Dr� Ds� Du  Dv@ Dw� Dx� Dz  D{@ D|� D}� D  D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�@ D�� D�� D�  D�@ D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AK?}AKK�AKK�AKO�AKO�AKO�AKK�AKO�AKO�AKS�AKXAKXAKS�AKS�AKXAK\)AK`BAKXAKXAKXAK\)AK\)AKXAK\)AK\)AK\)AK`BAK`BAKhsAKhsAKhsAK`BAK\)AK`BAK`BAK\)AK`BAK`BAKdZAKp�AK�AK��AK��AK��AK��AK��AK��AK�AK�AK�7AK�7AK�PAK��AK�^ALbALE�ALQ�ALZALZAL^5ALffALI�AL9XAL9XAL1'AL-AL�AL �AL-AL-AL9XALbAK��AK+AJȴAJZAJbAI�AIƨAIl�AH~�AG��AF�+AEoAD~�AD(�AC�wAB��AAp�A@��A@��AA"�AA&�AA�AA
=A@�`A@��AA�AA\)AA&�AAA@Q�A?l�A>�9A=�A>�A@^5A@��AA%AA�A@��A@ĜA@�A?�;A?7LA>^5A=�A=�TA=��A<ȴA<�A=7LA<��A<�!A:��A9��A9G�A8�yA7`BA5�mA4�RA3��A3�A3;dA2�A2��A2A�A1�PA0ffA/��A/
=A/VA.��A.�HA-A+��A+�hA+`BA+C�A+&�A*ĜA*ZA*1'A*1A)��A)\)A(M�A($�A(�A'��A&ffA%�7A%;dA$JA"�A"v�A"-A!��A!�A ��A ��A �RA ĜA �RA �/A!�A!�A   Al�A�`AbNA�uA��A��A�AĜA�yA�AK�A`BAK�A+A�yA��A~�AbA�9AA�jAA�^AXA�/A��A�Al�AS�Av�A�jA��A5?AJAJA1A�hAffA�A�A�-A��AdZA�+A�hAAG�A%A
9XA	33A	%A	VA	A�A�#AȴA=qA�PAO�AĜA�jA�AjAM�A{A�7AVA1A�^A|�AXA�A ��A ��A �@�t�@��@��H@�v�@�E�@�@���@��@�E�@���@�33@��@��^@���@�1@�C�@�E�@�@�G�@�A�@���@���@�n�@�5?@�@��T@�  @�ȴ@�5?@�7@�ƨ@�9X@�33@�$�@ܛ�@�b@ۮ@��@�~�@�{@�%@�j@���@�t�@��@�X@��/@Ѓ@��m@υ@���@�=q@�`B@�+@ʇ+@���@��@�
=@ə�@�(�@�33@�7L@ģ�@�r�@�S�@�p�@�O�@�/@���@��@��w@�-@���@���@���@�^5@�V@�-@���@���@�x�@�7L@���@���@�O�@��9@�  @��F@�|�@�+@��!@��^@���@�Q�@�(�@�~�@��9@�Z@��F@��@���@�Ĝ@�1'@��@���@��@�j@�|�@�n�@�hs@�b@�v�@�&�@���@�|�@�M�@�ȴ@�x�@�  @���@�@�/@�r�@�S�@��@�ff@��-@���@�S�@���@��@�`B@�`B@�?}@�%@�1@�ȴ@�v�@���@��@�1@�ƨ@���@�\)@�ȴ@���@���@���@~�+@�@� �@~�y@}�T@y�@�@|��@|�/@z~�@y�^@w�;@t(�@r�H@r~�@p��@pr�@l�j@jn�@i%@hQ�@hQ�@hb@g�@g�@g�P@g\)@e�T@c�m@b�@b��@b~�@`�`@`Ĝ@`�9@ax�@`��@_�P@_�@^�+@^{@]��@]p�@\�/@\�j@\j@[�
@[�@[C�@Y��@X��@X�9@X�9@Vȴ@T��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AK?}AKK�AKK�AKO�AKO�AKO�AKK�AKO�AKO�AKS�AKXAKXAKS�AKS�AKXAK\)AK`BAKXAKXAKXAK\)AK\)AKXAK\)AK\)AK\)AK`BAK`BAKhsAKhsAKhsAK`BAK\)AK`BAK`BAK\)AK`BAK`BAKdZAKp�AK�AK��AK��AK��AK��AK��AK��AK�AK�AK�7AK�7AK�PAK��AK�^ALbALE�ALQ�ALZALZAL^5ALffALI�AL9XAL9XAL1'AL-AL�AL �AL-AL-AL9XALbAK��AK+AJȴAJZAJbAI�AIƨAIl�AH~�AG��AF�+AEoAD~�AD(�AC�wAB��AAp�A@��A@��AA"�AA&�AA�AA
=A@�`A@��AA�AA\)AA&�AAA@Q�A?l�A>�9A=�A>�A@^5A@��AA%AA�A@��A@ĜA@�A?�;A?7LA>^5A=�A=�TA=��A<ȴA<�A=7LA<��A<�!A:��A9��A9G�A8�yA7`BA5�mA4�RA3��A3�A3;dA2�A2��A2A�A1�PA0ffA/��A/
=A/VA.��A.�HA-A+��A+�hA+`BA+C�A+&�A*ĜA*ZA*1'A*1A)��A)\)A(M�A($�A(�A'��A&ffA%�7A%;dA$JA"�A"v�A"-A!��A!�A ��A ��A �RA ĜA �RA �/A!�A!�A   Al�A�`AbNA�uA��A��A�AĜA�yA�AK�A`BAK�A+A�yA��A~�AbA�9AA�jAA�^AXA�/A��A�Al�AS�Av�A�jA��A5?AJAJA1A�hAffA�A�A�-A��AdZA�+A�hAAG�A%A
9XA	33A	%A	VA	A�A�#AȴA=qA�PAO�AĜA�jA�AjAM�A{A�7AVA1A�^A|�AXA�A ��A ��A �@�t�@��@��H@�v�@�E�@�@���@��@�E�@���@�33@��@��^@���@�1@�C�@�E�@�@�G�@�A�@���@���@�n�@�5?@�@��T@�  @�ȴ@�5?@�7@�ƨ@�9X@�33@�$�@ܛ�@�b@ۮ@��@�~�@�{@�%@�j@���@�t�@��@�X@��/@Ѓ@��m@υ@���@�=q@�`B@�+@ʇ+@���@��@�
=@ə�@�(�@�33@�7L@ģ�@�r�@�S�@�p�@�O�@�/@���@��@��w@�-@���@���@���@�^5@�V@�-@���@���@�x�@�7L@���@���@�O�@��9@�  @��F@�|�@�+@��!@��^@���@�Q�@�(�@�~�@��9@�Z@��F@��@���@�Ĝ@�1'@��@���@��@�j@�|�@�n�@�hs@�b@�v�@�&�@���@�|�@�M�@�ȴ@�x�@�  @���@�@�/@�r�@�S�@��@�ff@��-@���@�S�@���@��@�`B@�`B@�?}@�%@�1@�ȴ@�v�@���@��@�1@�ƨ@���@�\)@�ȴ@���@���@���@~�+@�@� �@~�y@}�T@y�@�@|��@|�/@z~�@y�^@w�;@t(�@r�H@r~�@p��@pr�@l�j@jn�@i%@hQ�@hQ�@hb@g�@g�@g�P@g\)@e�T@c�m@b�@b��@b~�@`�`@`Ĝ@`�9@ax�@`��@_�P@_�@^�+@^{@]��@]p�@\�/@\�j@\j@[�
@[�@[C�@Y��@X��@X�9@X�9@Vȴ@T��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBbNBbNBbNBbNBbNBbNBbNBbNBcTBbNBbNBcTBcTBdZBdZBdZBdZBdZBe`Be`Be`Be`BffBffBe`Be`Be`Be`Be`Be`Be`BffBffBe`Be`Be`Be`Be`Be`BffBgmBjBk�BjBjBjBjBhsBiyBiyBjBjBm�Bo�By�B� B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� Bz�Bt�Bp�Bm�Bo�Bp�Bq�Bq�Bw�Bw�By�B|�B}�B~�B� B�B�DB�{B��B�B�?B�FB�FB�FB�^B�}BɺB��B��B��B�}B�XB�RB��B�B��B��B��B��B��B��B�B�B�NB�;B�5B�#B��B�B�;B�;B�BƨB�jB�XB�3B��B�oB�%B}�Bz�Bx�Bx�Bu�Bp�BhsB_;BXBQ�BT�BT�BT�BI�B8RB49B33B33B33B33B0!B/B.B,B)�B"�B �B�B�BhB	7BB
��B
�B
�B
�B
�mB
�`B
�HB
�HB
�ZB
�mB
�B
�B
��B
��B
�B
�B
�B
�B
�5B
�BB
�HB
�TB
�`B
�mB
�B
�B
�B
��B
��B
��B
��B
�B
�B
�fB
�#B
��B
ɺB
ŢB
��B
�dB
�!B
�B
�LB
�XB
�-B
��B
��B
�B
�-B
�9B
�9B
�9B
�B
�B
�B
�B
��B
��B
��B
��B
�JB
�B
� B
y�B
u�B
u�B
u�B
u�B
s�B
n�B
iyB
gmB
cTB
bNB
_;B
_;B
^5B
\)B
[#B
YB
W
B
R�B
Q�B
O�B
M�B
L�B
J�B
J�B
K�B
J�B
I�B
G�B
G�B
E�B
D�B
C�B
A�B
=qB
8RB
33B
0!B
/B
+B
(�B
%�B
"�B
�B
�B
�B
�B
oB
PB

=B
	7B
1B
+B
B
B
B	��B	��B	�B	�yB	�fB	�TB	�HB	�HB	�;B	�/B	�#B	�B	��B	��B	��B	ŢB	ĜB	ÖB	B	��B	�}B	�jB	�XB	�9B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�VB	�PB	�7B	�%B	�B	�B	�B	�B	�B	�B	� B	}�B	z�B	x�B	v�B	u�B	t�B	s�B	r�B	p�B	n�B	k�B	k�B	jB	gmB	e`B	o�B	p�B	p�B	r�B	v�B	w�B	v�B	u�B	s�B	r�B	q�B	o�B	n�B	n�B	m�B	m�B	l�B	l�B	jB	dZB	bNB	aHB	aHB	`BB	_;B	_;B	aHB	bNB	bNB	dZB	e`B	ffB	e`B	gmB	jB	o�B	s�B	r�B	q�B	q�B	s�B	w�B	z�B	|�B	}�B	}�B	~�B	}�B	}�B	}�B	}�B	~�B	�1B	�=B	�DB	�VB	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�XB	�wB	B	B	B	B	ǮB	��B	��B	��B	��B	��B	�B	�BB	�HB	�`B	�B	�B	��B	��B	��B
B
B
%B
	7B
PB
\B
�B
�B
"�B
&�B
(�B
-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bb-BbLBbCBbPBbPBbWBbDBbNBcHBbDBbPBc\BcVBdMBdNBdQBdnBd^Be`BeXBeaBelBf[BfdBeeBeXBe_BeJBebBeeBexBfnBfZBedBejBeXBe`BeVBeBBf,Bg9BjqBk�Bj�Bj�BjzBj�BhxBijBizBjnBjEBmOBn�By?B�B��B�B�B��B�_B�EB�B�,B�!B�GB��B��B�B��B��B�0B|5Bu�Bq�BnnBpBqBr�BtHBz,B{B}�B~�B~�B�AB�4B�LB��B�\B�ZB��B�DB�}B��B�B� B��B�8B�QB��B�@B��B�KB�B��B�0B�$B��B�6B�qB�@B�
B�B��B�B�wB�B�TBҫB�[B��B�GB޺B�_B��B��B�VB��B��B��B~�B{�By�By�Bv�Br�Bk�Ba�BY�BQ�BU1BU{BXBN�B9�B4�B3�B3�B4JB4TB0�B/�B.�B-ZB,�B#`B �B �B!tB�B
OB{B
��B
�^B
�\B
�B
�JB
��B
�,B
�-B
�<B
�B
�B
�B
��B
��B
�iB
�_B
�iB
��B
�B
�`B
�B
�B
��B
��B
�B
�^B
��B
� B
��B
��B
�EB
�B
�B
�3B
�B
�B
ʫB
��B
�B
�GB
�>B
�hB
��B
��B
��B
��B
�B
��B
�/B
�VB
��B
�nB
�B
��B
��B
�CB
��B
�pB
��B
�B
��B
�B
�\B
|�B
vZB
u�B
u�B
v\B
v�B
q�B
k7B
ieB
d+B
c�B
_eB
_uB
^�B
\�B
[�B
Z�B
ZkB
T B
R�B
P�B
NLB
M�B
K�B
K_B
MbB
K�B
JEB
HB
HGB
E�B
EB
D<B
CBB
@�B
;�B
4cB
0rB
1B
,4B
*fB
'B
$MB
 �B
EB
NB
�B
�B
cB

�B
	�B
�B
	�B
�B
 B
*B	��B	��B	�XB	�#B	�B	�;B	��B	�(B	�,B	��B	ܲB	�B	�RB	�B	�B	ƣB	�]B	�.B	ÀB	�)B	�YB	��B	��B	�B	�B	��B	��B	��B	�EB	�,B	�~B	��B	��B	�4B	��B	�rB	��B	��B	��B	�B	��B	��B	�B	��B	��B	�*B	�>B	�jB	��B	�lB	�ZB	�~B	�B	��B	}B	y�B	w�B	vCB	u%B	tBB	s�B	r&B	pB	lYB	k�B	mB	jB	e�B	pB	q&B	qGB	s}B	w0B	x+B	wwB	v�B	toB	sNB	rYB	pOB	ozB	o�B	nqB	nXB	l�B	mLB	l�B	eHB	cHB	bB	a�B	`�B	_�B	_�B	a�B	b�B	b�B	d�B	fYB	f�B	f"B	g�B	j�B	o�B	s�B	sYB	r{B	q�B	tBB	xxB	{QB	}"B	~B	~.B	\B	~�B	~!B	~�B	~�B	~�B	�!B	��B	��B	��B	��B	�]B	��B	��B	�2B	��B	�'B	�]B	�B	��B	�+B	�%B	��B	�rB	�FB	�$B	�PB	�kB	��B	B	§B	�B	�>B	�B	��B	��B	�ZB	��B	�B	��B	�hB	��B	�B	��B	��B	��B	��B	�B
B
<B
\B
	YB
hB
�B
�B
�B
"�B
'7B
)EB
-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�]<#�<#�i<#�<#�<#�I<#�X<#�
<#�{<#�X<#�<#�<<#�<#׎<#�{<#�I<#�C<#�<#�
<#�<<#�<#�{<#�i<#�<#�<#�<<#�<#؄<#�<#�<#��<#�<<#�{<#�<#�X<#�<<#�
<#�X<#��<#�N<#�J<#ף<#�<#�<#ا<#�<#ߜ<#�<#׺<#�<#��<#�N<#�U<$v�<$!><#��<#؄<#�<#�X<#��<#�!<#��<#�<#�<#׺<#ߜ<#�<#�o<#�X<#�C<$p<$�!<%6Z<$�1<$��<$k�<#�Q<#��<$�7<)N<(%�<+��</��<%��<$�e<%<+�)<+��<%:{<#��<#�<#�<#�<#�E<#�(<#�<#��<$(<$v<#��<&�k<(�(<'Dv<&�+<'��</��<%D�<$}<#��<#�<$a<#�)<'��<&��<'��<%<#�<$k�<'��<#�l<$:�<$�<$��<;�<)�<$�t<$�!<0�><1�<,sq<(�|<$�j<$Y�<$v�<$|d<$�;<'�<+��<()+<%�<#�0<#��<$�<+"�<51<%:{<$�<#�"<#�<$��<$�(<$�<$v<$<<%2?<)�<$f<#ޫ<$�3<.�<(��<$Ş<,��<.�<$^�<$\"<$��<$k�<%�y<$ub<#�D<#��<#ٛ<#�Q<$}<#��<*1#<&D�<&�^<E��<#�<#��<#��<#�J<#�M<#��<$�<$�<#��<#�e<#��<$@|<$?[<$G<%Oz</f�<)۟<*�<':�<$��<$�<%�V<*9�<'J�<$+<#�W<)�<4��<$Sa<$�;<$v<#�<#ٛ<%k�<+��<$��<$<<$5w<#��<$W<(��<*{�<27a<'�:<$��<(!�<*a<$�<#�I<#ܯ<$k<*��<+H<&1�<&�3<$c�<%�#<#�l<#�N<$?[<#��<$C�<&!�<,�u<$��<$y�<$A�<$�<$B�<$c�<$#(<%�j<$�7<$�<#�	<$k<#�<#��<$+<&$h<,K�<,nt<$�!<#�<&�8<$��<%rN<$��<%�l<$�	<$�<%�!<,�u<+�<'*�<#��<#�<#�N<)�L<&/<$��<$�q<)�5<6��<&ke<%�@<'��<$y�<$/%<$o�<$��<$<<%�<$��<(j<7�<(�<$�<$H�<$}<$��<$%<$j|<$�.<%p<, <$��<#ޫ<#�*<#��<'��<'��<%�j<*�<$��<#��<&)�<)�6<#�<#�e<#�<$'<%��<(�<$6�<%��<)�<$�j<#��<#�4<$�<#��<#�<$�<$�J<)�g<'��<$�j<$ʾ<$	<#��<$�<$]h<%�M<%m�<$_�<#�H<)#=<)c�<#��<#�a<$
�<$(<$W<#�g<#��<$3U<$I�<$?[<$#(<$4e<$6�<$r�<$�3<$o�<$O�<#��<$H�<'�T<$��<$�X<$W<$�<$
<$�<$Gd<#��<#�<$<<$ K<$��<$�<$I�<#�8<#ף<#�]<#��<$.<$\"<#�U<$�<$.<#�N<#�J<#�*<#�N<#�W<$�<#�8<$(<$�<#�<#��<#�a<#�<% <%^�<$*<#�c<$B�<#�M<$k<$��<$p<#�<$�<#ߜ<$�J<$L<$
�<#�&<#�I<#��<#�$<#��<#ף<#��<$v<$4e<#�"<#�o<#�o<$�<#�<#ף<#ߜ<#�r<$�<#��<#�^<#�l<#ܯ<#�$<#��<#�X<#��<#�E<#ڑ<#��<#�<#ܯ<#��<#�&<#�<#�<#�{PRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment                                                                                                                                                                            SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;                                                                                                               TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                   PSAL_ADJUSTED_ERR set to magnitude of thermal mass adjustment      PSAL_ADJ_ERR: max(0.01, CTM + resolution error)                                                                                                                                              201805250000002018052500000020180525000000  AO  ARGQQCPL                                                                    20160805180057  QCP$                G�O�G�O�G�O�5F006           AO  ARGQQCPL                                                                    20160805180057  QCF$                G�O�G�O�G�O�0               WHOIARSQWHQCV0.5                                                                20180525000000  QC                  G�O�G�O�G�O�                WHOIARSQ CTMV1.0                                                                20180525000000  IP                  G�O�G�O�G�O�                WHOIARSQCLIMN/A CSIRO,ArgoCARS2016                                              20180525000000  IP                  G�O�G�O�G�O�                WHOIARDUV_31V1.0                                                                20191118095932  UP                  G�O�G�O�G�O�                