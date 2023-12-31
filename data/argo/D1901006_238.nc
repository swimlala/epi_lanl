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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160825180048  20191118095938  1901006 US ARGO PROJECT                                                 BRECK OWENS                                                     PRES            TEMP            PSAL               �A   AO  3381                            2C  D   SOLO_W                          946                             1.20                            851 @���X�1   @���""" �C�+h{@D�����1   IRIDIUM Primary sampling: averaged [data averaged with equal weights into irregular pressure bins                                                                                                                                                                          A   A   A   @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D0@ D1� D2� D4  D5@ D6� D7� D9  D:@ D;� D<� D>  D?@ D@� DA� DC  DD@ DE� DF� DH  DI@ DJ� DK� DM  DN@ DO� DP� DR  DS@ DT� DU� DW  DX@ DY� DZ� D\  D]@ D^� D_� Da  Db@ Dc� Dd� Df  Dg@ Dh� Di� Dk  Dl@ Dm� Dn� Dp  Dq@ Dr� Ds� Du  Dv@ Dw� Dx� Dz  D{@ D|� D}� D  D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�@ D�� D�� D�  D�@ D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D0@ D1� D2� D4  D5@ D6� D7� D9  D:@ D;� D<� D>  D?@ D@� DA� DC  DD@ DE� DF� DH  DI@ DJ� DK� DM  DN@ DO� DP� DR  DS@ DT� DU� DW  DX@ DY� DZ� D\  D]@ D^� D_� Da  Db@ Dc� Dd� Df  Dg@ Dh� Di� Dk  Dl@ Dm� Dn� Dp  Dq@ Dr� Ds� Du  Dv@ Dw� Dx� Dz  D{@ D|� D}� D  D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�@ D�� D�� D�  D�@ D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A:��A:��A:��A:��A:��A:��A:��A:��A:��A:��A:��A:�!A:�RA:�RA:�RA:ĜA:�jA:��A:ĜA:ĜA:ĜA:��A:��A:ĜA:ĜA:ĜA:ĜA:ĜA:ĜA:��A:��A:��A:��A:��A:ĜA:ĜA:��A:��A:��A:ȴA:ȴA:��A:��A:��A:��A:��A:�/A:�HA;�A;�wA<JA;�#A;l�A;+A;oA;\)A<A<�A<ĜA<I�A;�-A;K�A:bA8�+A7x�A6�RA4��A3�#A37LA2E�A2A1�A0��A0jA133A1�A1"�A0z�A05?A/�A-��A-t�A-O�A-"�A-�-A.-A.I�A.1'A-%A*�A*VA*1'A*  A)�A)\)A(ȴA(��A'�FA'O�A'?}A'�A'�A'%A%dZA%A#�7A!G�A!+A!�
A"�\A$JA$�\A$jA$�jA$VA �Az�A��A`BA�yA��A�A��A;dA�A(�A{A��A�-AhsA��A�A�/A�-A�\At�A��A�Ax�A�wA��A��A��AO�A��A�mA�-A�7Ap�AS�A"�A�uA=qA{A  A��A`BA�A��A�DAM�A5?A�A��A��A�A�A�7A��A�AƨA�FA��A`BA�A%AoA��A��A��A
=AVA%A��A��A��AbNA�A�FAdZAC�A�AoA
=A
�A
�A
�A	?}A�yA��A-A��AK�A&�AAĜA�\AJA�
A`BA��Ar�A^5A=qA��Al�A�AE�A��A ��A 1'@�dZ@�x�@�ff@���@���@���@�%@��w@�ȴ@�hs@�V@�A�@��y@��@��#@��@�G�@���@�z�@��@�@홚@�Q�@��@�hs@�hs@�`B@�hs@�h@���@� �@�\)@��@�\)@⟾@���@ߕ�@��H@ޗ�@��@�@�;d@�dZ@���@�1'@��
@�@��@��H@�v�@ݩ�@�%@܋D@܃@�
=@�~�@٩�@�O�@�1'@�n�@ԓu@�ȴ@Ѳ-@�%@�V@ț�@�?}@�|�@+@��7@��D@�{@��/@�1@�"�@�V@���@�/@��@�(�@���@�C�@�
=@��!@�M�@���@���@��@�\)@�ȴ@��@�r�@���@��@��-@�j@�r�@�1'@�t�@�@���@��@�j@�  @���@��P@���@�(�@��;@�@��!@�E�@�G�@�Ĝ@�  @�O�@��H@��#@�/@�1@��@���@���@���@�V@���@���@�1@��P@�+@�@�^5@���@�S�@���@��P@�1'@�hs@�I�@��@�;d@�-@�X@���@�"�@��+@�V@�S�@���@���@�;d@��@�Z@��@���@�C�@�"�@�M�@���@�@�M�@��@�~�@���@�X@��@�Z@�  @���@�ȴ@�n�@�"�@��y@���@��H@�33@�@�@�J@��7@�?}@���@;d@~{@}�T@}p�@|I�@{33@{t�@|I�@z��@v5?@q��@p��@pQ�@n��@q�@so@q��@n�R@nE�@jM�@i%@hr�@e?}@b^5@bJ@a�7@`��@`�@`  @^ȴ@]`B@\�j@\�D@\(�@[t�@Y&�@X  @V��@V��@VE�@U`B@T��@T�D@T(�@SdZ@S@Rn�@PA�@N��@N$�@MO�@L�/@K��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A:��A:��A:��A:��A:��A:��A:��A:��A:��A:��A:��A:�!A:�RA:�RA:�RA:ĜA:�jA:��A:ĜA:ĜA:ĜA:��A:��A:ĜA:ĜA:ĜA:ĜA:ĜA:ĜA:��A:��A:��A:��A:��A:ĜA:ĜA:��A:��A:��A:ȴA:ȴA:��A:��A:��A:��A:��A:�/A:�HA;�A;�wA<JA;�#A;l�A;+A;oA;\)A<A<�A<ĜA<I�A;�-A;K�A:bA8�+A7x�A6�RA4��A3�#A37LA2E�A2A1�A0��A0jA133A1�A1"�A0z�A05?A/�A-��A-t�A-O�A-"�A-�-A.-A.I�A.1'A-%A*�A*VA*1'A*  A)�A)\)A(ȴA(��A'�FA'O�A'?}A'�A'�A'%A%dZA%A#�7A!G�A!+A!�
A"�\A$JA$�\A$jA$�jA$VA �Az�A��A`BA�yA��A�A��A;dA�A(�A{A��A�-AhsA��A�A�/A�-A�\At�A��A�Ax�A�wA��A��A��AO�A��A�mA�-A�7Ap�AS�A"�A�uA=qA{A  A��A`BA�A��A�DAM�A5?A�A��A��A�A�A�7A��A�AƨA�FA��A`BA�A%AoA��A��A��A
=AVA%A��A��A��AbNA�A�FAdZAC�A�AoA
=A
�A
�A
�A	?}A�yA��A-A��AK�A&�AAĜA�\AJA�
A`BA��Ar�A^5A=qA��Al�A�AE�A��A ��A 1'@�dZ@�x�@�ff@���@���@���@�%@��w@�ȴ@�hs@�V@�A�@��y@��@��#@��@�G�@���@�z�@��@�@홚@�Q�@��@�hs@�hs@�`B@�hs@�h@���@� �@�\)@��@�\)@⟾@���@ߕ�@��H@ޗ�@��@�@�;d@�dZ@���@�1'@��
@�@��@��H@�v�@ݩ�@�%@܋D@܃@�
=@�~�@٩�@�O�@�1'@�n�@ԓu@�ȴ@Ѳ-@�%@�V@ț�@�?}@�|�@+@��7@��D@�{@��/@�1@�"�@�V@���@�/@��@�(�@���@�C�@�
=@��!@�M�@���@���@��@�\)@�ȴ@��@�r�@���@��@��-@�j@�r�@�1'@�t�@�@���@��@�j@�  @���@��P@���@�(�@��;@�@��!@�E�@�G�@�Ĝ@�  @�O�@��H@��#@�/@�1@��@���@���@���@�V@���@���@�1@��P@�+@�@�^5@���@�S�@���@��P@�1'@�hs@�I�@��@�;d@�-@�X@���@�"�@��+@�V@�S�@���@���@�;d@��@�Z@��@���@�C�@�"�@�M�@���@�@�M�@��@�~�@���@�X@��@�Z@�  @���@�ȴ@�n�@�"�@��y@���@��H@�33@�@�@�J@��7@�?}@���@;d@~{@}�T@}p�@|I�@{33@{t�@|I�@z��@v5?@q��@p��@pQ�@n��@q�@so@q��@n�R@nE�@jM�@i%@hr�@e?}@b^5@bJ@a�7@`��@`�@`  @^ȴ@]`B@\�j@\�D@\(�@[t�@Y&�@X  @V��@V��@VE�@U`B@T��@T�D@T(�@SdZ@S@Rn�@PA�@N��@N$�@MO�@L�/@K��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
��B
��B
��B
��B
��B
��B
��B
�'B
B
ɺB
��B
��B
��B
�fB
��BDBoBVB	7BB
�B
�HB
��B
ǮB
�9B
��B
��B
��B
�hB
�JB
�B
�B
��B
��B
��B
��B
�uB
�PB
�B
�B
�1B
�\B
��B
�?B
�jB
�dB
�-B
��B
��B
��B
�-B
�qB
ƨB
ǮB
ƨB
��B
��B
��B
��B
B
ĜB
�^B
�^B
�-B
��B
��B
�!B
�wB
��B
�)B
�#B
�;B
�B
�B
��B
��B
�uB
�bB
�PB
�DB
�7B
�+B
�B
�%B
�DB
�VB
�hB
�oB
��B
��B
�{B
�=B
~�B
u�B
t�B
x�B
� B
�%B
�=B
�DB
�B
~�B
|�B
u�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
t�B
t�B
s�B
q�B
q�B
p�B
p�B
q�B
s�B
s�B
s�B
t�B
x�B
z�B
|�B
� B
�B
�+B
�+B
�B
�B
�B
�+B
�+B
�+B
�7B
�DB
�VB
�\B
�\B
�bB
�\B
�\B
�PB
�JB
�=B
�7B
�1B
�1B
�+B
�%B
�B
� B
|�B
y�B
w�B
t�B
r�B
p�B
n�B
m�B
l�B
jB
hsB
ffB
dZB
`BB
_;B
^5B
\)B
YB
W
B
Q�B
M�B
H�B
B�B
<jB
8RB
33B
&�B
$�B
#�B
#�B
!�B
�B
�B
�B
{B
oB
bB
VB
PB
PB
DB

=B
1B
%B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�fB	�NB	�;B	�)B	�)B	�/B	�TB	�`B	�mB	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�HB	�B	��B	��B	ȴB	�qB	��B	��B	�\B	�7B	�B	}�B	w�B	ffB	dZB	aHB	_;B	^5B	]/B	\)B	[#B	ZB	YB	XB	W
B	VB	VB	VB	S�B	Q�B	O�B	M�B	G�B	A�B	8RB	1'B	-B	-B	+B	(�B	#�B	�B	�B	�B	�B	{B	�B	�B	�B	�B	�B	�B	�B	{B	uB	bB	DB	B	B	B	  B��B��B��B��B��B��B��B��B��B��B	  B	B	�B	 �B	%�B	(�B	/B	7LB	K�B	K�B	A�B	?}B	=qB	<jB	;dB	9XB	6FB	9XB	J�B	Q�B	K�B	K�B	K�B	K�B	N�B	S�B	YB	[#B	]/B	hsB	{�B	�B	�B	�B	�+B	�JB	�VB	�VB	�VB	�\B	�{B	��B	��B	��B	��B	�B	�'B	�9B	�RB	�dB	�jB	�qB	�qB	�}B	��B	B	ÖB	ǮB	��B	��B	��B	ĜB	�}B	ƨB	��B	��B	��B	�)B	�/B	�#B	�/B	�#B	�B	�#B	�B	�B	�#B	�)B	�5B	�BB	�HB	�TB	�mB	�sB	�yB	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
+B
	7B
DB
bB
�B
�B
 �B
"�B
%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
�tB
�}B
��B
��B
��B
��B
�}B
�tB
�vB
��B
��B
�mB
��B
��B
�B
��B
��B
��B
��B
�B
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
�zB
��B
�oB
�}B
��B
��B
��B
�}B
�rB
��B
��B
��B
�wB
�|B
��B
�B
�VB
��B
��B
͆B
�B
�2B
�B
�rB
�B�BB
zBvB
��B
�]B
�NB
��B
�B
��B
�LB
�oB
��B
��B
��B
�7B
��B
��B
�{B
��B
��B
��B
�SB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�VB
��B
�xB
�B
�5B
�7B
�6B
��B
��B
��B
��B
�B
��B
��B
��B
�DB
�=B
��B
��B
�xB
�SB
�eB
�cB
��B
�>B
�YB
��B
��B
��B
�PB
��B
�0B
��B
�6B
�B
�nB
��B
� B
�+B
��B
��B
�wB
��B
��B
�)B
w!B
toB
w�B
BB
��B
�$B
�/B
�!B
�B
~�B
v{B
u8B
u	B
uB
u\B
vVB
v�B
v=B
vB
ufB
u�B
t�B
rGB
rJB
qZB
p�B
r�B
tB
t'B
tB
t�B
x�B
z�B
|�B
�B
�HB
�rB
��B
��B
�KB
�B
�]B
�;B
�&B
�B
�AB
�lB
��B
��B
��B
�0B
�9B
�vB
�9B
��B
��B
�TB
�NB
�|B
��B
��B
�~B
}�B
z�B
yB
vJB
s�B
qB
oB
nLB
m;B
k�B
i%B
g�B
f�B
`�B
_B
^�B
]�B
Y�B
X�B
S�B
O�B
J�B
D�B
>B
;LB
7�B
'�B
%4B
$B
$�B
#�B
 :B
�B
2B
�B
mB
�B
�B
�B
�B
�B

�B
	B
�B
4B
B	��B	�TB	��B	��B	��B	��B	��B	��B	�B	��B	�RB	�B	�tB	�B	�DB	ܞB	��B	�B	�B	�1B	��B	�"B	�AB	��B	��B	��B	�\B	��B	�B	�KB	��B	�B	�pB	��B	�$B	�7B	�B	�B	��B	ԨB	�B	��B	��B	�B	�]B	��B	��B	��B	��B	QB	h(B	e�B	b�B	`[B	^�B	^B	\�B	[�B	Z�B	Y�B	X�B	W�B	W*B	W�B	W B	T�B	R�B	QDB	P(B	J�B	EuB	<B	3B	-/B	-�B	,:B	+:B	&-B	$gB	rB	AB	�B	�B	zB	�B	B	�B	2B	RB	B	aB	�B	~B	B	�B	3B	�B	uB�|B�B�?B��B��B��B��B��B�(B�B	 rB��B	�B	 �B	%�B	(�B	.gB	5�B	L�B	N$B	BFB	@B	>XB	=B	;�B	:OB	7pB	8YB	JYB	SdB	L�B	L�B	K�B	K�B	OVB	T B	Y�B	[B	\�B	foB	{nB	�fB	��B	�rB	�[B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�*B	��B	�BB	��B	��B	��B	�B	��B	��B	��B	��B	��B	ǨB	̏B	�WB	�:B	�B	��B	��B	�5B	�.B	�^B	ܔB	�&B	�UB	�sB	ۜB	�WB	�+B	�B	�?B	�TB	�_B	�cB	�pB	�B	��B	�B	�B	�B	��B	�\B	�B	�B	��B	��B	�2B	�(B
  B
3B
`B
SB
	lB
�B
�B
�B
�B
 �B
#B
%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#׺<#׎<#׎<#�$<#�X<#�<#�<#�<#׎<#�X<#�$<#��<#�<#�<#�<#�D<#�&<#�<<#�<#�<#׺<#�<#�<<#�&<#�<#�<#�&<#�<#׺<#�<#�<#�<#�<#�0<#�<#�<#�X<#׺<#�i<#�<<#�X<#�c<#׺<#�<#�<#��<#��<$2G<%��<$\"<#��<$�7<$?[<#�<$Gd<&1�<%�d<$6�<%F<&e<%:<,��<1Ey<+�<(!�<7�<)�<&�9<(ܠ<$��<%��<(�\<$v<&��<$�e<$�k<&?><$� <+��<-%b<%S<$�<#��<%��<%4L<#�	<#�<,S<;ׄ<&�<$�<$*<#�4<%��<%�L<$G<(ܠ<$�<#��<#��<#�
<$	<2=�<%�d<0�a<>��<#��<&�?<'x�</ߩ<%��<#�<$Gd<%�L<\	3<H��<'��<%@�<%>�<$��<%rN<$��<&'<'�<$��<#�M<#�<$Sa<$J�<$ �<#�<&��<,.<,F�<+�X<%K:<#�!<$��<$E<#�<&��<*\�<'F<'1;<&�n<$><<$�<#�!<#��<$%<%��<$�w<$p<#�<$/%<$�L<%s<$"2<$%<$<<#�N<$c�<#��<#��<#��<#ף<#ף<#ܯ<#��<#��<#��<#�l<$7�<$f�<#ޫ<#ا<#ޫ<#��<#�<#�*<#�<#؄<#�J<#�a<$	�<$_�<$k�<$��<$� <#��<$<<#��<#ٛ<#�<$c�<&y<(�_<$��<$�V<%(<%��<$�3<$�<$<<$A�<$5w<%��<$7�<%it<'r#<$<#�&<$<<&e<$Sa<&�<&4p<&q<'d<()+<%��<*�<1�j<$n�<#�"<#��<$�B<&��<%�M<&�a<$*<%:<&�n<$�-<$ <#��<$�<$�<$A�<$n�<%rN<'[)<'<.�<$�<#�I<#�c<#�X<#ٛ<$��<$��<%s<'��<.Z)<%*<$��<-%b<$ʾ<$ �<#�U<#�<#�<#�<$'<$�<$v<$�-<#�l<#��<$1:<$��<$�w<$@|<#�<'5<$��<%S<$$<&!�<);-<)�<)w�<&y<%s<1�<T,�<6��<)��<%͍<%�6<&
(</��<K(�<&<�<%^�<% <$�h<$H�<$^�<$b�<$aD<$(<#�Q<$}<$-<$��<%��<$�w<$�b<$�Q<%Z2<(�<*(}</r	<.>�<&�3<#�]<#��<$�<'�c<(�<46<&y�<$@|<#��<#��<#�0<$A�<$r<%:{<$}<$F9<%��<$x+<%U�<0�i<.��<&�<$��<&h�<%}�<$r<#�C<#�<$&<'*<$4e<$�B<$U�<#�5<#�l<#��<'*�<$��<#�]<#׺<#�<$9�<%��<$F9<(%�<$C�<$�<$y�<$�<#��<$��<$�.<$�<#�	<%�J<$�t<$O�<#��<#�8<$�<#��<$f<#��<$)
<&��<$�<#�!<$�<#�5<#�<$	<#�U<#�<$�<#�l<#��<#�8<#��<#�
<#�r<#ڑ<$-<#�I<#��<#��<#��<$,<#��<#�<#�l<#�)<#�W<#�&<#��<$v<%k�<%�V<#�m<#�<$ <$�<$Z<#��<$��<#ޫ<%0<$�<#�N<$�3<$��<#ڑ<#�^<#��<#�<#�<#�H<$�<#�N<#ا<#�+<#��<$L<#�W<#��<#ڑ<#�r<#�4<#�J<#�*<#ۮ<#�l<#��<#ߜ<#��<#�e<#�+<#ۮ<#��<#�<#ףPRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment                                                                                                                                                                            SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;                                                                                                               TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                   PSAL_ADJUSTED_ERR set to magnitude of thermal mass adjustment      PSAL_ADJ_ERR: max(0.01, CTM + resolution error)                                                                                                                                              201805250000002018052500000020180525000000  AO  ARGQQCPL                                                                    20160825180048  QCP$                G�O�G�O�G�O�5F006           AO  ARGQQCPL                                                                    20160825180048  QCF$                G�O�G�O�G�O�0               WHOIARSQWHQCV0.5                                                                20180525000000  QC                  G�O�G�O�G�O�                WHOIARSQ CTMV1.0                                                                20180525000000  IP                  G�O�G�O�G�O�                WHOIARSQCLIMN/A CSIRO,ArgoCARS2016                                              20180525000000  IP                  G�O�G�O�G�O�                WHOIARDUV_31V1.0                                                                20191118095938  UP                  G�O�G�O�G�O�                