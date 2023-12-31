CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:30Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140830  20181024140830  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��dS�
�1   @��d���@4��E���c�ȴ9X1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@y��@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bg��Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C �C"  C#�fC%�fC(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~�C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  D   D � DfD� D��D� D  D� D  D�fD  D�fD  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0fD0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;fD;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB�fDC  DCy�DC��DDy�DD��DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DMfDM�fDN  DN� DO  DO� DPfDP�fDQfDQ� DR  DR� DSfDS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DYy�DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Dn��Doy�Do��Dp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dt� Du  Duy�Dv  Dv� Dw  Dws3Dyi�D�K�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @L(�@�{@�G�A��A$��AD��Ad��A�Q�A�Q�A��A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	(�B(�B(�B!(�B)(�B1(�B9(�BA(�BI(�BQ(�BY�\Ba(�BhBq(�By(�B��{B��{B�aHB��{B��{B��{B��{B��{B�ǮB��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�ǮB�{B�aHB�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
0�CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C c�C"J=C$0�C&0�C(J=C*J=C,c�C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=Cb0�CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=Cxc�CzJ=C|J=C~c�C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�RC�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�1�C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�1�C�%D �D ��D�D��D)D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D)D�)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$)D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC�)DD)DD�)DE)DE�)DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ�)DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW)DW��DX�DX��DY�DY�)DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do)Do�)Dp)Dp��Dq�Dq��Dr)Dr��Ds�Ds��Dt�Dt��Du�Du�)Dv�Dv��Dw�Dw��Dy|)D�T�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A�  A�  A�  A�A�A�A�%A�1A�1A�%A�%A�%A�1A�
=A�
=A�JA�JA�JA�1A���A���A���A��;A�XAء�AҍPAϛ�A�`BA�C�A� �Aǲ-AőhA��A��RA��PA��7A���A��A�bNA�bA�p�A���A�7LA�G�A�A�A���A��-A�ffA�%A�
=A�/A�r�A��A�5?A���A�ffA�r�A��A��wA�I�A��^A��A��A���A���A�
=A�JA�~�A�1A�^5A�7LA���A�-A�=qA��!A��A���A�E�A��^A� �A���A�C�A�I�A� �A~~�A}��A}\)A{"�Ay��Ax�`Awx�AvE�At$�ApbNAoC�Al�\Ai�hAh��Ah-Ag/AeVAb�jA_O�A\$�AY�-AWhsAV��AV{AT�9AR��ARI�AQ`BAPbAM+ALAJ�AI�
AGAE|�AC�AC%AA�A@ �A?"�A>�RA=��A;ƨA: �A7�mA5�7A4^5A3`BA1��A1hsA133A0�uA/33A.�A.A-"�A+S�A*ĜA*�9A*��A*��A*��A*~�A*{A)��A(��A(1A';dA&�+A&  A#�A �\A�RA�7AbA�A�AQ�A�HA��AQ�A�A��A��AĜAbNA�A�Al�AbAS�A7LA�A
��A
 �A	�-A	;dA�yA��A�RA9XA�PAhsAO�A/A�A�;A|�A?}A�HAjAA��A9XA��AXA33A �HA r�@�l�@�X@�$�@�t�@��j@�b@��
@�K�@��@���@�V@�ȴ@���@�1@�ȴ@��T@�9@��;@�O�@�{@�9@�@��@ܓu@ܓu@ܬ@ܬ@�bN@�ƨ@�{@�(�@��;@�l�@�{@�@��@�o@�V@���@ˮ@�|�@�"�@�J@��`@ț�@�Z@��@ǅ@��y@��@őh@�&�@ă@�j@���@��@�x�@�Z@�  @�t�@�\)@���@��@�=q@��@�Ĝ@�(�@�dZ@�o@�5?@�hs@��@���@�j@�b@��m@�33@��y@�ȴ@��R@��!@��!@�ȴ@��@���@��\@�V@��@��@���@�Z@� �@��@��P@��^@�V@�V@�%@���@��j@��@��D@��u@�bN@��@�ƨ@�dZ@�"�@�
=@�o@�"�@�
=@�5?@��@��#@���@���@�p�@�G�@���@��j@�r�@� �@��F@�|�@�C�@��@��@���@�Ĝ@�Z@�I�@�A�@��m@�K�@��@���@�=q@���@��@�%@��@��u@�j@�bN@�I�@��@���@�+@��R@��\@��@�X@���@���@��D@�I�@��m@��w@��@�K�@��@��y@�ȴ@��R@��9@�Z@��y@�M�@���@�`B@�/@��@���@�%@��u@�(�@�Q�@�hs@�$�@���@�@�C�@��P@���@���@�C�@�33@�~�@�-@��@�M�@�J@�?}@���@��@��D@���@�bN@�  @��@�A�@���@���@���@��D@�(�@��w@��F@�  @��w@�t�@�"�@���@�@�+@���@���@�v�@�^5@�E�@�-@�{@��@��@��T@��^@���@�hs@�7L@���@��j@�j@�(�@��m@�ƨ@��P@�l�@�C�@�
=@��@���@���@���@��\@�$�@��T@��^@���@���@���@��h@��@�`B@�?}@�/@�&�@��@���@�(�@� �@��@�b@�1@��
@���@�K�@�C�@�C�@�@���@��\@�v�@�=q@��@�J@���@�/@��@��j@��D@�bN@�A�@�b@���@�33@���@���@��\@�x�@y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A�  A�  A�  A�A�A�A�%A�1A�1A�%A�%A�%A�1A�
=A�
=A�JA�JA�JA�1A���A���A���A��;A�XAء�AҍPAϛ�A�`BA�C�A� �Aǲ-AőhA��A��RA��PA��7A���A��A�bNA�bA�p�A���A�7LA�G�A�A�A���A��-A�ffA�%A�
=A�/A�r�A��A�5?A���A�ffA�r�A��A��wA�I�A��^A��A��A���A���A�
=A�JA�~�A�1A�^5A�7LA���A�-A�=qA��!A��A���A�E�A��^A� �A���A�C�A�I�A� �A~~�A}��A}\)A{"�Ay��Ax�`Awx�AvE�At$�ApbNAoC�Al�\Ai�hAh��Ah-Ag/AeVAb�jA_O�A\$�AY�-AWhsAV��AV{AT�9AR��ARI�AQ`BAPbAM+ALAJ�AI�
AGAE|�AC�AC%AA�A@ �A?"�A>�RA=��A;ƨA: �A7�mA5�7A4^5A3`BA1��A1hsA133A0�uA/33A.�A.A-"�A+S�A*ĜA*�9A*��A*��A*��A*~�A*{A)��A(��A(1A';dA&�+A&  A#�A �\A�RA�7AbA�A�AQ�A�HA��AQ�A�A��A��AĜAbNA�A�Al�AbAS�A7LA�A
��A
 �A	�-A	;dA�yA��A�RA9XA�PAhsAO�A/A�A�;A|�A?}A�HAjAA��A9XA��AXA33A �HA r�@�l�@�X@�$�@�t�@��j@�b@��
@�K�@��@���@�V@�ȴ@���@�1@�ȴ@��T@�9@��;@�O�@�{@�9@�@��@ܓu@ܓu@ܬ@ܬ@�bN@�ƨ@�{@�(�@��;@�l�@�{@�@��@�o@�V@���@ˮ@�|�@�"�@�J@��`@ț�@�Z@��@ǅ@��y@��@őh@�&�@ă@�j@���@��@�x�@�Z@�  @�t�@�\)@���@��@�=q@��@�Ĝ@�(�@�dZ@�o@�5?@�hs@��@���@�j@�b@��m@�33@��y@�ȴ@��R@��!@��!@�ȴ@��@���@��\@�V@��@��@���@�Z@� �@��@��P@��^@�V@�V@�%@���@��j@��@��D@��u@�bN@��@�ƨ@�dZ@�"�@�
=@�o@�"�@�
=@�5?@��@��#@���@���@�p�@�G�@���@��j@�r�@� �@��F@�|�@�C�@��@��@���@�Ĝ@�Z@�I�@�A�@��m@�K�@��@���@�=q@���@��@�%@��@��u@�j@�bN@�I�@��@���@�+@��R@��\@��@�X@���@���@��D@�I�@��m@��w@��@�K�@��@��y@�ȴ@��R@��9@�Z@��y@�M�@���@�`B@�/@��@���@�%@��u@�(�@�Q�@�hs@�$�@���@�@�C�@��P@���@���@�C�@�33@�~�@�-@��@�M�@�J@�?}@���@��@��D@���@�bN@�  @��@�A�@���@���@���@��D@�(�@��w@��F@�  @��w@�t�@�"�@���@�@�+@���@���@�v�@�^5@�E�@�-@�{@��@��@��T@��^@���@�hs@�7L@���@��j@�j@�(�@��m@�ƨ@��P@�l�@�C�@�
=@��@���@���@���@��\@�$�@��T@��^@���@���@���@��h@��@�`B@�?}@�/@�&�@��@���@�(�@� �@��@�b@�1@��
@���@�K�@�C�@�C�@�@���@��\@�v�@�=q@��@�J@���@�/@��@��j@��D@�bN@�A�@�b@���@�33@���@���@��\@�x�@y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�oB�oB�hB�hB�\B��B��B�sB�B�B+B�B$�B-B;dBG�BJ�BP�BW
BYB[#B]/B[#B_;B]/BYBW
BT�BP�BF�B7LB'�B!�B�B�B	7B��B�`B�B��BÖB�3B��B�bBs�Be`B\)BD�B<jB6FB+B+B
�sB
��B
ƨB
�wB
��B
�oB
|�B
`BB
J�B
<jB
2-B
%�B
�B
JB
1B
B	��B	�B	�sB	�NB	�/B	��B	ȴB	��B	�FB	��B	��B	��B	��B	�\B	�B	r�B	bNB	R�B	E�B	>wB	8RB	.B	!�B	�B	�B	\B��B�B�B�B�BB�B��B�B�
B��B��B��B��B��BȴBƨBB�wB�qB�^B�RB�LB�FB�3B�-B�!B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�PB�7B�B� B� B}�B� B� B~�B~�B~�B}�B|�B{�B}�B}�B}�B}�B~�B�B�B�B�B�+B�1B�=B�DB�=B�=B�1B�+B�1B�DB�\B��B��B�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�9B�RB�jB�jB�jB�qBÖB��B��B��B��B��B��B��B�B�#B�#B�)B�)B�)B�/B�5B�;B�BB�NB�NB�ZB�fB�yB�B�B�B�B�B�B��B	B	B	%B	JB	\B	{B	�B	�B	�B	�B	!�B	"�B	(�B	)�B	+B	-B	.B	/B	0!B	33B	:^B	;dB	;dB	;dB	@�B	F�B	H�B	I�B	J�B	K�B	N�B	P�B	S�B	T�B	T�B	VB	W
B	W
B	XB	XB	YB	\)B	^5B	`BB	bNB	bNB	cTB	gmB	m�B	n�B	o�B	p�B	q�B	s�B	w�B	|�B	~�B	�B	�1B	�DB	�DB	�JB	�JB	�\B	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�'B	�-B	�-B	�-B	�-B	�!B	��B	��B	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�FB	�XB	�^B	�qB	�wB	�qB	�wB	�wB	�wB	��B	B	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�5B	�BB	�BB	�HB	�HB	�NB	�ZB	�TB	�NB	�TB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
B
%B
+B
+B
1B
1B

=B
DB
DB
DB
JB
"B
)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�oB�oB�hB�hB�\B��B��B�sB�B�B+B�B$�B-B;dBG�BJ�BP�BW
BYB[#B]/B[#B_;B]/BYBW
BT�BP�BF�B7LB'�B!�B�B�B	7B��B�`B�B��BÖB�3B��B�bBs�Be`B\)BD�B<jB6FB+B+B
�sB
��B
ƨB
�wB
��B
�oB
|�B
`BB
J�B
<jB
2-B
%�B
�B
JB
1B
B	��B	�B	�sB	�NB	�/B	��B	ȴB	��B	�FB	��B	��B	��B	��B	�\B	�B	r�B	bNB	R�B	E�B	>wB	8RB	.B	!�B	�B	�B	\B��B�B�B�B�BB�B��B�B�
B��B��B��B��B��BȴBƨBB�wB�qB�^B�RB�LB�FB�3B�-B�!B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�PB�7B�B� B� B}�B� B� B~�B~�B~�B}�B|�B{�B}�B}�B}�B}�B~�B�B�B�B�B�+B�1B�=B�DB�=B�=B�1B�+B�1B�DB�\B��B��B�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�9B�RB�jB�jB�jB�qBÖB��B��B��B��B��B��B��B�B�#B�#B�)B�)B�)B�/B�5B�;B�BB�NB�NB�ZB�fB�yB�B�B�B�B�B�B��B	B	B	%B	JB	\B	{B	�B	�B	�B	�B	!�B	"�B	(�B	)�B	+B	-B	.B	/B	0!B	33B	:^B	;dB	;dB	;dB	@�B	F�B	H�B	I�B	J�B	K�B	N�B	P�B	S�B	T�B	T�B	VB	W
B	W
B	XB	XB	YB	\)B	^5B	`BB	bNB	bNB	cTB	gmB	m�B	n�B	o�B	p�B	q�B	s�B	w�B	|�B	~�B	�B	�1B	�DB	�DB	�JB	�JB	�\B	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�'B	�-B	�-B	�-B	�-B	�!B	��B	��B	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�FB	�XB	�^B	�qB	�wB	�qB	�wB	�wB	�wB	��B	B	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�5B	�BB	�BB	�HB	�HB	�NB	�ZB	�TB	�NB	�TB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
B
%B
+B
+B
1B
1B

=B
DB
DB
DB
JB
"B
)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140830                              AO  ARCAADJP                                                                    20181024140830    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140830  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140830  QCF$                G�O�G�O�G�O�0               