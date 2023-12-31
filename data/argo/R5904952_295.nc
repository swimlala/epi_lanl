CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:13Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190613  20181005190613  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              'A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�)�Ib1   @�*DDY0@1�
=p��cdZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     'A   A   A   @�  @�  A   A   A@  A`  A���A���A�  A�33A�  A�  A�  A�  B   B  B  B��B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C��D   D � D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D��D� DfD� D��D� D  D� D  D� D  D� D  D� D  D� DfD� D  Dy�D��D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D � D!fD!� D"  D"� D#  D#� D$fD$� D%  D%y�D%��D&� D'  D'� D(  D(y�D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/y�D0  D0�fD1fD1� D2  D2� D3  D3y�D4  D4� D5  D5� D6  D6�fD7  D7� D8  D8� D9  D9� D:  D:y�D:��D;y�D;��D<� D=  D=� D>  D>�fD?  D?�fD@  D@� DA  DA� DBfDB�fDB��DCy�DD  DD� DE  DEy�DF  DF� DGfDG�fDH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DPy�DQ  DQ�fDRfDR� DS  DS� DT  DT� DU  DU� DU��DV� DWfDW�fDXfDX� DX��DY� DZ  DZ�fD[fD[�fD\  D\y�D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� DbfDb�fDcfDc� Dd  Dd�fDefDe�fDffDf�fDg  Dgy�Dh  Dh�fDi  Diy�Dj  Dj� DkfDk� Dl  Dly�Dm  Dm�fDn  Dn� DofDo� Dp  Dp� Dp��Dqy�Dq��Dry�Dr��Dsy�Dt  Dt� Du  Du� Dv  Dv�fDwfDw�fDw�3Dy� D�0�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�G�A��A$��AD��Ad��A��A��A�Q�A��A�Q�A�Q�A�Q�A�Q�B(�B	(�B(�BB B)(�B1(�B9(�BA(�BI(�BQ(�BY(�Ba(�Bi(�BpBy(�B��{B��{B��{B��{B�aHB��{B��{B��{B��{B��{B��{B�aHB�aHB��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{B�aHBܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$c�C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<0�C>J=C@J=CBJ=CDJ=CFc�CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CT0�CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=Cvc�CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�RC�%C�%C�RC�%C�%C�%C�%C�RC�RC�%C�%C�%C�%C�%C�%C�1�C�%C�RC�%C�1�C�%C�RC�RC�RC�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�1�C�1�C�1�C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�RC�%C�%C�%C�RC�%C�%C�%C�1�C�%C�%C�%C�1�D �D ��D�D��D�D�)D)D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D)D��D�D��D)D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�)D)D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D)D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%�)D&)D&��D'�D'��D(�D(�)D)�D)��D*�D*��D+�D+�)D,�D,��D-�D-��D.�D.��D/�D/�)D0�D0��D1�D1��D2�D2��D3�D3�)D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:�)D;)D;�)D<)D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC)DC�)DD�DD��DE�DE�)DF�DF��DG�DG��DH�DH�)DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP�)DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV)DV��DW�DW��DX�DX��DY)DY��DZ�DZ��D[�D[��D\�D\�)D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg�)Dh�Dh��Di�Di�)Dj�Dj��Dk�Dk��Dl�Dl�)Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw��Dy��D�:>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�VA�VA�XA�XA�ZA�ZA�ZA�\)A�\)A�\)A�^5A�^5A�^5A�\)A�ZA�\)A�\)A�\)A�\)A�\)A�ZA�XA�ZA�S�A�Q�A�O�A�K�A���A�~�A�%A��;AɍPA��A�l�A�&�A���Aư!A��A�oA�ZA��A�|�A�{A��A���A�|�A�+A��
A�\)A�A��A�p�A�`BA�G�A�ȴA�ffA�-A�oA���A��PA��A�Q�A�l�A���A��jA���A��wA�9XA��wA�t�A���A�1'A���A�9XA�1'A�z�A��A�A���A���A���A�^5A���A��HA���A��FA�C�A�bA���A��A�r�A�\)A�A�ZA���A�v�A�  A��-A�
=A��uA��A�jA���A�XA�S�A�33A�I�A��FA��A�hsA`BA|��At�+Aq+AoK�Al�AgdZAbZA^��A_x�A\�AX�AQ��AO�wAM?}AJ�/AJ$�AIt�AI�AHM�AF��AE��AD�AC;dAA|�A??}A=p�A;O�A9��A8�A6�/A4ȴA37LA1;dA/VA,�A+�mA*�HA)/A'��A&�yA$�uA#�-A"�A!��A ��A=qAn�A�9A�A�A=qA�RA�FA�yA-AXA�HA�9A �AK�AVA�\A�7A��A�+AA�A5?A�AbA�Ax�A33A
�\A	hsA	�A��A$�A��A33A9XAp�A�\A�^AS�AC�A�RA �A�A�Ap�AS�A;dA+A"�A �`A �@�@���@��w@���@��9@�-@�9@�^5@�n�@���@�`B@��@�P@��@@�S�@��@���@��@�$�@���@�&�@�j@�  @�R@��@�^@���@�O�@���@�bN@�w@�@�w@�@��H@��@��@���@�h@�p�@�X@�%@�Ĝ@㝲@�{@��@��@���@�G�@���@�I�@߾w@�dZ@�;d@�^5@��@�v�@ݙ�@�x�@�&�@ܛ�@��@۶F@�t�@ڏ\@�J@�?}@�9X@���@�K�@�"�@���@���@�%@���@ԣ�@�Z@��
@Ӯ@�\)@�ff@�O�@д9@�I�@��m@�|�@�33@��@�~�@��@�G�@�V@���@̓u@�l�@ʸR@��@ɡ�@���@Ȭ@��;@�K�@�
=@�
=@Ƨ�@Ɵ�@�^5@�J@���@ź^@�hs@�Ĝ@���@Ý�@ÍP@Å@�dZ@�
=@¸R@�v�@�E�@���@�G�@��`@���@���@��D@�Q�@�1@��m@���@�K�@�+@���@�ff@�J@���@��^@�hs@��j@�Z@�A�@���@�l�@���@�5?@�@���@���@��
@��@�~�@�V@�M�@��^@��@��/@��9@�z�@�1'@��F@��@�33@�"�@�dZ@�l�@�
=@��@��h@��@�x�@��7@���@�7L@�b@�;d@�~�@�@�?}@���@��@��@���@��F@���@�t�@��y@�v�@�$�@�@�x�@�G�@��`@�I�@��@��
@��w@�|�@�?}@�Z@�A�@�A�@�9X@�Z@���@�Q�@��@���@�t�@�|�@��m@��@�\)@��\@�ff@�E�@�J@��#@�hs@�9X@�ȴ@���@�hs@���@��9@�j@�9X@�9X@�9X@�A�@�A�@�1'@�A�@��`@�V@� �@�A�@�V@�V@���@�Q�@��
@��@�v�@�v�@�^5@�E�@��@��@���@�/@��/@�I�@��F@�9X@�?}@�7L@��@��9@�A�@�(�@�1@��F@�t�@�+@��@�ȴ@��\@�$�@���@�O�@�?}@�7L@���@�Z@��F@�"�@���@�M�@���@��#@��^@�`B@�?}@�&�@�V@��o@�c11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�VA�VA�VA�XA�XA�ZA�ZA�ZA�\)A�\)A�\)A�^5A�^5A�^5A�\)A�ZA�\)A�\)A�\)A�\)A�\)A�ZA�XA�ZA�S�A�Q�A�O�A�K�A���A�~�A�%A��;AɍPA��A�l�A�&�A���Aư!A��A�oA�ZA��A�|�A�{A��A���A�|�A�+A��
A�\)A�A��A�p�A�`BA�G�A�ȴA�ffA�-A�oA���A��PA��A�Q�A�l�A���A��jA���A��wA�9XA��wA�t�A���A�1'A���A�9XA�1'A�z�A��A�A���A���A���A�^5A���A��HA���A��FA�C�A�bA���A��A�r�A�\)A�A�ZA���A�v�A�  A��-A�
=A��uA��A�jA���A�XA�S�A�33A�I�A��FA��A�hsA`BA|��At�+Aq+AoK�Al�AgdZAbZA^��A_x�A\�AX�AQ��AO�wAM?}AJ�/AJ$�AIt�AI�AHM�AF��AE��AD�AC;dAA|�A??}A=p�A;O�A9��A8�A6�/A4ȴA37LA1;dA/VA,�A+�mA*�HA)/A'��A&�yA$�uA#�-A"�A!��A ��A=qAn�A�9A�A�A=qA�RA�FA�yA-AXA�HA�9A �AK�AVA�\A�7A��A�+AA�A5?A�AbA�Ax�A33A
�\A	hsA	�A��A$�A��A33A9XAp�A�\A�^AS�AC�A�RA �A�A�Ap�AS�A;dA+A"�A �`A �@�@���@��w@���@��9@�-@�9@�^5@�n�@���@�`B@��@�P@��@@�S�@��@���@��@�$�@���@�&�@�j@�  @�R@��@�^@���@�O�@���@�bN@�w@�@�w@�@��H@��@��@���@�h@�p�@�X@�%@�Ĝ@㝲@�{@��@��@���@�G�@���@�I�@߾w@�dZ@�;d@�^5@��@�v�@ݙ�@�x�@�&�@ܛ�@��@۶F@�t�@ڏ\@�J@�?}@�9X@���@�K�@�"�@���@���@�%@���@ԣ�@�Z@��
@Ӯ@�\)@�ff@�O�@д9@�I�@��m@�|�@�33@��@�~�@��@�G�@�V@���@̓u@�l�@ʸR@��@ɡ�@���@Ȭ@��;@�K�@�
=@�
=@Ƨ�@Ɵ�@�^5@�J@���@ź^@�hs@�Ĝ@���@Ý�@ÍP@Å@�dZ@�
=@¸R@�v�@�E�@���@�G�@��`@���@���@��D@�Q�@�1@��m@���@�K�@�+@���@�ff@�J@���@��^@�hs@��j@�Z@�A�@���@�l�@���@�5?@�@���@���@��
@��@�~�@�V@�M�@��^@��@��/@��9@�z�@�1'@��F@��@�33@�"�@�dZ@�l�@�
=@��@��h@��@�x�@��7@���@�7L@�b@�;d@�~�@�@�?}@���@��@��@���@��F@���@�t�@��y@�v�@�$�@�@�x�@�G�@��`@�I�@��@��
@��w@�|�@�?}@�Z@�A�@�A�@�9X@�Z@���@�Q�@��@���@�t�@�|�@��m@��@�\)@��\@�ff@�E�@�J@��#@�hs@�9X@�ȴ@���@�hs@���@��9@�j@�9X@�9X@�9X@�A�@�A�@�1'@�A�@��`@�V@� �@�A�@�V@�V@���@�Q�@��
@��@�v�@�v�@�^5@�E�@��@��@���@�/@��/@�I�@��F@�9X@�?}@�7L@��@��9@�A�@�(�@�1@��F@�t�@�+@��@�ȴ@��\@�$�@���@�O�@�?}@�7L@���@�Z@��F@�"�@���@�M�@���@��#@��^@�`B@�?}@�&�@�V@��o@�c11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
7LB
�B
��B
�B
�?B
�dB
�B
�B
��B
�B1B)�B<jBC�BD�BG�BH�BL�BgmBs�BhsB�=B�DB��B��B�-B�^BɺB�
B�B�B�/B�5B�B��B
=BuB�B{B�B�B�B�B�B �B#�B �B�B�B�B�B{BPB+B��B�B�B�BB��B�}B�3B�B��B��B�B`BBW
BO�BJ�BD�B>wB(�BuB
�B
��B
�B
�=B
�+B
k�B
dZB
[#B
33B
�B
{B
B	��B	�3B	��B	�bB	dZB	>wB	2-B	J�B	9XB	�B�B�;B��BƨBĜBB��B�qB�^B�FB�-B�B�B��B��B��B��B��B��B��B��B��B�B�?B�dB�wB��B��B��B�wB�qB�jB�dB��B�}BÖBŢBȴBƨB�wB�jB��BÖBĜBŢB��B��B��B�B�B�)B�#B�B�HB�yB��B	B	B	B	%B	DB	1B	B	B	B	B	B	hB	hB	oB	{B	oB	�B	�B	,B	.B	0!B	1'B	2-B	5?B	8RB	<jB	@�B	B�B	E�B	B�B	B�B	C�B	F�B	E�B	:^B	#�B	DB��B��B	+B	>wB	W
B	]/B	^5B	]/B	]/B	_;B	jB	w�B	|�B	|�B	{�B	}�B	y�B	|�B	� B	�B	�B	�B	�B	� B	�B	�B	�B	�B	�B	�+B	�PB	�bB	�hB	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�-B	�'B	�'B	�'B	�3B	�FB	�XB	�dB	�dB	�qB	��B	��B	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	ɺB	ɺB	��B	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�
B	�B	�B	�B	��B	�B	�B	�
B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�5B	�NB	�TB	�TB	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B
B
  B
B
  B
  B
  B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
  B	��B
  B
%B
+B
%B
B
B
B
B
%B
+B
+B
+B
+B

=B
	7B
1B
%B
B
	7B
uB
{B
{B
{B
uB
uB
uB
uB
uB
uB
uB
uB
oB
oB
hB
bB
bB
bB
\B
VB
PB
PB
PB
JB
JB
JB
JB
JB
JB
PB
bB
B
!|22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
7LB
�B
��B
�B
�?B
�dB
�B
�B
��B
�B1B)�B<jBC�BD�BG�BH�BL�BgmBs�BhsB�=B�DB��B��B�-B�^BɺB�
B�B�B�/B�5B�B��B
=BuB�B{B�B�B�B�B�B �B#�B �B�B�B�B�B{BPB+B��B�B�B�BB��B�}B�3B�B��B��B�B`BBW
BO�BJ�BD�B>wB(�BuB
�B
��B
�B
�=B
�+B
k�B
dZB
[#B
33B
�B
{B
B	��B	�3B	��B	�bB	dZB	>wB	2-B	J�B	9XB	�B�B�;B��BƨBĜBB��B�qB�^B�FB�-B�B�B��B��B��B��B��B��B��B��B��B�B�?B�dB�wB��B��B��B�wB�qB�jB�dB��B�}BÖBŢBȴBƨB�wB�jB��BÖBĜBŢB��B��B��B�B�B�)B�#B�B�HB�yB��B	B	B	B	%B	DB	1B	B	B	B	B	B	hB	hB	oB	{B	oB	�B	�B	,B	.B	0!B	1'B	2-B	5?B	8RB	<jB	@�B	B�B	E�B	B�B	B�B	C�B	F�B	E�B	:^B	#�B	DB��B��B	+B	>wB	W
B	]/B	^5B	]/B	]/B	_;B	jB	w�B	|�B	|�B	{�B	}�B	y�B	|�B	� B	�B	�B	�B	�B	� B	�B	�B	�B	�B	�B	�+B	�PB	�bB	�hB	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�-B	�'B	�'B	�'B	�3B	�FB	�XB	�dB	�dB	�qB	��B	��B	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	ɺB	ɺB	��B	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�
B	�B	�B	�B	��B	�B	�B	�
B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�5B	�NB	�TB	�TB	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B
B
  B
B
  B
  B
  B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
  B	��B
  B
%B
+B
%B
B
B
B
B
%B
+B
+B
+B
+B

=B
	7B
1B
%B
B
	7B
uB
{B
{B
{B
uB
uB
uB
uB
uB
uB
uB
uB
oB
oB
hB
bB
bB
bB
\B
VB
PB
PB
PB
JB
JB
JB
JB
JB
JB
PB
bB
B
!|22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190613                              AO  ARCAADJP                                                                    20181005190613    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190613  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190613  QCF$                G�O�G�O�G�O�8000            