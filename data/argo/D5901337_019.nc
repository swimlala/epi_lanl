CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:28Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112631  20190522121836  1901_5055_019                   2C  D   APEX                            2140                            040306                          846 @�X���_�1   @�X��m��@/�;dZ��c^~��"�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  @���A   A@  A`  A�  A�  A�  A�33A�  A�  A�33A�  B ffB  B  B  B   B'��B/��B8  B@ffBH  BP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B���C   C�C  C  C  C
  C  C�C  C  C  C  C�C  C  C  C   C!�fC$  C&  C(�C*  C,  C.�C0  C2  C4  C6  C8  C:  C;�fC>  C@�CB�CD  CE�fCG�fCI�fCL  CN  CP  CQ�fCS�fCV  CX  CZ�C\  C^  C`  Cb�Cd  Cf  Ch  Cj�Cl  Cm�fCo�fCr  Ct  Cv�Cx  Cy�fC|  C~  C�  C�  C�  C��3C��3C�  C�  C�  C��C��C�  C��3C��3C��3C��3C��3C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C��3C�  C��C��C�  C�  C��3C�  C��C��C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C��3C�  C��C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C��3C�  C��C��C��C�  C�  C��3C�  C�  C��3C�  C�  C��C��C��3C�  D   D � DfD� D  D�fD  Dy�D  D� D  D� D  D� DfD� D  D� D	  D	y�D	��D
� DfD� D��Dy�D��Dy�D��Dy�D��Dy�D  D� D��Dy�D��D� D  D� DfD�fD  Dy�D��D� D  D� D  D� DfD� D��Dy�D  D�fD  D� D  D�fDfD�fDfDy�D��D � D!  D!� D"  D"� D#  D#� D$fD$�fD%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D*��D+� D,  D,� D-  D-� D.  D.� D/fD/�fD0  D0y�D1  D1� D2  D2y�D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9�fD:fD:�fD;  D;� D;��D<y�D=  D=�fD>fD>�fD?  D?� D@  D@� DAfDA�fDBfDB�fDCfDC� DC��DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQy�DR  DR�fDS  DS� DT  DT� DT��DUy�DV  DV�fDW  DW� DX  DX� DY  DY� DY��DZy�DZ��D[y�D[��D\y�D]  D]� D]��D^y�D^��D_y�D_��D`� Da  Da� Db  Db� Dc  Dc�fDd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dy` D��D�<�D�i�D�� D�3D�C3D�l�D�ɚD���D�#3D�s3DǬ�D�fD�)�D�VfD��D���D�&fD�` D�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @�33@�  A��A9��AY��Ay��A���A���A�  A���A���A�  A���A���BffBffBffBffB&  B.  B6ffB>��BFffBNffBVffB^ffBf��Bn��BvffB~ffB�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�  B�33B�33B�33B�33B�  B�33C�3C��C��C��C	��C��C�3C��C��C��C��C�3C��C��C��C��C!� C#��C%��C'�3C)��C+��C-�3C/��C1��C3��C5��C7��C9��C;� C=��C?�3CA�3CC��CE� CG� CI� CK��CM��CO��CQ� CS� CU��CW��CY�3C[��C]��C_��Ca�3Cc��Ce��Cg��Ci�3Ck��Cm� Co� Cq��Cs��Cu�3Cw��Cy� C{��C}��C��C���C���C�� C�� C���C���C���C�ٚC�ٚC���C�� C�� C�� C�� C�� C���C�ٚC���C�� C���C���C�� C���C���C���C�� C���C�ٚC�ٚC���C���C�� C���C�ٚC�ٚC���C���C�� C���C���C���C���C�ٚC���C���C�ٚC���C���C���C���C���C�� C���C���C�� C���C���C���C�� C�� C�� C���C���C���C���C���C���C���C���C���C���C���C���C�� C�� C���C�ٚC���C�� C���C�ٚC���C���C�ٚC���C���C���C�ٚC���C���C���C���C���C���C���C�� C�� C�� C�� C���C���C�ٚC���C���C�ٚC���C���C�ٚC���C���C�� C���C�ٚC�ٚC�ٚC���C���C�� C���C���C�� C���C���C�ٚC�ٚC�� C���C���D ffD ��DffD�fDl�D�fD` D�fDffD�fDffD�fDffD��DffD�fDffD�fD	` D	� D
ffD
��DffD� D` D� D` D� D` D� D` D�fDffD� D` D� DffD�fDffD��Dl�D�fD` D� DffD�fDffD�fDffD��DffD� D` D�fDl�D�fDffD�fDl�D��Dl�D��D` D� D ffD �fD!ffD!�fD"ffD"�fD#ffD#��D$l�D$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(��D)ffD)�fD*ffD*� D+ffD+�fD,ffD,�fD-ffD-�fD.ffD.��D/l�D/�fD0` D0�fD1ffD1�fD2` D2�fD3l�D3�fD4ffD4�fD5ffD5�fD6ffD6�fD7l�D7�fD8ffD8�fD9l�D9��D:l�D:�fD;ffD;� D<` D<�fD=l�D=��D>l�D>�fD?ffD?�fD@ffD@��DAl�DA��DBl�DB��DCffDC� DDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK��DLffDL�fDMl�DM�fDNffDN�fDOffDO�fDPffDP�fDQ` DQ�fDRl�DR�fDSffDS�fDTffDT� DU` DU�fDVl�DV�fDWffDW�fDXffDX�fDYffDY� DZ` DZ� D[` D[� D\` D\�fD]ffD]� D^` D^� D_` D_� D`ffD`�fDaffDa�fDbffDb�fDcl�Dc�fDdffDd� DeffDe�fDfffDf�fDgffDg�fDh` Dh�fDil�Di�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDyFfD���D�0 D�\�D��3D��fD�6fD�` D���D�� D�fD�ffDǠ D���D��D�I�D� D�� D��D�S3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�-A�(�A�(�A�-A�1'A�1'A�/A�/A�/A�1'A�1'A�1'A�33A�7LA�7LA�7LA�7LA�5?A�5?A�5?A�7LA�9XA�9XA�9XA�;dA�=qA�?}A�=qA�=qA�C�A�E�A�A�A�C�A�K�A�K�A�O�A�M�A�C�AɸRA�ffA�K�A���A��A���A��#A�ffA�C�A�C�A�v�A��A�-A�A�33A�+A�bNA�(�A��/A�33A�?}A��A�^5A�  A�(�A��TA��A��A�oA��;A��mA��TA���A�&�A�v�A�+A�hsA+A|bNAy�PAv��At�Ap�Al�AhbAgO�Ac��A^�uAZ��AX~�AT�+AQ�AP��AN�\AJ�AHr�AG�7AF�uAE�;ABz�A?�hA>  A<��A;�hA;VA:�A8�A7��A7?}A6��A6r�A3t�A-�A,M�A,E�A+��A*�HA);dA(��A(ĜA(~�A( �A'��A'l�A&�9A%K�A#��A��AQ�A/A��A-A��A�^AXA�\A�!A9XA��A�A�A�
A7LAVAbA�7A33A�FA��A�mA��A�
A�A"�An�A9XA�A �A�A{A5?A�wAG�A��AM�A �A�A�A��A�A%A��A��A  AdZAVA��A=qAK�A
bA	ƨA	"�AE�A1A�Az�A	\)A
1A
�jA��A$�A�A�A�FA�-A�A
ĜA
9XA
$�A
E�A
{A	�A	�wA��A��A�9A1'A�9A33AdZA;dA�`AAZAbNAr�A�;A��A��AZA�A bN@�;d@�x�@�(�@���@�K�@�=q@�/@�?}@��/@��y@��j@�A�@�(�@�1'@�b@��@�C�@��H@�$�@�p�@��@�9@�z�@@�\@�-@��@���@�dZ@�@�V@���@�X@���@��@��@�7L@���@�u@���@��y@��@�G�@���@�1'@�  @�K�@��@ާ�@�V@���@��/@�z�@�z�@��m@ۅ@ۅ@�(�@���@�O�@�&�@ܬ@�r�@۝�@�
=@���@ڗ�@�v�@�-@�@�7L@ؼj@�|�@ָR@�{@�%@Դ9@�Z@��
@��
@�1'@�bN@� �@�S�@�J@���@�Q�@��@�Q�@�b@��
@�ƨ@�t�@�+@�n�@�p�@���@�j@�1@˅@���@�^5@��@ɡ�@�x�@�O�@��@ȋD@�bN@�I�@� �@�C�@��@Ə\@�^5@�{@��#@Ų-@Ł@Ĭ@�1'@��@å�@�S�@�S�@�|�@�l�@�^5@�X@���@�Ĝ@�(�@��w@��F@�t�@��\@�=q@��@�{@�{@�J@�@��@��@��@�r�@���@�"�@�33@���@�=q@��T@�x�@�?}@��/@���@�ƨ@�;d@��@���@�ff@�M�@�=q@��^@�G�@���@��D@�I�@��@�\)@�;d@�dZ@�+@��@��#@�x�@�p�@���@�bN@��P@�33@�"�@�ȴ@��@�O�@�b@��F@�t�@��@��R@��+@�=q@��T@��7@�V@���@�bN@�Z@�Q�@�Q�@�A�@��@��@�o@��@���@���@���@��h@���@���@��u@�  @��w@��w@�S�@�M�@���@��/@�Ĝ@��D@���@��R@�n�@�M�@�$�@���@�O�@��@�O�@�`B@�O�@�&�@�%@��@�b@���@�\)@�"�@�33@�S�@�l�@�|�@�\)@��H@��R@�n�@�E�@�{@��T@�p�@�O�@�?}@�7L@�%@��j@�Z@�ƨ@�@��+@�n�@�E�@�5?@���@�G�@��/@��@���@���@��u@��w@�ȴ@���@�-@���@w
=@l�/@dI�@\��@T�D@Mp�@GK�@?�w@8 �@1�@*��@%O�@ r�@^5@�@�9@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�+A�-A�(�A�(�A�-A�1'A�1'A�/A�/A�/A�1'A�1'A�1'A�33A�7LA�7LA�7LA�7LA�5?A�5?A�5?A�7LA�9XA�9XA�9XA�;dA�=qA�?}A�=qA�=qA�C�A�E�A�A�A�C�A�K�A�K�A�O�A�M�A�C�AɸRA�ffA�K�A���A��A���A��#A�ffA�C�A�C�A�v�A��A�-A�A�33A�+A�bNA�(�A��/A�33A�?}A��A�^5A�  A�(�A��TA��A��A�oA��;A��mA��TA���A�&�A�v�A�+A�hsA+A|bNAy�PAv��At�Ap�Al�AhbAgO�Ac��A^�uAZ��AX~�AT�+AQ�AP��AN�\AJ�AHr�AG�7AF�uAE�;ABz�A?�hA>  A<��A;�hA;VA:�A8�A7��A7?}A6��A6r�A3t�A-�A,M�A,E�A+��A*�HA);dA(��A(ĜA(~�A( �A'��A'l�A&�9A%K�A#��A��AQ�A/A��A-A��A�^AXA�\A�!A9XA��A�A�A�
A7LAVAbA�7A33A�FA��A�mA��A�
A�A"�An�A9XA�A �A�A{A5?A�wAG�A��AM�A �A�A�A��A�A%A��A��A  AdZAVA��A=qAK�A
bA	ƨA	"�AE�A1A�Az�A	\)A
1A
�jA��A$�A�A�A�FA�-A�A
ĜA
9XA
$�A
E�A
{A	�A	�wA��A��A�9A1'A�9A33AdZA;dA�`AAZAbNAr�A�;A��A��AZA�A bN@�;d@�x�@�(�@���@�K�@�=q@�/@�?}@��/@��y@��j@�A�@�(�@�1'@�b@��@�C�@��H@�$�@�p�@��@�9@�z�@@�\@�-@��@���@�dZ@�@�V@���@�X@���@��@��@�7L@���@�u@���@��y@��@�G�@���@�1'@�  @�K�@��@ާ�@�V@���@��/@�z�@�z�@��m@ۅ@ۅ@�(�@���@�O�@�&�@ܬ@�r�@۝�@�
=@���@ڗ�@�v�@�-@�@�7L@ؼj@�|�@ָR@�{@�%@Դ9@�Z@��
@��
@�1'@�bN@� �@�S�@�J@���@�Q�@��@�Q�@�b@��
@�ƨ@�t�@�+@�n�@�p�@���@�j@�1@˅@���@�^5@��@ɡ�@�x�@�O�@��@ȋD@�bN@�I�@� �@�C�@��@Ə\@�^5@�{@��#@Ų-@Ł@Ĭ@�1'@��@å�@�S�@�S�@�|�@�l�@�^5@�X@���@�Ĝ@�(�@��w@��F@�t�@��\@�=q@��@�{@�{@�J@�@��@��@��@�r�@���@�"�@�33@���@�=q@��T@�x�@�?}@��/@���@�ƨ@�;d@��@���@�ff@�M�@�=q@��^@�G�@���@��D@�I�@��@�\)@�;d@�dZ@�+@��@��#@�x�@�p�@���@�bN@��P@�33@�"�@�ȴ@��@�O�@�b@��F@�t�@��@��R@��+@�=q@��T@��7@�V@���@�bN@�Z@�Q�@�Q�@�A�@��@��@�o@��@���@���@���@��h@���@���@��u@�  @��w@��w@�S�@�M�@���@��/@�Ĝ@��D@���@��R@�n�@�M�@�$�@���@�O�@��@�O�@�`B@�O�@�&�@�%@��@�b@���@�\)@�"�@�33@�S�@�l�@�|�@�\)@��H@��R@�n�@�E�@�{@��T@�p�@�O�@�?}@�7L@�%@��j@�Z@�ƨ@�@��+@�n�@�E�@�5?@���@�G�@��/@��@���@���@��u@��w@�ȴ@���@�-@���@w
=@l�/@dI�@\��@T�D@Mp�@GK�@?�w@8 �@1�@*��@%O�@ r�@^5@�@�9@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	J�B	J�B	J�B	J�B	J�B	I�B	I�B	J�B	J�B	J�B	J�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	J�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	K�B	M�B	H�B	J�B	Q�B	O�B	M�B	M�B	ZB	�DB
PB
e`B
�B
`BB
cTB
jB
k�B
hsB
cTB
S�B
K�B
O�B
~�B
T�B
>wB
A�B
D�B
F�B
Q�B
@�B
A�B
7LB
33B
/B
+B
)�B
�B
�B
B	�`B	ŢB	�LB	�!B	�B	��B	z�B	]/B	K�B	>wB	.B	�B	
=B��B	B��B�B�B��BÖB�jB�9B�B��B��B�!B�'B��B��B�B�}B�FB�'B�!B�9B�dB��B�5B�B��B	PB��B�TB�ZB�B�B�sB�B��B	  B	VB	�B	%�B	-B	1'B	%�B	�B��B��B��B	  B��B�B��B�B�#B�`B	1B	1B	B		7B	%B	B��B��B��B	B	�B	33B	J�B	m�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�9B	�LB	�LB	�FB	�9B	�?B	�XB	�wB	��B	�}B	�qB	�dB	�RB	�^B	�RB	�3B	�B	�B	�B	�B	�3B	�LB	ĜB	�
B	�HB	�B	��B
	7B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
�B
uB

=B
DB
�B
'�B
-B
/B
)�B
#�B
 �B
$�B
'�B
$�B
�B
�B
�B
�B
JB
+B
B
  B
  B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B	��B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
1B
1B
1B

=B
	7B
	7B
	7B
	7B
	7B
	7B
1B
+B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B
1B
	7B
1B
1B
1B
1B
	7B
	7B
	7B

=B
DB
DB
	7B

=B
DB
DB
DB
DB
DB

=B
1B
%B
B
B
B
B
B
+B
+B
+B
+B
+B
+B
%B
%B
%B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
B
B	��B	��B	��B	��B
  B
  B
B
B
+B

=B
JB
PB
PB
JB
DB
	7B
1B
1B
	7B

=B
JB
JB
PB
PB
VB
\B
bB
bB
hB
bB
bB
hB
hB
bB
bB
bB
bB
\B
\B
\B
VB
VB
\B
\B
VB
VB
VB
VB
\B
bB
\B
oB
�B
�B
&�B
.B
49B
9XB
A�B
F�B
L�B
R�B
XB
^5B
cTB
iyB
l�B
p�B
u�B
{�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	J�B	J�B	J�B	J�B	J�B	I�B	I�B	J�B	J�B	J�B	J�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	J�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	K�B	M�B	H�B	J�B	Q�B	O�B	N�B	O�B	e`B	�!B
49B
�\B
��B
x�B
l�B
o�B
p�B
w�B
�B
gmB
`BB
jB
�uB
dZB
T�B
XB
L�B
T�B
l�B
ZB
[#B
H�B
@�B
C�B
B�B
9XB
.B
:^B
F�B
\B	�B	ȴB	�wB	��B	�dB	��B	o�B	_;B	Q�B	>wB	9XB	%�B	uB	bB	{B	DB�B�HB�5B��B�}BÖBĜB�9B�RB�dB�RB�qB�}B��B��B�RB�FB�dBƨB�B�ZB�B	JB	'�B	�B�B�mB�B��B�B�B��B	B	hB	�B	,B	7LB	>wB	7LB	2-B	%B	  B��B	�B	{B�)B�
B�B�B�TB	�B	hB	%B	PB	VB	hB	B��B��B��B	PB	(�B	>wB	cTB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	�FB	�RB	�XB	�XB	�XB	�RB	�FB	�XB	��B	ĜB	ĜB	B	�wB	�jB	�}B	�}B	�dB	�'B	�9B	�9B	�B	�-B	�3B	�wB	��B	�)B	�fB	��B

=B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
&�B
+B
"�B
�B
JB
1B
�B
'�B
/B
33B
2-B
-B
 �B
%�B
-B
+B
!�B
 �B
!�B
!�B
oB
PB
%B
B
B
B
B	��B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B
  B	��B
B
B
B
B
B
B
+B
+B
1B
1B
1B
	7B
	7B

=B
DB

=B
DB
DB
	7B
	7B
	7B
	7B
	7B
1B
+B
1B
DB
1B
1B
1B
	7B

=B

=B

=B
	7B
1B
1B
1B
1B
DB
	7B
	7B
DB
PB
VB

=B
DB
PB
PB
PB
JB
JB
JB
DB
	7B
%B
B
B
B
B
1B
1B
1B
1B
	7B
1B
+B
%B
%B
%B
+B
+B
+B
B
B
B
%B
B
B
B
B
+B
1B
+B
B
B
B
B
B
B
B
B	��B	��B	��B
  B
  B
B
B
+B

=B
JB
VB
PB
PB
PB
	7B
	7B
	7B
	7B

=B
JB
JB
PB
VB
VB
bB
hB
hB
oB
hB
hB
hB
hB
hB
hB
hB
oB
hB
bB
\B
\B
VB
bB
bB
\B
\B
VB
VB
bB
oB
\B
oB
�B
�B
&�B
/B
49B
9XB
A�B
F�B
M�B
R�B
XB
_;B
cTB
iyB
m�B
q�B
u�B
{�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
=t�=��='�=t�<ě�<#�
<#�
<#�
<u<�h<���<��
<���<��
<u<�9X<�9X<#�
<T��<���<ě�<���<�C�<T��<��
<�j<u<�C�=+=�o='�=t�<�C�<e`B<�9X=C�=+<�t�<�t�<���<�o<���<�/<ě�<T��<���<��<���<�t�<���<�C�<49X<��
<���<#�
<#�
<#�
<T��<�j<���<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<���<��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<�C�<�/<#�
<#�
<#�
<�1<��<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250142012011312501420120113125014  AO  ARGQ                                                                        20111205112631  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112631  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125014  IP                  G�O�G�O�G�O�                