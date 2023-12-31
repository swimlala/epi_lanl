CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:31Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112801  20190522121836  1901_5055_027                   2C  D   APEX                            2140                            040306                          846 @�lp��1   @�lp���@+�Ƨ�ck��S��1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@���@���AffA@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B��B  B   B(  B0  B8ffB@  BH  BP  BX  B`  BhffBp  Bw��B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�33C   C  C  C�C�C
  C�fC  C  C  C�fC  C�C  C  C  C   C!�fC#�fC&  C(  C*  C+�fC.  C0  C2  C4  C6  C8�C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX�CZ�C\�C^�C`  Cb  Cd  Cf�Ch  Ci�fCl  Cn  Co�fCr  Ct�Cv  Cx  Cy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C�  C��C��C��C�  C��3C�  C��C�  C�  C��3C��3C�  C��C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C�  C��C��C�  C�  C��3C�  C�  C�  C��3C�  C��C��C�  C��3C��3C�  C��C��C��C��C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C��3C�  C��C�  C�  C��C�  C��3C��3C�  C�  C��C�  C�  C��C�  C��3C��3C�  C��C�  C��3C�  C��C��C��C�  C�  C�  C��3C��3C�  C��C��C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C��3C��D   D � D  D� D  D� D  D� DfD� DfD�fD  Dy�D  D� D��Dy�D	  D	y�D
  D
�fD  Dy�D��Dy�D  D� DfD� D��D� DfD�fD  D�fD  D� DfD� D  D�fD  D� DfD� D  D�fD  Dy�D  D� D  D� D��D� DfD� D��D� DfD� D  Dy�D   D �fD!  D!� D!��D"� D#fD#� D$  D$� D%fD%� D&  D&y�D&��D'� D(fD(� D)  D)�fD*fD*� D+  D+� D,  D,y�D-  D-�fD.  D.� D/  D/� D/��D0� D1  D1y�D2  D2� D3  D3�fD4fD4� D5  D5� D6  D6� D7fD7� D7��D8y�D8��D9� D:  D:� D;  D;y�D<  D<�fD=fD=� D>  D>� D>��D?� D@fD@� DA  DA� DB  DB� DC  DCy�DC��DD� DEfDE� DF  DF� DF��DG� DH  DH� DI  DI� DJ  DJ� DJ��DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQy�DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DV��DW� DXfDX�fDYfDY� DZ  DZ� D[  D[� D\  D\� D]fD]� D]��D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg�fDhfDh� Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu� Dv  Dvs3DyL�D��fD�FfD�p D�ٚD��3D�33D�` D��fD�3D�9�D�` DǶfD��3D�#3D�@ D�fD��3D�,�D�I�D�L�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @l��@�  @�  A  A9��AY��Ay��A���A���A���A���A͙�A���A���A���BffB  BffBffB&ffB.ffB6��B>ffBFffBNffBVffB^ffBf��BnffBv  B~ffB�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�ffB�33B�33B�ffB�33B�33B�33B�33B�ffB�33B�33B�33B�33B�  B�33B�ffB�33C��C��C�3C�3C	��C� C��C��C��C� C��C�3C��C��C��C��C!� C#� C%��C'��C)��C+� C-��C/��C1��C3��C5��C7�3C9��C;��C=��C?� CA��CC��CE��CG��CI��CK��CM��CO��CQ� CS��CU��CW�3CY�3C[�3C]�3C_��Ca��Cc��Ce�3Cg��Ci� Ck��Cm��Co� Cq��Cs�3Cu��Cw��Cy� C{� C}� C� C�� C�� C�� C�� C�� C���C�ٚC�ٚC�ٚC���C�� C���C�ٚC���C���C�� C�� C���C�ٚC���C���C�� C���C�ٚC���C���C���C���C���C���C���C���C�� C�� C�� C�� C�� C�� C�� C�� C���C�ٚC�ٚC���C���C�� C���C���C���C�� C���C�ٚC�ٚC���C�� C�� C���C�ٚC�ٚC�ٚC�ٚC���C���C���C�� C���C�ٚC�ٚC���C���C���C���C�� C���C�ٚC���C���C�ٚC���C�� C�� C���C���C�ٚC���C���C�ٚC���C�� C�� C���C�ٚC���C�� C���C�ٚC�ٚC�ٚC���C���C���C�� C�� C���C�ٚC�ٚC���C���C�� C�� C���C�ٚC���C���C���C���C���C���C���C�� C�� C�� C���C���C���C�� C�ٚC���D ffD �fDffD�fDffD�fDffD��DffD��Dl�D�fD` D�fDffD� D` D�fD	` D	�fD
l�D
�fD` D� D` D�fDffD��DffD� DffD��Dl�D�fDl�D�fDffD��DffD�fDl�D�fDffD��DffD�fDl�D�fD` D�fDffD�fDffD� DffD��DffD� DffD��DffD�fD` D�fD l�D �fD!ffD!� D"ffD"��D#ffD#�fD$ffD$��D%ffD%�fD&` D&� D'ffD'��D(ffD(�fD)l�D)��D*ffD*�fD+ffD+�fD,` D,�fD-l�D-�fD.ffD.�fD/ffD/� D0ffD0�fD1` D1�fD2ffD2�fD3l�D3��D4ffD4�fD5ffD5�fD6ffD6��D7ffD7� D8` D8� D9ffD9�fD:ffD:�fD;` D;�fD<l�D<��D=ffD=�fD>ffD>� D?ffD?��D@ffD@�fDAffDA�fDBffDB�fDC` DC� DDffDD��DEffDE�fDFffDF� DGffDG�fDHffDH�fDIffDI�fDJffDJ� DK` DK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPl�DP�fDQ` DQ�fDRffDR�fDSffDS�fDT` DT�fDUffDU�fDVffDV� DWffDW��DXl�DX��DYffDY�fDZffDZ�fD[ffD[�fD\ffD\��D]ffD]� D^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf��Dgl�Dg��DhffDh� DiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm� DnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt��DuffDu�fDvY�Dy33D��D�9�D�c3D���D��fD�&fD�S3D���D��fD�,�D�S3Dǩ�D��fD�fD�33Dਗ਼D��fD�  D�<�D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aͧ�Aͥ�Aͥ�Aͧ�Aͥ�Aͥ�Aͣ�Aͥ�Aͩ�AͬAͬAͬAͰ!AͮAͲ-Aʹ9Aʹ9Aʹ9AͶFAͶFAͶFAͲ-AͮA͓uA�S�A�A�XA���AǅA�VA�C�A�9XA�+A��A�
=A��A���AƲ-Aƕ�A�M�A��A�n�Aě�A�/A�A�^5A���A�r�A���A�A�A�x�A�"�A���A��HA��;A�jA���A�S�A��^A��A���A���A���A���A��/A�Q�A�&�A�^5A��A�VA�S�A�t�A�A�%A�A�ƨA���A�t�A��A�+A��/A�l�A���A���A��mA��`A��DA~�`A}S�Az��At��Ar�Ap=qAnM�Aj$�Ag33Ad�jAb �A]�AZffAVJAS�AQC�AO�hAM�
AI��AG�hAE�AD�uAC?}AAA>=qA<A:bNA81A5��A4bNA2��A1dZA/�
A.��A-ƨA+�
A*{A(�/A(VA(�A'�mA&jA#��A��A  A
=A��A�RAI�A�wA9XA�;A�;A�A��A�A�-A�A  AhsA��A�A  A��A33A�jAM�A�A�FA\)A�A�A�A��A�A�/AG�A�TA�hA�+AdZA�FA
�\A
�uAoA
1'A	ƨA��A�A��A%AS�A��A�RA��A=qAz�A�A33A�yAZA�FA�;AA�AffAr�A~�AM�A�mA �HA ��AA �/A I�A $�A �+A E�A {@�@��+@��A v�AG�A��A �\A (�@���@�C�@���@��@���@��`@��/@�r�@�33@��T@�{@�M�@��h@���@���@�ƨ@�$�@���@�+@�{@�&�@�G�@�Z@�F@�C�@@�@���@���@�D@�
=@�ȴ@�V@�?}@�Q�@�  @�@�dZ@�33@�~�@��@�7L@�9@�u@�I�@���@�|�@�ȴ@�O�@�9@�Z@��@ߝ�@�dZ@ާ�@ݙ�@���@�A�@�"�@�E�@ٺ^@�`B@أ�@�1@���@׶F@׮@�b@��@׾w@׶F@׮@ם�@���@��
@ׅ@�"�@�ff@���@�@�p�@�V@�j@���@��@ҏ\@�V@��@Ѻ^@Ѻ^@�`B@���@Ь@�1@�K�@��@�5?@Ͳ-@�hs@�?}@��@��`@̴9@�Q�@�I�@��@�dZ@�n�@ɡ�@�%@���@ȋD@�%@�r�@��@�^5@�-@�-@Ų-@�O�@�1'@�|�@�M�@��^@��@��@��
@�K�@��@�$�@�M�@�;d@�S�@���@���@�~�@�^5@���@���@�  @�33@���@��+@�o@���@��-@�1'@�l�@�l�@��H@���@�x�@�33@���@��`@�1'@�  @�t�@�;d@��@�M�@��T@��h@�/@��/@�bN@�b@���@��@��\@�=q@���@��@���@��;@�ƨ@��F@���@�
=@���@�{@�{@�{@���@��@�Ĝ@���@�b@��@���@��@�ȴ@���@��+@�ff@��#@��h@�p�@��@��@��/@�9X@��F@�dZ@�+@�o@���@�M�@��T@��h@�x�@�p�@�hs@�7L@���@���@�bN@��@��m@��F@�dZ@�@���@���@�ff@�-@�J@��#@��h@��@��/@���@�j@��@�\)@�S�@��@�ȴ@���@�~�@�=q@��@��h@�V@���@��@�I�@��@��w@�K�@�
=@��H@��!@���@��+@�ff@�-@��@���@��^@���@�x�@�G�@�Ĝ@�r�@�(�@���@���@�\)@�"�@��@�~�@��@��@���@�x�@�O�@��`@�C�@��@l�@t��@hQ�@_\)@W��@O�@H�@AX@8��@1&�@*�@'+@!&�@C�@�@�H@��@"�@A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aͧ�Aͥ�Aͥ�Aͧ�Aͥ�Aͥ�Aͣ�Aͥ�Aͩ�AͬAͬAͬAͰ!AͮAͲ-Aʹ9Aʹ9Aʹ9AͶFAͶFAͶFAͲ-AͮA͓uA�S�A�A�XA���AǅA�VA�C�A�9XA�+A��A�
=A��A���AƲ-Aƕ�A�M�A��A�n�Aě�A�/A�A�^5A���A�r�A���A�A�A�x�A�"�A���A��HA��;A�jA���A�S�A��^A��A���A���A���A���A��/A�Q�A�&�A�^5A��A�VA�S�A�t�A�A�%A�A�ƨA���A�t�A��A�+A��/A�l�A���A���A��mA��`A��DA~�`A}S�Az��At��Ar�Ap=qAnM�Aj$�Ag33Ad�jAb �A]�AZffAVJAS�AQC�AO�hAM�
AI��AG�hAE�AD�uAC?}AAA>=qA<A:bNA81A5��A4bNA2��A1dZA/�
A.��A-ƨA+�
A*{A(�/A(VA(�A'�mA&jA#��A��A  A
=A��A�RAI�A�wA9XA�;A�;A�A��A�A�-A�A  AhsA��A�A  A��A33A�jAM�A�A�FA\)A�A�A�A��A�A�/AG�A�TA�hA�+AdZA�FA
�\A
�uAoA
1'A	ƨA��A�A��A%AS�A��A�RA��A=qAz�A�A33A�yAZA�FA�;AA�AffAr�A~�AM�A�mA �HA ��AA �/A I�A $�A �+A E�A {@�@��+@��A v�AG�A��A �\A (�@���@�C�@���@��@���@��`@��/@�r�@�33@��T@�{@�M�@��h@���@���@�ƨ@�$�@���@�+@�{@�&�@�G�@�Z@�F@�C�@@�@���@���@�D@�
=@�ȴ@�V@�?}@�Q�@�  @�@�dZ@�33@�~�@��@�7L@�9@�u@�I�@���@�|�@�ȴ@�O�@�9@�Z@��@ߝ�@�dZ@ާ�@ݙ�@���@�A�@�"�@�E�@ٺ^@�`B@أ�@�1@���@׶F@׮@�b@��@׾w@׶F@׮@ם�@���@��
@ׅ@�"�@�ff@���@�@�p�@�V@�j@���@��@ҏ\@�V@��@Ѻ^@Ѻ^@�`B@���@Ь@�1@�K�@��@�5?@Ͳ-@�hs@�?}@��@��`@̴9@�Q�@�I�@��@�dZ@�n�@ɡ�@�%@���@ȋD@�%@�r�@��@�^5@�-@�-@Ų-@�O�@�1'@�|�@�M�@��^@��@��@��
@�K�@��@�$�@�M�@�;d@�S�@���@���@�~�@�^5@���@���@�  @�33@���@��+@�o@���@��-@�1'@�l�@�l�@��H@���@�x�@�33@���@��`@�1'@�  @�t�@�;d@��@�M�@��T@��h@�/@��/@�bN@�b@���@��@��\@�=q@���@��@���@��;@�ƨ@��F@���@�
=@���@�{@�{@�{@���@��@�Ĝ@���@�b@��@���@��@�ȴ@���@��+@�ff@��#@��h@�p�@��@��@��/@�9X@��F@�dZ@�+@�o@���@�M�@��T@��h@�x�@�p�@�hs@�7L@���@���@�bN@��@��m@��F@�dZ@�@���@���@�ff@�-@�J@��#@��h@��@��/@���@�j@��@�\)@�S�@��@�ȴ@���@�~�@�=q@��@��h@�V@���@��@�I�@��@��w@�K�@�
=@��H@��!@���@��+@�ff@�-@��@���@��^@���@�x�@�G�@�Ĝ@�r�@�(�@���@���@�\)@�"�@��@�~�@��@��@���@�x�@�O�@��`@�C�@��@l�@t��@hQ�@_\)@W��@O�@H�@AX@8��@1&�@*�@'+@!&�@C�@�@�H@��@"�@A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	w�B	w�B	w�B	w�B	w�B	w�B	x�B	w�B	w�B	w�B	x�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	v�B	v�B	v�B	u�B	u�B	�uB	�/B
 �B
0!B
49B
49B
33B
49B
5?B
7LB
<jB
<jB
:^B
8RB
9XB
:^B
>wB
8RB
/B
B�B
S�B
� B
��B
�fBoB$�B,B33B)�B33BH�B\)BVBM�BE�B=qB"�B�B+B=qBk�B��B��B��B��B�wB��BɺB�qB��BgmB�B
�fB
�B
�PB
o�B
E�B
(�B	��B	�B	�RB	��B	�B	x�B	s�B	e`B	VB	K�B	A�B	6FB	(�B	�B		7B��B�B�yB�5B��B��B��BƨB�?B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB��B��B��B��B�jBÖB�?B��B�oB�\B�PB�VB�\B�\B�uB��B��B��B��B��B��B��B��B�B�'B�-B�-B�XB�wB��B��BŢB��B��B�B	bB	/B	7LB	cTB	�B	�B	{�B	u�B	z�B	u�B	n�B	e`B	ffB	l�B	~�B	�B	|�B	v�B	n�B	gmB	VB	N�B	bNB	YB	Q�B	_;B	iyB	s�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�B	�B	�B	�B	�^B	��B	�/B	�BB	�TB	�ZB	�TB	�`B	�`B	�TB	�NB	�NB	�TB	�ZB	�`B	�NB	�mB	�B	�yB	�B	�B	�B	�`B	�/B	�#B	�B	�B	�)B	�#B	�B	�#B	�#B	�#B	�)B	�)B	�#B	�)B	�/B	�/B	�)B	�)B	�;B	�;B	�;B	�;B	�5B	�5B	�5B	�;B	�HB	�NB	�`B	�ZB	�TB	�;B	�5B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�5B	�/B	�5B	�5B	�/B	�/B	�5B	�;B	�BB	�NB	�fB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B	��B
B
B	��B	��B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B
%B
B
B
B
B
B	��B	��B	��B	��B
B
B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B	��B
  B
B
B
B
B
  B
  B
B
B
B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
JB
PB
PB
PB
VB
\B
bB
oB
oB
oB
uB
uB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
49B
<jB
B�B
J�B
L�B
P�B
XB
`BB
e`B
iyB
m�B
p�B
s�B
w�B
z�B
~�B
�B
�%B
�=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	w�B	w�B	w�B	w�B	w�B	w�B	x�B	w�B	w�B	w�B	x�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	v�B	v�B	w�B	x�B	�B	��B	�B
$�B
33B
6FB
5?B
49B
5?B
6FB
8RB
=qB
=qB
;dB
:^B
<jB
>wB
C�B
?}B
33B
F�B
VB
{�B
��B
�B�B-B33BA�B49B?}B[#Bq�Bm�BaHB[#BQ�B1'B'�B8RBR�B|�B��B��B��B�B�B�NB�
B��B��B��B5?BDB
��B
�B
�{B
_;B
R�B
 �B
	7B	�5B	�jB	�uB	�+B	�JB	�%B	hsB	[#B	R�B	Q�B	>wB	+B	 �B	�B	1B	B�B�NB�NB�ZB�;BĜB�XB�FB�?B�XB�FB�3B�B�-B�B��B��B��B��B��B��B��B��B��B��B��BǮB��B��B�RB��B��B��B��B�uB�uB��B�LB��B�-B�-B�!B�'B�B�B�3B�?B�FB�LB�jBBŢBĜBǮB��B��B��B	1B	,B	/B	_;B	�7B	�bB	�B	z�B	�B	� B	x�B	k�B	ffB	l�B	�%B	�%B	�B	}�B	w�B	w�B	^5B	J�B	p�B	^5B	M�B	]/B	hsB	q�B	�B	�B	{�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�'B	�'B	�B	�B	�9B	ɺB	�5B	�mB	�fB	�sB	�ZB	�sB	�yB	�`B	�TB	�TB	�fB	�sB	�yB	�NB	�sB	�B	�B	�B	��B	�B	�B	�HB	�;B	�/B	�#B	�BB	�5B	�)B	�5B	�5B	�5B	�/B	�5B	�BB	�5B	�BB	�HB	�;B	�/B	�HB	�BB	�HB	�NB	�BB	�HB	�BB	�BB	�NB	�ZB	�sB	�sB	�sB	�;B	�BB	�/B	�5B	�BB	�5B	�NB	�HB	�;B	�5B	�BB	�5B	�BB	�/B	�;B	�5B	�;B	�BB	�HB	�fB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
B
B	��B	��B	��B	��B
%B
B	��B	��B
B
B
B
+B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
+B
+B
%B
%B
B
%B
B	��B	��B	��B	��B
B
B
B	��B	��B	��B	��B	��B	��B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B
  B
B
B
B
B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
%B
%B
%B
+B
%B
+B
%B
1B
1B
1B
1B
	7B
1B
	7B

=B

=B

=B

=B
DB
DB
PB
JB
PB
VB
PB
PB
VB
bB
bB
uB
oB
uB
{B
{B
uB
{B
uB
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
49B
<jB
B�B
J�B
M�B
Q�B
XB
`BB
e`B
iyB
m�B
q�B
s�B
w�B
{�B
~�B
�B
�+B
�=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�t�<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<D��<�t�<�1<�j<���<�1<��
<T��<D��<T��<�1<�C�<D��<#�
<#�
<D��<�j<��
<T��<�t�=�w=8Q�<�h=t�<�1<�=t�<���='�=\)=@�=�P=C�<u<e`B<ě�=o<�t�<u<�C�<�/<�1<�t�<�9X<�`B<���<���<���<T��<e`B<u<�j<u<49X<49X<D��<�C�<�t�<�o<T��<�C�<e`B<D��<#�
<D��<49X<#�
<#�
<e`B<D��<#�
<#�
<#�
<49X<�o<�9X<�j<T��<#�
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
<D��<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250162012011312501620120113125016  AO  ARGQ                                                                        20111205112801  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112801  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125016  IP                  G�O�G�O�G�O�                