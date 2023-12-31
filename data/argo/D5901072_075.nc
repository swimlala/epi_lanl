CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:59Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               KA   AO  20111130144256  20190522121829  1728_5048_075                   2C  D   APEX                            2142                            040306                          846 @�� �F?�1   @��!W?�@6ix����b�hr� �1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   BffBffBffB ffB(ffB0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B���B���B���B�  B�33B�33B�33B�  B���B�  B�33B�  B�  B�33B�  B���B�  B�33B�  B�  B�  B���B�  B�  B�  B���B���B���C  C  C  C�C	�fC�fC�fC  C  C  C  C�C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:�C<  C>  C@�CB  CD  CF  CH�CJ  CL  CM�fCO�fCQ�fCS�fCV  CX  CZ�C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C��C��C��C��C��C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C��C��C��C��C�  C��3C��3C�  C��3C��3C�  C��C��C�  C��3C�  C��C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C��C��C��C�  C�  C��3C�  C��C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C��3C�  C��C��C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C��3C�  C��C�  C��3D � D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  Dy�D	  D	� D	��D
�fD  D� D  D� D  D� DfD� D  D� D��Dy�DfD� D��D� D  D� D��Dy�D��Dy�D��Dy�D��D� DfD� D  D� D  D� D��Dy�D  D�fDfD�fDfD� D  D� D��D � D!fD!� D"  D"� D#  D#� D#��D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.y�D/  D/� D0  D0� D1  D1� D2fD2�fD3  D3y�D4  D4� D5fD5� D6  D6� D7fD7� D7��D8� D9  D9�fD:  D:� D;fD;� D<  D<� D<��D=� D>  D>y�D?  D?� D?��D@� DA  DA� DA��DB� DCfDC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH�fDI  DIy�DJ  DJ� DK  DK� DL  DL�fDMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^� D_fD_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv��DyffD��3D�9�D�l�D���D��D�  D�VfD��fD�3D�6fD�vfDǦfD�� D�fD�p D��D��3D�	�D�S3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @l��@�33@�33A��A9��AY��Ay��A���A���A���A�  A���A���A���A���B��B��B��B��B&��B.ffB6  B>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�ffB�33B�  B�  B�  B�33B�ffB�ffB�ffB�33B�  B�33B�ffB�33B�33B�ffB�33B�  B�33B�ffB�33B�33B�33B�  B�33B�33B�33B�  B�  B�  C��C��C��C�3C	� C� C� C��C��C��C��C�3C�3C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5� C7��C9�3C;��C=��C?�3CA��CC��CE��CG�3CI��CK��CM� CO� CQ� CS� CU��CW��CY�3C[�3C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}�3C�3C�ٚC�ٚC�ٚC�ٚC���C���C�ٚC���C�� C�� C���C���C���C���C���C�ٚC�ٚC�ٚC�ٚC���C�� C�� C���C�� C�� C���C�ٚC�ٚC���C�� C���C�ٚC���C���C���C���C�ٚC�� C���C���C���C���C�� C���C���C���C���C�� C�� C���C���C�ٚC���C���C�ٚC�ٚC�ٚC���C���C�� C���C�ٚC���C���C���C�ٚC�ٚC���C���C�ٚC���C���C���C���C�� C���C���C���C���C�� C���C�ٚC���C���C���C�� C�� C���C�ٚC�ٚC���C�� C���C���C���C���C���C�� C���C���C���C�ٚC���C���C���C���C���C���C���C�ٚC���C�� C���C���C�� C���C���C���C���C���C�ٚC�ٚC���C�� C���C�ٚC���C�� D ffD �fDffD�fDffD�fDffD�fD` D�fDffD�fDffD�fDffD�fD` D�fD	ffD	� D
l�D
�fDffD�fDffD�fDffD��DffD�fDffD� D` D��DffD� DffD�fDffD� D` D� D` D� D` D� DffD��DffD�fDffD�fDffD� D` D�fDl�D��Dl�D��DffD�fDffD� D ffD ��D!ffD!�fD"ffD"�fD#ffD#� D$ffD$�fD%` D%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,` D,�fD-ffD-�fD.` D.�fD/ffD/�fD0ffD0�fD1ffD1��D2l�D2�fD3` D3�fD4ffD4��D5ffD5�fD6ffD6��D7ffD7� D8ffD8�fD9l�D9�fD:ffD:��D;ffD;�fD<ffD<� D=ffD=�fD>` D>�fD?ffD?� D@ffD@�fDAffDA� DBffDB��DCffDC�fDDffDD��DEffDE�fDFffDF�fDGffDG�fDHl�DH�fDI` DI�fDJffDJ�fDKffDK�fDLl�DL��DMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ� DRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]� D^ffD^��D_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe� DfffDf�fDgffDg�fDhffDh�fDil�Di�fDjffDj�fDkffDk�fDlffDl�fDmffDm��DnffDn�fDoffDo�fDpffDp� DqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv� DyL�D��fD�,�D�` D���D���D�3D�I�D���D��fD�)�D�i�DǙ�D��3D�	�D�c3D� D��fD���D�Ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A�A�%A�%A�1A�bA��A��A��A��A� �A�+A�M�A�t�Aʙ�AʾwA��A�bA��A���AʾwAʡ�AʃA�bA��A���A�"�A��A��A���A�ĜA��A�hsA�oA���A��HA���A�9XA�x�A���A�O�A�  A��A��#A�/A��7A�ZA�1'A��`A�5?A�jA���A�33A��/A���A�C�A���A� �A���A���A�r�A���A�%A��RA�M�A�`BA��A���A��A��;A��HA�Q�A�-A���A��hA�\)A�+A���A�l�A��A��A�I�A���A��yA��uA�VA��A���A�VA� �A�=qA��A��A���A�v�A�VA��\A�{A���A��
A�JA���A�1A��9A�A��mA��wA��Ax�A}�^A|��A{Ax�+AwAv�At�As�#Ar{An�+Ak�Ai�Agp�Afv�AdȴAcp�Ab�Abr�Aa�Aa&�A_��A]�#A\ZAZ{AWO�AT�/AR��AP��AN�AM�wALĜAK��AIG�AHbAG�-AF�HAEXAC��AB��ABA�AA��AAXA@�HA?�wA>M�A>JA=�A=�^A=?}A<~�A;�A;7LA9/A7�A5�FA4�A3�A2  A0�uA.�9A-��A-%A,I�A+ƨA+`BA*�RA)�wA(�HA'�A'/A&bNA%%A#�-A"��A"=qA"{A"A!�A!�A!O�A ��A A`BA��A�7AVA��A|�A�uA�A�yA9XA�A?}A��A��AĜA�-A��A�A
�HA	;dA��Av�A �Ax�A9XAS�Av�A1A|�A�#AXAC�A7LA&�A �A �9A I�@��;@�$�@�bN@���@�O�@�A�@��@�5?@�w@��@��@��@�-@�7L@���@��@�I�@�t�@�@��@�;d@�v�@ᙚ@��@ܼj@�K�@�V@���@�\)@��@���@�C�@�X@��/@��m@�$�@�Ĝ@��m@���@�/@��@�-@���@�@�G�@���@�r�@���@��-@�?}@���@�(�@�1@���@��@�=q@�?}@� �@�+@���@�{@�p�@�z�@���@�"�@��\@�5?@�@���@�x�@���@��u@�1@�l�@�K�@�l�@�\)@�{@���@�hs@�?}@��@��@��j@��@�Z@�bN@�1'@��@��@�@�ȴ@�=q@�G�@��@�I�@��w@�t�@�o@���@�{@�@���@�?}@��`@��D@��
@�C�@�S�@�\)@��@��+@�%@��@��P@��@��m@�@��^@���@�1'@�K�@���@�5?@���@��#@�J@�=q@�J@���@�Ĝ@�z�@�1'@��@�|�@���@��@��^@���@�x�@�V@���@��/@���@�z�@�r�@�Z@�(�@���@��m@��@��m@��F@�l�@��@�~�@�V@�n�@�=q@�=q@�E�@�-@���@��h@�`B@�O�@��@��@�9X@�b@�  @��m@��w@���@�|�@�|�@�o@���@��\@��@�p�@�7L@�/@��@���@���@���@�z�@�I�@�(�@�1@���@��w@���@�|�@�;d@�
=@��y@��!@�E�@�@��#@��#@��@��T@���@�X@��@�V@�V@��@��9@�j@� �@�b@��m@�ƨ@���@�S�@�33@�
=@���@�v�@�^5@�=q@��@�@���@��7@�`B@�&�@��`@��9@��D@��D@�z�@�bN@�b@�ƨ@���@�K�@�@��R@���@�v�@�5?@�J@���@��#@���@��@�`B@�X@�?}@��@�V@��/@�Ĝ@��j@��9@��u@�bN@�A�@�9X@��@�"�@{ƨ@r��@k�
@d��@^{@W��@QX@J��@@  @8��@3�
@.V@(A�@$�@ �u@�@�@C�@1'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�%A�A�%A�%A�1A�bA��A��A��A��A� �A�+A�M�A�t�Aʙ�AʾwA��A�bA��A���AʾwAʡ�AʃA�bA��A���A�"�A��A��A���A�ĜA��A�hsA�oA���A��HA���A�9XA�x�A���A�O�A�  A��A��#A�/A��7A�ZA�1'A��`A�5?A�jA���A�33A��/A���A�C�A���A� �A���A���A�r�A���A�%A��RA�M�A�`BA��A���A��A��;A��HA�Q�A�-A���A��hA�\)A�+A���A�l�A��A��A�I�A���A��yA��uA�VA��A���A�VA� �A�=qA��A��A���A�v�A�VA��\A�{A���A��
A�JA���A�1A��9A�A��mA��wA��Ax�A}�^A|��A{Ax�+AwAv�At�As�#Ar{An�+Ak�Ai�Agp�Afv�AdȴAcp�Ab�Abr�Aa�Aa&�A_��A]�#A\ZAZ{AWO�AT�/AR��AP��AN�AM�wALĜAK��AIG�AHbAG�-AF�HAEXAC��AB��ABA�AA��AAXA@�HA?�wA>M�A>JA=�A=�^A=?}A<~�A;�A;7LA9/A7�A5�FA4�A3�A2  A0�uA.�9A-��A-%A,I�A+ƨA+`BA*�RA)�wA(�HA'�A'/A&bNA%%A#�-A"��A"=qA"{A"A!�A!�A!O�A ��A A`BA��A�7AVA��A|�A�uA�A�yA9XA�A?}A��A��AĜA�-A��A�A
�HA	;dA��Av�A �Ax�A9XAS�Av�A1A|�A�#AXAC�A7LA&�A �A �9A I�@��;@�$�@�bN@���@�O�@�A�@��@�5?@�w@��@��@��@�-@�7L@���@��@�I�@�t�@�@��@�;d@�v�@ᙚ@��@ܼj@�K�@�V@���@�\)@��@���@�C�@�X@��/@��m@�$�@�Ĝ@��m@���@�/@��@�-@���@�@�G�@���@�r�@���@��-@�?}@���@�(�@�1@���@��@�=q@�?}@� �@�+@���@�{@�p�@�z�@���@�"�@��\@�5?@�@���@�x�@���@��u@�1@�l�@�K�@�l�@�\)@�{@���@�hs@�?}@��@��@��j@��@�Z@�bN@�1'@��@��@�@�ȴ@�=q@�G�@��@�I�@��w@�t�@�o@���@�{@�@���@�?}@��`@��D@��
@�C�@�S�@�\)@��@��+@�%@��@��P@��@��m@�@��^@���@�1'@�K�@���@�5?@���@��#@�J@�=q@�J@���@�Ĝ@�z�@�1'@��@�|�@���@��@��^@���@�x�@�V@���@��/@���@�z�@�r�@�Z@�(�@���@��m@��@��m@��F@�l�@��@�~�@�V@�n�@�=q@�=q@�E�@�-@���@��h@�`B@�O�@��@��@�9X@�b@�  @��m@��w@���@�|�@�|�@�o@���@��\@��@�p�@�7L@�/@��@���@���@���@�z�@�I�@�(�@�1@���@��w@���@�|�@�;d@�
=@��y@��!@�E�@�@��#@��#@��@��T@���@�X@��@�V@�V@��@��9@�j@� �@�b@��m@�ƨ@���@�S�@�33@�
=@���@�v�@�^5@�=q@��@�@���@��7@�`B@�&�@��`@��9@��D@��D@�z�@�bN@�b@�ƨ@���@�K�@�@��R@���@�v�@�5?@�J@���@��#@���@��@�`B@�X@�?}@��@�V@��/@�Ĝ@��j@��9@��u@�bN@�A�@�9X@��@�"�@{ƨ@r��@k�
@d��@^{@W��@QX@J��@@  @8��@3�
@.V@(A�@$�@ �u@�@�@C�@1'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B�B�B�B�)B�;B�`B��B&�BdZB�%B��BŢB�sB�B��B��B  B  B��B��B��B��B�B�B��B1BPBuBuB{B{B�B{B{B\BJBB��BB1BJBPB�B.B/B&�B�B�B�B�B�B�B�B�BPB��B�fB�;B�TB�fB��B��B�-B��BffBT�B|�B�\Bv�B_;B]/BcTB^5BYBP�BH�B=qB33B(�BhB��B�B�/B�}B��B��B~�BjBZB<jB$�BbB
��B
�TB
�B
ǮB
�wB
�-B
��B
�B
t�B
]/B
O�B
?}B
5?B
)�B
�B
oB
	7B
B	��B	�B	�B	��B	ƨB	�LB	��B	��B	��B	�oB	�PB	�7B	�B	�B	{�B	s�B	k�B	e`B	S�B	K�B	D�B	@�B	9XB	49B	0!B	+B	#�B	�B	�B	�B	�B	bB	JB	
=B		7B	+B	B	B��B��B��B��B��B��B�B�B�fB�5B�B��B��BŢB�wB�?B�!B�B��B��B��B��B��B��B��B�bB�JB�7B�1B�B�B�B�B� B}�B{�Bx�Bt�Br�Bo�Bn�Bk�BiyBiyBgmBe`BdZB_;B[#BW
BQ�BM�BM�BN�BL�BK�BK�BJ�BJ�BH�BH�BG�BE�BD�BC�BA�B@�B@�B@�B@�B?}B>wB>wB=qB>wB?}BB�BC�BD�BC�BB�BB�B>wBB�B>wB<jB;dB:^B7LB7LB49B2-B1'B1'B1'B1'B0!B0!B49B49B49B49B49B5?B6FB7LB9XB=qBB�BH�BL�BP�BS�BM�BK�BE�BE�BI�BO�BVBYB\)BaHBdZBffBjBo�Bq�Bs�Bt�Bt�Bv�Bz�B�B�+B�DB�\B��B��B��B��B��B��B��B��B��B��B�B�B�-B�wBŢBƨBƨBȴBɺB��B��B��B��B�B�;B�BB�NB�NB�NB�BB�;B�TB�mB�B�B�B�B�B��B��B��B��B	%B	JB	VB	VB	bB	hB	oB	{B	uB	oB	{B	�B	�B	�B	�B	�B	$�B	&�B	'�B	)�B	,B	.B	1'B	6FB	8RB	7LB	7LB	8RB	:^B	9XB	:^B	<jB	=qB	?}B	?}B	@�B	D�B	E�B	G�B	J�B	L�B	M�B	N�B	P�B	T�B	\)B	_;B	`BB	`BB	aHB	bNB	ffB	k�B	p�B	u�B	y�B	{�B	|�B	~�B	� B	�B	�%B	�1B	�7B	�1B	�1B	�7B	�DB	�JB	�JB	�PB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�FB	�LB	�LB	�LB	�XB	�dB	�dB	�qB	�wB	�}B	��B	B	ÖB	ĜB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�HB	�NB	�NB	�TB	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
B
PB
{B
�B
$�B
+B
2-B
8RB
A�B
H�B
N�B
T�B
YB
^5B
bNB
hsB
l�B
o�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B�B�B�B�)B�;B�ZB��B%�BcTB�B��BĜB�sB�B��BBBB��B��BBPBJB	7BoB{B�B�B�B�B�B�B�B�B�B�BhB1B	7BVB\BbB�B7LB9XB/B#�B�B�B�B#�B%�B%�B+B�B  B�B�ZB�B�B�;B��BĜB��Bo�BR�B�B��B�BbNBaHBiyBbNB_;BT�BO�BE�BA�B9XB �B%B  B�B��B�?B��B�VB{�Bl�BP�B:^B#�B%B
�B
�TB
��B
��B
ƨB
�FB
�{B
�B
l�B
]/B
K�B
@�B
7LB
/B
�B
hB
JB
B
B
B	�yB	�
B	B	�3B	�B	��B	��B	�hB	�\B	�JB	�PB	�=B	�B	~�B	w�B	ffB	^5B	R�B	L�B	B�B	=qB	;dB	9XB	,B	$�B	$�B	$�B	 �B	�B	bB	VB	VB	PB	VB	DB	  B��B��B��B��B��B��B��B�B�B�ZB�B�
B��BɺB�jB�?B�'B�B��B��B��B��B��B��B��B��B�hB�VB�1B�B�B�B�B�B�B~�Bz�By�Bx�By�Bw�Bq�Bp�Bn�Bl�Bk�BjBjBe`B\)BT�BVBT�BR�BXBVBN�BM�BL�BO�BP�BL�BJ�BH�BG�BI�BC�BA�BA�B@�B@�BA�BA�BB�BF�BI�BI�BI�BG�BG�BI�BF�BH�BC�BB�B@�B?}B=qB>wB:^B7LB8RB6FB5?B5?B6FB:^B<jB:^B9XB:^B:^B;dB<jB>wB?}B@�BG�BO�BL�BT�BZBS�BR�BH�BG�BJ�BO�BXB]/B\)BaHBffBhsBm�Bp�Bt�Bv�Bt�Bx�B{�B~�B�B�+B�DB�uB��B��B��B��B��B��B��B��B��B��B�B�B�-B��BɺBȴBǮBɺBɺB��B��B��B��B�B�;B�NB�ZB�TB�NB�NB�;B�fB�sB�B�B�B�B�B��B��B	  B	B	1B	VB	bB	VB	hB	uB	�B	�B	�B	uB	{B	�B	�B	�B	�B	�B	'�B	(�B	)�B	+B	,B	.B	1'B	6FB	:^B	7LB	8RB	9XB	:^B	;dB	=qB	>wB	=qB	@�B	?}B	A�B	E�B	E�B	H�B	K�B	L�B	N�B	O�B	Q�B	T�B	\)B	`BB	`BB	aHB	cTB	bNB	gmB	k�B	q�B	u�B	y�B	{�B	}�B	� B	�B	�B	�+B	�=B	�=B	�7B	�7B	�=B	�DB	�PB	�JB	�VB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�FB	�LB	�RB	�RB	�RB	�^B	�jB	�jB	�wB	�wB	�}B	��B	ÖB	ĜB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�)B	�)B	�5B	�5B	�5B	�5B	�;B	�BB	�HB	�TB	�TB	�ZB	�fB	�mB	�sB	�sB	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
PB
{B
�B
$�B
,B
2-B
8RB
A�B
I�B
N�B
T�B
ZB
_;B
cTB
hsB
l�B
o�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<ě�<�h<�9X<���<D��<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<�o<u<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<T��<#�
<#�
<#�
<#�
<D��<49X<D��<�t�<�C�<#�
<#�
<#�
<u<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<�o<u<49X<�o<�C�<T��<e`B<�o<u<�C�<�t�<��
<�1<���<u<T��<#�
<#�
<T��<��
<�9X<�o<�o<u<T��<D��<49X<T��<u<#�
<#�
<#�
<49X<�C�<ě�<��
<�o<49X<#�
<49X<#�
<#�
<#�
<#�
<#�
<D��<e`B<e`B<���<�t�<�t�<�t�<e`B<49X<#�
<#�
<49X<e`B<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<49X<e`B<49X<#�
<#�
<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<u<e`B<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452192012011014521920120110145219  AO  ARGQ                                                                        20111130144256  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144256  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145219  IP                  G�O�G�O�G�O�                