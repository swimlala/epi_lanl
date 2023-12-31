CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:57Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ^    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       `   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       j$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       r8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   zL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               EA   AO  20111130144218  20190522121829  1728_5048_069                   2C  D   APEX                            2142                            040306                          846 @��6r( 1   @��7�?�@6���Q��b��x���1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @   @�  @�  A   A   AA��Aa��A�  A�  A�33A�33A�33A�33A�33A�  B   B  B  B��B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�33B�  B�  B���B���B���B���B���B���B���B�  B�  B�  B�33B�  B���B�  B�  B�  B�33B�  C   C  C  C�fC  C
  C  C  C�fC  C  C�C�fC�fC  C  C   C!�fC$  C&  C'�fC*  C,  C.  C0  C2  C4  C6�C8  C:  C<�C>  C@  CB�CD  CE�fCH  CJ  CK�fCN  CP  CR  CT�CV  CX  CY�fC\  C^�C`  Cb  Cc�fCe�fCg�fCj  Cl�Cn�Cp  Cq�fCs�fCv  Cx  Cz  C|�C~�C��C��C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C��3C��3C��3C�  C��C��C��C�  C�  C��3C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��3D y�DfD� D  D� D  D� D  D�fD  D� D  D�fDfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D��Dy�D  D� D  Dy�D  D�fD  D� D  Dy�D  D�fD  Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  D� D  D� D  Dy�D  D�fD  Dy�D   D � D ��D!� D"  D"y�D#  D#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1fD1� D1��D2� D3fD3� D4  D4�fD5  D5� D5��D6y�D7  D7� D8  D8� D9fD9�fD:  D:� D;  D;�fD<  D<y�D=  D=� D=��D>� D?  D?y�D@  D@�fDA  DA� DB  DB� DC  DC� DC��DD� DE  DE� DF  DF� DG  DG� DH  DHy�DI  DI� DI��DJ� DK  DKy�DK��DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV�fDW  DWy�DX  DX� DY  DY� DY��DZ� D[  D[y�D\  D\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh�fDi  Diy�Dj  Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDy��D���D�33D�s3D��3D�3D�&fD�� D�� D��D�0 D�Y�D�� D�  D�33D�i�D���D���D�fD�S3D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @ff@fff@�33@�33A��A;33A[33Ay��A���A�  A�  A�  A�  A�  A���A���BffBffB  BffB&ffB.  B6ffB>ffBFffBNffBVffB^ffBfffBn  BvffB~ffB�33B�33B�33B�33B�33B�33B�ffB�33B�  B�33B�ffB�33B�33B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�ffB�33B�  B�33B�33B�33B�ffB�33B�33C��C��C� C��C	��C��C��C� C��C��C�3C� C� C��C��C��C!� C#��C%��C'� C)��C+��C-��C/��C1��C3��C5�3C7��C9��C;�3C=��C?��CA�3CC��CE� CG��CI��CK� CM��CO��CQ��CS�3CU��CW��CY� C[��C]�3C_��Ca��Cc� Ce� Cg� Ci��Ck�3Cm�3Co��Cq� Cs� Cu��Cw��Cy��C{�3C}�3C�3C�ٚC���C���C���C���C���C�� C�� C�� C���C���C���C���C���C���C�ٚC�ٚC�ٚC���C�� C�� C�� C�� C���C���C���C���C���C�� C�� C�� C�� C���C�ٚC���C���C���C���C���C�� C���C���C�� C���C���C�� C���C���C���C���C�� C���C���C���C�ٚC���C���C�ٚC���C���C���C���C���C�� C�� C���C�ٚC�ٚC���C�� C���C�ٚC���C���C���C���C���C���C�ٚC���C���C���C�� C���C�ٚC���C���C�ٚC���C���C���C�� C�� C�� C���C�ٚC�ٚC�ٚC���C���C�� C���C���C�� C���C���C�� C�� C���C���C���C�� C���C���C���C���C���C���C���C���C�� C���C���C�� C���C���C���C�� D ` D ��DffD�fDffD�fDffD�fDl�D�fDffD�fDl�D��DffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD� D` D�fDffD�fD` D�fDl�D�fDffD�fD` D�fDl�D�fD` D� D` D� D` D� D` D� D` D�fDffD�fDffD�fD` D�fDl�D�fD` D�fD ffD � D!ffD!�fD"` D"�fD#ffD#� D$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*��D+ffD+�fD,ffD,�fD-ffD-�fD.l�D.�fD/ffD/�fD0ffD0��D1ffD1� D2ffD2��D3ffD3�fD4l�D4�fD5ffD5� D6` D6�fD7ffD7�fD8ffD8��D9l�D9�fD:ffD:�fD;l�D;�fD<` D<�fD=ffD=� D>ffD>�fD?` D?�fD@l�D@�fDAffDA�fDBffDB�fDCffDC� DDffDD�fDEffDE�fDFffDF�fDGffDG�fDH` DH�fDIffDI� DJffDJ�fDK` DK� DLffDL��DMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDT` DT�fDUffDU�fDVl�DV�fDW` DW�fDXffDX�fDYffDY� DZffDZ�fD[` D[�fD\l�D\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe��DfffDf�fDgffDg�fDhl�Dh�fDi` Di�fDjffDj�fDk` Dk�fDlffDl�fDmffDm�fDnffDn� DoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwffDw��Dy� D���D�&fD�ffD��fD��fD��D��3D��3D���D�#3D�L�D��3D��3D�&fD�\�D�� D���D���D�FfD�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A°!A�AhA7A�~�A�r�A�n�A�hsA�ffA�ffA�ffA�ffA�dZA�dZA�bNA�\)A�;dA�A�p�A�O�A�ĜA�XA�S�A�7LA�VA��
A��\A�Q�A��yA��PA�E�A�$�A�A��9A��A��FA��A�5?A��A�A���A�x�A�^5A�/A��mA��9A���A�x�A�dZA�I�A�5?A�(�A�
=A���A���A�x�A�;dA��+A��A�A�A�"�A�
=A��mA��9A�hsA�;dA�(�A�1A�ȴA���A�ffA���A��A�I�A�{A��+A���A��^A��DA�A�A�v�A�~�A�-A���A��A�1'A�|�A��A�ƨA���A��+A��A�5?A�E�A�;dA�C�A��!A�JA�I�A�A�A��uA���A��A�C�A��hA��7A��A�=qA���A�
=A��A�;dA�^5A��uA��A�~�A�bNA�~�A�ĜA�7LA�5?A�
=A���A���A��A��`A��
A�p�A�ZA}
=Az�Axr�Avn�Ar��Ann�Aj�RAg�Af^5AeC�Ad��AdbNAb$�A`=qA^��A\�A[
=AYl�AX(�AVQ�ATv�ASAQ�PAOt�AMoAJ�AH1'AE��AD^5AC�ABȴA@�A@$�A?"�A=oA:��A9%A8��A7��A5�A2�yA1�#A1x�A1�A0��A/�wA-�A,VA+��A*�/A*n�A)ƨA)oA(5?A&�/A%33A#��A"��A!�wA ��A   A;dA5?A�A�mA�A�TAp�A�yAbNA�A/AjA��A7LA�yAbA+A�A1AS�A�!A�
A�A1A�hA33A�A��A�A
z�A	�TA	\)A	
=A��A{At�A�A�A7LA�A��A��A�uA��AS�A $�@�ff@�7L@�"�@��@���@�S�@��\@��7@�Q�@���@��@�`B@���@��
@��H@�h@�bN@�K�@�`B@�;d@旍@�-@�/@��y@߅@�=q@ܬ@�A�@�dZ@�=q@�I�@���@թ�@�Z@�J@Ь@��
@�^5@̼j@�A�@�S�@�ȴ@ʰ!@���@�1@˥�@�+@��H@���@ȼj@�\)@Ƈ+@š�@�%@þw@\@��@���@���@�hs@��@��@�Z@��@��
@���@���@���@�\)@���@�E�@�V@��h@�%@��`@��j@��u@��j@���@���@�Z@���@�t�@�~�@�/@��m@��P@��+@�?}@��@�`B@��^@���@��@��H@�ff@�J@�@��h@�G�@��/@���@�j@���@�\)@��@�M�@��#@�V@��@�{@���@��@�j@��@��@��F@�C�@��+@�Q�@��m@��@��9@��@�~�@��@�j@�K�@�+@���@�@��@���@�C�@��@��H@���@���@��@�p�@���@�dZ@�t�@���@�dZ@���@��@���@�hs@�7L@��@�A�@� �@�1@��
@��P@�dZ@�"�@���@���@�n�@�5?@�J@��#@���@��@�x�@�p�@�hs@�G�@�&�@��@�%@���@�V@���@��@���@��u@��D@�j@�(�@�(�@�A�@� �@�b@��;@���@�"�@��@�
=@��@�+@�33@�"�@��!@�E�@��@�V@�~�@��+@�M�@���@�x�@��`@��@��9@��u@�z�@�I�@�1'@�  @���@�dZ@�;d@�33@�33@�o@��!@�M�@�E�@��@���@��#@��T@�@�x�@�&�@���@��j@�A�@��m@��@�dZ@���@���@�ff@�5?@�J@��^@�x�@�G�@�%@��@��/@��9@�z�@�A�@�(�@�1@��w@��@�C�@���@��R@�n�@�-@��T@���@�Ĝ@�&�@{dZ@t�@ihs@d1@[o@TI�@Pr�@J�@EV@>ȴ@9�@3�@.�+@%p�@ �u@dZ@  @�
@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A°!A�AhA7A�~�A�r�A�n�A�hsA�ffA�ffA�ffA�ffA�dZA�dZA�bNA�\)A�;dA�A�p�A�O�A�ĜA�XA�S�A�7LA�VA��
A��\A�Q�A��yA��PA�E�A�$�A�A��9A��A��FA��A�5?A��A�A���A�x�A�^5A�/A��mA��9A���A�x�A�dZA�I�A�5?A�(�A�
=A���A���A�x�A�;dA��+A��A�A�A�"�A�
=A��mA��9A�hsA�;dA�(�A�1A�ȴA���A�ffA���A��A�I�A�{A��+A���A��^A��DA�A�A�v�A�~�A�-A���A��A�1'A�|�A��A�ƨA���A��+A��A�5?A�E�A�;dA�C�A��!A�JA�I�A�A�A��uA���A��A�C�A��hA��7A��A�=qA���A�
=A��A�;dA�^5A��uA��A�~�A�bNA�~�A�ĜA�7LA�5?A�
=A���A���A��A��`A��
A�p�A�ZA}
=Az�Axr�Avn�Ar��Ann�Aj�RAg�Af^5AeC�Ad��AdbNAb$�A`=qA^��A\�A[
=AYl�AX(�AVQ�ATv�ASAQ�PAOt�AMoAJ�AH1'AE��AD^5AC�ABȴA@�A@$�A?"�A=oA:��A9%A8��A7��A5�A2�yA1�#A1x�A1�A0��A/�wA-�A,VA+��A*�/A*n�A)ƨA)oA(5?A&�/A%33A#��A"��A!�wA ��A   A;dA5?A�A�mA�A�TAp�A�yAbNA�A/AjA��A7LA�yAbA+A�A1AS�A�!A�
A�A1A�hA33A�A��A�A
z�A	�TA	\)A	
=A��A{At�A�A�A7LA�A��A��A�uA��AS�A $�@�ff@�7L@�"�@��@���@�S�@��\@��7@�Q�@���@��@�`B@���@��
@��H@�h@�bN@�K�@�`B@�;d@旍@�-@�/@��y@߅@�=q@ܬ@�A�@�dZ@�=q@�I�@���@թ�@�Z@�J@Ь@��
@�^5@̼j@�A�@�S�@�ȴ@ʰ!@���@�1@˥�@�+@��H@���@ȼj@�\)@Ƈ+@š�@�%@þw@\@��@���@���@�hs@��@��@�Z@��@��
@���@���@���@�\)@���@�E�@�V@��h@�%@��`@��j@��u@��j@���@���@�Z@���@�t�@�~�@�/@��m@��P@��+@�?}@��@�`B@��^@���@��@��H@�ff@�J@�@��h@�G�@��/@���@�j@���@�\)@��@�M�@��#@�V@��@�{@���@��@�j@��@��@��F@�C�@��+@�Q�@��m@��@��9@��@�~�@��@�j@�K�@�+@���@�@��@���@�C�@��@��H@���@���@��@�p�@���@�dZ@�t�@���@�dZ@���@��@���@�hs@�7L@��@�A�@� �@�1@��
@��P@�dZ@�"�@���@���@�n�@�5?@�J@��#@���@��@�x�@�p�@�hs@�G�@�&�@��@�%@���@�V@���@��@���@��u@��D@�j@�(�@�(�@�A�@� �@�b@��;@���@�"�@��@�
=@��@�+@�33@�"�@��!@�E�@��@�V@�~�@��+@�M�@���@�x�@��`@��@��9@��u@�z�@�I�@�1'@�  @���@�dZ@�;d@�33@�33@�o@��!@�M�@�E�@��@���@��#@��T@�@�x�@�&�@���@��j@�A�@��m@��@�dZ@���@���@�ff@�5?@�J@��^@�x�@�G�@�%@��@��/@��9@�z�@�A�@�(�@�1@��w@��@�C�@���@��R@�n�@�-@��T@���@�Ĝ@�&�@{dZ@t�@ihs@d1@[o@TI�@Pr�@J�@EV@>ȴ@9�@3�@.�+@%p�@ �u@dZ@  @�
@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBD�BC�BF�BC�BC�BC�BF�BG�BH�BI�BJ�BL�BM�BO�BQ�BT�BZB_;B_;BR�B49B?}BE�BK�BM�BM�BO�BR�BS�BYB^5BcTBe`BhsBjBq�Bx�B�B}�B~�B�%B�PB��B��B��B�B�'B�-B�?B�FB�XBBƨB��B��B��B��B��BŢB�LB�RB�dB�jB�qB��BǮB��B��B��B��B��B�B�B�B�#B�#B�5B�#B�B�B�#B�B��B��B�fB�5B��BŢB�'B��B�BXB;dB(�B�BJBDBB��B�B�ZB�TB�BB�B��BȴB�jB�-B�B��B��B�=By�BhsBYBE�B.B�B
��B
�B
��B
�jB
�B
��B
�=B
z�B
e`B
Q�B
E�B
49B
VB	��B	�NB	��B	�-B	��B	{�B	cTB	\)B	S�B	N�B	J�B	:^B	/B	&�B	�B	PB	B��B�B�sB�;B��BƨB�qB�3B�B��B��B��B��B��B�bB�JB�=B�B}�B{�Bv�Bq�Bo�Bn�Bm�Bk�BiyBffBffBjBiyBhsBgmBe`BcTBdZBaHB^5B^5B^5B]/B]/B]/B\)B]/B\)B^5B_;B_;B_;B^5B]/B]/B`BB]/B_;B^5B]/B]/B]/B\)B\)B^5B_;B\)B\)B]/B\)B[#BZBYBYBYBXBW
BVBT�BT�BR�BQ�BP�BO�BN�BL�BK�BL�BK�BK�BL�BN�BM�BO�BR�BS�BS�BVBW
BXBYBZBYBYBYBXBW
BW
BVBR�BR�BQ�BP�BN�BI�BD�BF�BF�BG�BE�BC�BB�B@�B?}B>wB=qB?}B@�BA�BC�BC�BF�BI�BM�BXBl�Bv�B{�B|�B�B�1B�DB�PB�\B�\B�hB��B��B��B��B��B��B��B�B�!B�-B�FB�qBBǮB��B��B��B��B��B�B�B�#B�5B�BB�TB�`B�fB�mB�mB�ZB�TB�ZB�TB�ZB�fB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	�B	�B	�B	$�B	'�B	(�B	+B	$�B	'�B	-B	49B	33B	0!B	.B	,B	,B	0!B	2-B	1'B	.B	-B	-B	.B	1'B	49B	49B	8RB	=qB	;dB	;dB	>wB	A�B	A�B	@�B	?}B	A�B	B�B	C�B	G�B	I�B	J�B	K�B	M�B	N�B	O�B	Q�B	S�B	VB	XB	ZB	^5B	aHB	dZB	ffB	gmB	hsB	iyB	n�B	p�B	s�B	v�B	w�B	z�B	|�B	~�B	� B	�B	�B	�+B	�1B	�7B	�VB	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�FB	�FB	�FB	�LB	�XB	�XB	�XB	�XB	�XB	�XB	�^B	�dB	�qB	�qB	�wB	��B	ÖB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�mB	�sB	�B	�B	�B	�B	��B
B
VB
�B
�B
'�B
/B
49B
8RB
<jB
D�B
J�B
O�B
VB
^5B
bNB
gmB
jB
o�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BF�BD�BF�BC�BD�BD�BF�BG�BH�BI�BJ�BL�BM�BO�BQ�BVB\)BdZBjBm�BB�BC�BF�BM�BO�BP�BR�BVBYB]/BaHBe`BgmBl�Bp�Bu�B{�B�B�B�B�1B�VB��B��B��B�B�-B�9B�FB�LB�^BÖBȴB��B��B�B�B�)B��B�^B�^B�qB�wB��BĜBɺB��B��B��B�B�B�)B�/B�/B�;B�NB�ZB�;B�/B�;B�fB�ZB�B�#B�B�fB�5B��B�}B�B��BiyBG�B6FB'�B�B{BVB%B��B�B�B�B�TB�B�BŢB�jB�9B�B��B��B�+Bu�BhsBVB?}B1'BoB
�`B
�#B
��B
�dB
��B
��B
�JB
s�B
^5B
\)B
VB
 �B
+B	��B	�sB	��B	�3B	�\B	k�B	cTB	YB	W
B	[#B	H�B	<jB	5?B	$�B	�B	VB	+B��B��B�B�ZB�B��BB�}B�'B��B��B��B��B��B��B��B�JB�B�%B�+B� Bv�Bq�Bq�Bq�Bq�Bs�Bp�Bp�Bo�Bl�Bm�Bk�Bk�Bo�Bl�BhsBhsBe`Be`BcTBdZBe`BgmBdZBgmBdZBcTBcTBbNBbNBcTBffBcTBbNBbNBcTBdZBbNBaHBbNBdZBffBcTBbNBaHB`BBaHB`BB_;B^5B^5B\)BZBZB[#BZBXBYBW
BR�BR�BS�BS�BR�BQ�BS�BS�BT�BVBW
BW
BW
BXB[#B\)B]/B]/B]/B\)B]/B^5B]/B\)B]/B]/BZBR�BQ�BW
BXBT�BI�BK�BI�BG�BK�BJ�BG�B@�B?}BF�BB�BC�BF�BF�BE�BF�BH�BI�BL�BVBn�Bx�B}�B�B�B�1B�VB�hB�uB�\B��B��B��B��B��B��B��B��B�B�!B�-B�FB�qBBǮB��B��B��B�
B�B�
B�B�#B�5B�HB�TB�mB�fB�B�B�ZB�`B�sB�TB�`B�fB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	  B	  B	B��B��B	B	�B	�B	�B	%�B	'�B	-B	+B	%�B	'�B	-B	7LB	33B	49B	2-B	,B	,B	2-B	5?B	5?B	1'B	-B	-B	/B	2-B	7LB	49B	:^B	=qB	=qB	;dB	>wB	C�B	D�B	@�B	@�B	A�B	C�B	E�B	H�B	J�B	K�B	L�B	M�B	O�B	P�B	R�B	T�B	W
B	YB	[#B	_;B	aHB	e`B	ffB	gmB	hsB	jB	n�B	p�B	s�B	v�B	w�B	{�B	}�B	� B	�B	�B	�%B	�1B	�1B	�7B	�\B	�oB	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�B	�-B	�LB	�LB	�LB	�RB	�^B	�^B	�^B	�^B	�^B	�XB	�^B	�jB	�wB	�wB	�wB	��B	ĜB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�/B	�;B	�BB	�HB	�HB	�NB	�TB	�ZB	�`B	�fB	�fB	�sB	�yB	�B	�B	�B	�B	��B
B
VB
�B
�B
'�B
/B
49B
8RB
<jB
D�B
J�B
P�B
VB
^5B
bNB
gmB
k�B
o�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<���<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<49X<#�
<#�
<49X<e`B<�C�<���<�C�<D��<T��<T��<49X<#�
<#�
<49X<D��<#�
<#�
<49X<49X<#�
<T��<#�
<#�
<#�
<#�
<T��<T��<D��<T��<u<u<�C�<���<��
<D��<u<�C�<�o<e`B<T��<�o<e`B<49X<�9X=+<�C�<�C�<��
<�`B<�h<ě�<�t�<#�
<#�
<#�
<#�
<�o<e`B<T��<e`B<T��<T��<49X<e`B<T��<D��<T��<�C�<���<��
<u<�o<#�
<#�
<#�
<49X<#�
<#�
<e`B<e`B<49X<#�
<#�
<�o<T��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452172012011014521720120110145217  AO  ARGQ                                                                        20111130144218  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144218  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145217  IP                  G�O�G�O�G�O�                