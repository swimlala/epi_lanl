CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:36Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               ,A   AO  20111205113058  20190522121836  1901_5055_044                   2C  D   APEX                            2140                            040306                          846 @Ԗ�+���1   @Ԗ����@.����+�cƧ1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @�  A   AffA>ffA`  A�  A�  A�  A�33A�33A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  BpffBxffB�33B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�33C �C  C  C�fC�fC
  C�C�C�C  C  C  C  C�C  C  C �C"  C#�fC&�C(  C*  C,  C.  C0  C2�C4�C6  C8  C:  C<�C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CO�fCR  CT�CV�CX  CZ  C\  C]�fC_�fCb  Cd�Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C��C��C��C��C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C��C�  C��3C�  C��C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C��C�  C��3C�  C��C��C��C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C��C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C��C�  C��3C�  C��D   D � D  D� D��D� D  D� D  D� D��D� D  Dy�D��Dy�D��Dy�D��D	y�D
  D
�fD  D� D  D� D��Dy�D��D� DfD�fD  D� D��Dy�D��Dy�D��Dy�D  D� D  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D   D � D!  D!�fD"  D"� D#fD#� D$  D$y�D$��D%� D&  D&� D'  D'� D(fD(� D(��D)� D*fD*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2fD2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;�fD<  D<y�D=  D=� D>fD>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DS��DTy�DU  DU�fDVfDV�fDWfDW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_fD_� D`  D`� Da  Day�Da��Db� DcfDc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dl  Dl�fDmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy� D��D�6fD�ffD���D�fD�FfD�ffD���D��3D�#3D�	�DǶfD��3D�  D�ffD�3D��fD�#3D�Y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@fff@�33@�33A  A8  AY��Ay��A���A���A�  A�  A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBF��BNffBVffB^ffBfffBn��Bv��B~��B�33B�  B�33B�  B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�  B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�  B�33B�ffB�ffC��C��C� C� C	��C�3C�3C�3C��C��C��C��C�3C��C��C�3C!��C#� C%�3C'��C)��C+��C-��C/��C1�3C3�3C5��C7��C9��C;�3C=��C?��CA��CC��CE� CG��CI��CK��CM��CO� CQ��CS�3CU�3CW��CY��C[��C]� C_� Ca��Cc�3Ce�3Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C�3C�ٚC�ٚC�ٚC�ٚC���C���C���C�� C���C�ٚC���C���C���C���C�� C�� C�� C�� C�� C���C�ٚC���C�� C���C�ٚC���C���C�� C�� C�� C�� C�� C�� C�� C�� C���C�ٚC���C���C���C���C���C���C�ٚC���C�ٚC���C���C���C�� C���C���C�� C���C���C���C���C�ٚC���C���C�ٚC���C�� C���C�ٚC�ٚC�ٚC���C���C���C�� C���C���C���C�ٚC���C�� C���C���C���C�ٚC�� C�� C���C���C���C���C���C���C���C�� C���C���C�� C���C�ٚC���C���C���C�ٚC���C�� C���C���C���C���C�ٚC�ٚC�ٚC���C���C���C���C���C���C�� C���C�ٚC���C�� C���C���C�ٚC���C�� C���C�ٚC���D ffD �fDffD� DffD�fDffD�fDffD� DffD�fD` D� D` D� D` D� D	` D	�fD
l�D
�fDffD�fDffD� D` D� DffD��Dl�D�fDffD� D` D� D` D� D` D�fDffD�fD` D�fDl�D�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD` D�fD ffD �fD!l�D!�fD"ffD"��D#ffD#�fD$` D$� D%ffD%�fD&ffD&�fD'ffD'��D(ffD(� D)ffD)��D*ffD*�fD+ffD+�fD,` D,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1l�D1��D2ffD2��D3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;l�D;�fD<` D<�fD=ffD=��D>ffD>� D?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDI` DI�fDJffDJ�fDKffDK�fDLl�DL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQl�DQ�fDRffDR�fDSffDS� DT` DT�fDUl�DU��DVl�DV��DWffDW�fDXffDX��DYffDY�fDZffDZ�fD[ffD[�fD\ffD\��D]ffD]�fD^ffD^��D_ffD_�fD`ffD`�fDa` Da� DbffDb��Dcl�Dc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDk` Dk�fDll�Dl��DmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwffDy�fD�  D�)�D�Y�D���D���D�9�D�Y�D�� D��fD�fD���Dǩ�D��fD�3D�Y�D�fD�ٚD�fD�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A׬Aװ!A״9A׺^A׺^A׺^A׺^A׼jA׼jA׼jA׼jA׼jA׾wA���A׾wA���A���A�A�A�A�ĜA�ƨA�ȴA�ȴA�ȴA�ƨA�ƨA�ƨA�ƨA�C�Aк^A�9XA���AɁAȁA��A�hsA�S�A�S�A�ƨA�9XA�VA�O�A��FA���A�A�A��mA��7A���A��
A��A��RA�E�A���A�I�A��`A�(�A�;dA���A�ȴA�{A�E�A���A�-A�=qA�+A�ȴA��DA�|�A�t�A�/A��A�"�A���A��\A���A�p�A�hsA�XA�"�A���A�A�\)A�JA�t�A�hsA���A�
=A�{A�\)A��A���A�r�A�1A��uA�S�A���A~��Az$�Ax��Ax-Au��Aq��Ao�Ai�;Ae��Abz�A^  AZ�`AT��AQ�AP1AMAL�DAK�7AHr�ACƨA@A�A=�^A:�jA:1'A9�A7�A5�A4I�A3hsA0A�A/�
A.{A,��A, �A*��A*jA*ĜA+�-A,��A-��A.�yA0��A0��A/|�A/K�A.jA.9XA-��A-
=A,�jA,ffA+�7A*��A*��A*-A)��A(�HA'?}A%��A%\)A&ffA%�TA%��A%��A%"�A$�RA$I�A#�^A#l�A"ffAƨA�AO�A �A�HAM�AVA�+AjA��A�AQ�AK�AjA��AoA?}A�jAVA��A�AȴA��AI�A �A�A1A��AjAG�A;dA"�A�A�RAr�A�wAK�A%A
��A
JA	�
A	�^A	�PA	&�AȴA�RA��A��A�AdZA��A�9A�AVA�An�Ap�AoA��A �A�TA\)AVA ȴA �!A M�@���@���@��+@��h@���@��@��P@�
=@�;d@�$�@��@���@�K�@�@���@��@�V@��@�bN@�ƨ@���@�G�@�Ĝ@���@���@�=q@�h@�j@���@�~�@���@��;@�33@���@���@�&�@��`@�Q�@��@�l�@���@��T@��#@��@�`B@��D@�1@�V@ݡ�@�/@��@ܬ@ܓu@� �@�|�@�o@��y@��H@�J@٩�@ٙ�@�x�@�%@ؼj@�z�@�A�@׍P@�"�@�~�@��@�;d@��y@�n�@��@թ�@�%@���@�z�@�(�@Ӯ@ӝ�@�o@�V@���@�hs@���@�bN@��
@�\)@���@��@ͺ^@�7L@̋D@�1@ˮ@�33@ʗ�@�=q@Ɂ@��m@�n�@�M�@���@��@��;@�ȴ@�~�@�5?@�@�G�@���@��@�Q�@�(�@��F@�\)@�"�@���@�{@��T@�/@��@��@���@�"�@�@���@�V@�-@�x�@�&�@���@��@�A�@��@��m@���@��;@��m@���@��w@�K�@��@�/@��D@�1'@�1@��@���@�E�@���@�`B@�/@��@��j@�j@�Q�@� �@��P@�dZ@�l�@�K�@�ȴ@�5?@���@�hs@��@��m@��P@�K�@��R@��!@�n�@�E�@���@���@�hs@�G�@�V@��`@��@�1@�|�@�"�@��@���@�v�@�-@�J@���@�&�@�bN@�  @��@��\@���@�^5@�5?@��T@�x�@��@��j@�bN@��@��;@�ƨ@��P@�l�@���@��@�ȴ@��!@��\@�^5@��T@���@�p�@��@��@���@��@�(�@��@���@�K�@�o@���@�M�@�{@���@��-@�`B@�&�@��`@���@�j@�A�@�(�@�1@��;@���@�l�@�o@���@�~�@�=q@���@�/@���@�Z@��;@��w@��@��@��@���@�n�@�=q@�J@�@�x�@��w@��7@��@}�h@st�@j-@bn�@Z-@Q&�@H��@AG�@;"�@4z�@/\)@)��@#��@��@Ĝ@ƨ@b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A׬Aװ!A״9A׺^A׺^A׺^A׺^A׼jA׼jA׼jA׼jA׼jA׾wA���A׾wA���A���A�A�A�A�ĜA�ƨA�ȴA�ȴA�ȴA�ƨA�ƨA�ƨA�ƨA�C�Aк^A�9XA���AɁAȁA��A�hsA�S�A�S�A�ƨA�9XA�VA�O�A��FA���A�A�A��mA��7A���A��
A��A��RA�E�A���A�I�A��`A�(�A�;dA���A�ȴA�{A�E�A���A�-A�=qA�+A�ȴA��DA�|�A�t�A�/A��A�"�A���A��\A���A�p�A�hsA�XA�"�A���A�A�\)A�JA�t�A�hsA���A�
=A�{A�\)A��A���A�r�A�1A��uA�S�A���A~��Az$�Ax��Ax-Au��Aq��Ao�Ai�;Ae��Abz�A^  AZ�`AT��AQ�AP1AMAL�DAK�7AHr�ACƨA@A�A=�^A:�jA:1'A9�A7�A5�A4I�A3hsA0A�A/�
A.{A,��A, �A*��A*jA*ĜA+�-A,��A-��A.�yA0��A0��A/|�A/K�A.jA.9XA-��A-
=A,�jA,ffA+�7A*��A*��A*-A)��A(�HA'?}A%��A%\)A&ffA%�TA%��A%��A%"�A$�RA$I�A#�^A#l�A"ffAƨA�AO�A �A�HAM�AVA�+AjA��A�AQ�AK�AjA��AoA?}A�jAVA��A�AȴA��AI�A �A�A1A��AjAG�A;dA"�A�A�RAr�A�wAK�A%A
��A
JA	�
A	�^A	�PA	&�AȴA�RA��A��A�AdZA��A�9A�AVA�An�Ap�AoA��A �A�TA\)AVA ȴA �!A M�@���@���@��+@��h@���@��@��P@�
=@�;d@�$�@��@���@�K�@�@���@��@�V@��@�bN@�ƨ@���@�G�@�Ĝ@���@���@�=q@�h@�j@���@�~�@���@��;@�33@���@���@�&�@��`@�Q�@��@�l�@���@��T@��#@��@�`B@��D@�1@�V@ݡ�@�/@��@ܬ@ܓu@� �@�|�@�o@��y@��H@�J@٩�@ٙ�@�x�@�%@ؼj@�z�@�A�@׍P@�"�@�~�@��@�;d@��y@�n�@��@թ�@�%@���@�z�@�(�@Ӯ@ӝ�@�o@�V@���@�hs@���@�bN@��
@�\)@���@��@ͺ^@�7L@̋D@�1@ˮ@�33@ʗ�@�=q@Ɂ@��m@�n�@�M�@���@��@��;@�ȴ@�~�@�5?@�@�G�@���@��@�Q�@�(�@��F@�\)@�"�@���@�{@��T@�/@��@��@���@�"�@�@���@�V@�-@�x�@�&�@���@��@�A�@��@��m@���@��;@��m@���@��w@�K�@��@�/@��D@�1'@�1@��@���@�E�@���@�`B@�/@��@��j@�j@�Q�@� �@��P@�dZ@�l�@�K�@�ȴ@�5?@���@�hs@��@��m@��P@�K�@��R@��!@�n�@�E�@���@���@�hs@�G�@�V@��`@��@�1@�|�@�"�@��@���@�v�@�-@�J@���@�&�@�bN@�  @��@��\@���@�^5@�5?@��T@�x�@��@��j@�bN@��@��;@�ƨ@��P@�l�@���@��@�ȴ@��!@��\@�^5@��T@���@�p�@��@��@���@��@�(�@��@���@�K�@�o@���@�M�@�{@���@��-@�`B@�&�@��`@���@�j@�A�@�(�@�1@��;@���@�l�@�o@���@�~�@�=q@���@�/@���@�Z@��;@��w@��@��@��@���@�n�@�=q@�J@�@�x�@��w@��7@��@}�h@st�@j-@bn�@Z-@Q&�@H��@AG�@;"�@4z�@/\)@)��@#��@��@Ĝ@ƨ@b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BM�BN�BN�BM�BM�BM�BM�BM�BM�BL�BK�BI�BG�B@�B �B
�#B
�'B
��B
�hB
�DB
�PB
��B
��B
��B
�qB�B=qBJ�B\)B��B��B�BB�B2-BN�B_;B]/BdZBjBffBdZBe`Bq�Bw�B~�B~�B� B}�Bs�Bl�Bp�Bl�BcTBXBL�BJ�BM�B@�B0!B!�B\B��B�B��B�RB�'B��By�BS�B5?B#�B1B
�B
�B
��B
�B
�\B
p�B
7LB
�B	�B	��B	�wB	�?B	�B	��B	z�B	cTB	F�B	1'B	�B	B�B�/B��B��BĜB�}B�LB�B��B�B�3B��B��B��B�B�B��B��B�)B�B��B��B	B	bB	&�B	R�B	n�B	�VB	�B	��B	��B	��B
+B
hB
oB
�B
"�B
'�B
33B
49B
8RB
9XB
8RB
7LB
7LB
0!B
"�B
�B
�B
.B
1'B
8RB
=qB
<jB
:^B
7LB
7LB
7LB
,B
oB	��B	�NB	��B
	7B
B
B
%B
1B	��B	�sB	��B	��B	ɺB	�B	�BB	�ZB	�mB	�B	�B	�yB	�B	��B	��B	��B	��B
B
B	��B	��B	��B	��B	��B	��B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
1B
B
+B
+B
%B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�`B	�;B	�BB	�BB	�BB	�NB	�NB	�ZB	�`B	�fB	�fB	�`B	�ZB	�ZB	�sB	�B	�B	�B	�yB	�mB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
1B
	7B
	7B
	7B
1B
1B
1B
1B
1B
1B
	7B
1B
	7B
	7B

=B

=B
	7B

=B
	7B
1B
DB
JB
PB
PB
PB
PB
PB
VB
VB
\B
VB
\B
\B
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
hB
oB
uB
{B
{B
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
(�B
-B
33B
8RB
=qB
B�B
H�B
N�B
S�B
YB
^5B
cTB
ffB
jB
o�B
s�B
w�B
{�B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BM�BN�BN�BM�BM�BM�BM�BM�BM�BL�BK�BJ�BK�BR�BM�B
��B
ĜB
��B
��B
�bB
�uB
��B
�?B
�XB
��B�BF�BR�BhsB��B�5B��B	7B"�B9XBS�BdZBbNBk�Bp�Bo�Bp�Bt�B{�B�B�1B�%B�+B�7B~�Bq�Bv�Bx�Bo�Be`BS�BS�B^5BO�B<jB0!B�BVB��B�;BɺBƨB�dB��BjBC�B:^B�B
��B
�yB
�mB
ÖB
��B
��B
O�B
=qB
{B	�)B	ǮB	�}B	B	�9B	��B	�+B	cTB	K�B	=qB	!�B	�B�B�HB�#B��B��B��B��B��B��BŢB��B�/B�TB�`B�BB�)B�mB�TB	  B	B	  B	PB	bB	"�B	J�B	e`B	�1B	��B	��B	��B
%B

=B
�B
�B
�B
'�B
+B
7LB
:^B
<jB
=qB
<jB
<jB
?}B
<jB
,B
�B
�B
1'B
2-B
;dB
A�B
@�B
>wB
<jB
=qB
C�B
?}B
&�B
1B	�/B	��B
JB
B
B

=B
oB
\B	��B	�B	��B	ǮB	�
B	�BB	�mB	�sB	��B	��B	�B	�B	��B	��B	��B
  B
+B

=B	��B	��B	��B	��B	��B
B
%B
B
B
  B
  B
  B	��B
  B
B
B
  B
B
	7B
VB
B

=B
	7B
1B
+B
+B
	7B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�TB	�TB	�BB	�TB	�`B	�NB	�ZB	�`B	�fB	�fB	�`B	�ZB	�`B	�sB	�B	�B	�B	�B	�mB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B
  B
  B
  B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
1B
+B
	7B

=B

=B

=B
	7B

=B

=B
	7B
	7B
1B

=B
	7B

=B
DB

=B

=B

=B

=B
	7B
1B
DB
PB
VB
VB
PB
VB
VB
\B
VB
bB
\B
bB
\B
hB
bB
oB
hB
oB
oB
oB
uB
oB
oB
uB
oB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
�B
!�B
)�B
.B
49B
8RB
=qB
B�B
H�B
N�B
S�B
YB
^5B
cTB
ffB
jB
o�B
t�B
w�B
{�B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�=49X<�`B<���<T��<#�
<#�
<#�
<49X<�t�<u<��
<#�
<#�
<#�
<D��<e`B<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<u<#�
<#�
<#�
<#�
<#�
<49X<49X<#�
<#�
<D��<D��<T��<#�
<#�
<u<e`B<D��<e`B<T��<u<u<�o<�C�<�1<�h<��<�9X<e`B<�1<�9X<49X<u<���<�1<���='�<ě�=t�=�w<���<#�
<#�
<�1<ě�<���=\)<�/<���<��<�`B=t�<��
<u<u<#�
<e`B<���=o<ě�<���<�C�<#�
<49X<e`B<D��<#�
<49X<�t�<#�
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
<D��<���<��
<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250222012011312502220120113125022  AO  ARGQ                                                                        20111205113058  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113058  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125023  IP                  G�O�G�O�G�O�                