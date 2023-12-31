CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:54Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               8A   AO  20111130144054  20190522121829  1728_5048_056                   2C  D   APEX                            2142                            040306                          846 @Ե�4���1   @Ե��Q�@5��/���c333331   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A��A   A>ffA^ffA~ffA�  A�  A�  A�  A�  A�33A�33B   B  B  B��B��B'��B/��B7��B@  BHffBPffBX  B_��Bg��Bo��Bx  B�33B�  B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B���B�  B�  B�33B�  B���B�  B�  B�  B���B�  C �C�C  C�fC  C
�C  C  C  C�C�C  C  C�C  C  C   C"  C$  C%�fC'�fC)�fC+�fC.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CA�fCD  CF�CH  CJ  CK�fCM�fCO�fCR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf�Ch�Cj�Cl�Cn�Cp  Cq�fCt  Cv�Cx�Cz�C|�C~�C��C��C��C��C�  C��3C�  C�  C��C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C��3C��3C�  C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C��3C�  C��C��C��C��C��C�  C��3C�  C�  C�  C��C��C�  C��3C�  C��C�  C�  C��3C�  C��C�  C��3C�  C�  C��3C��3C�  C�  C��C��3C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��C�  C�  C�  C��3C��C�  C�  C�  C��3C�  C��C��D fD �fD  D� D  Dy�D��D� D  D�fD  Dy�D��D� D  D� D��D� D	  D	y�D	��D
y�D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  Dy�D  D�fDfD�fDfD�fDfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD�fDfD�fD fD �fD!fD!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&y�D'  D'� D(  D(� D)  D)y�D)��D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D1  D1� D2fD2�fD3fD3�fD4fD4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9�fD:fD:�fD;fD;�fD<  D<� D=fD=� D>  D>� D?  D?� D@  D@�fDA  DAy�DA��DB� DC  DC� DDfDD� DE  DE� DF  DF� DF��DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU�fDV  DVy�DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\  D\y�D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh� Dh��Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dy��D�	�D�@ D��D��3D��fD�P D�s3D�� D�  D�<�D�l�Dǳ3D��D�3D�s3D๚D�� D�  D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @l��@�33@�ffA��A8  AX  Ax  A���A���A���A���A���A�  A�  A���BffBffB  B  B&  B.  B6  B>ffBF��BN��BVffB^  Bf  Bn  BvffB~��B�33B�33B�33B�ffB�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�  B�  B�33B�33B�ffB�33B�  B�33B�33B�33B�  B�33B�ffC�3C��C� C��C	�3C��C��C��C�3C�3C��C��C�3C��C��C��C!��C#��C%� C'� C)� C+� C-��C/��C1��C3��C5��C7��C9��C;� C=��C?��CA� CC��CE�3CG��CI��CK� CM� CO� CQ��CS�3CU��CW��CY��C[��C]��C_��Ca��Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co��Cq� Cs��Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC���C�� C���C���C�ٚC���C�� C�� C���C�ٚC���C���C���C���C���C���C�ٚC���C�� C���C�ٚC���C�� C�� C���C�� C�� C���C�ٚC�ٚC���C���C���C���C���C���C�� C�� C���C�ٚC�ٚC���C���C�� C���C�ٚC�ٚC�ٚC�ٚC�ٚC���C�� C���C���C���C�ٚC�ٚC���C�� C���C�ٚC���C���C�� C���C�ٚC���C�� C���C���C�� C�� C���C���C�ٚC�� C���C���C�� C���C���C�� C���C���C�� C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C�ٚC���C�� C���C���C���C�� C���C���C���C�ٚC���C���C�ٚC���C���C���C�� C�ٚC���C���C���C�� C���C�ٚC�ٚC�ٚD l�D �fDffD�fD` D� DffD�fDl�D�fD` D� DffD�fDffD� DffD�fD	` D	� D
` D
�fDffD�fDffD�fDffD� DffD�fDffD�fDffD�fDffD�fD` D�fDl�D��Dl�D��Dl�D��Dl�D�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDl�D��Dl�D��Dl�D��D l�D ��D!ffD!�fD"ffD"�fD#ffD#��D$ffD$�fD%ffD%�fD&` D&�fD'ffD'�fD(ffD(�fD)` D)� D*ffD*�fD+ffD+�fD,ffD,�fD-ffD-��D.ffD.�fD/ffD/�fD0ffD0�fD1ffD1��D2l�D2��D3l�D3��D4ffD4�fD5l�D5�fD6ffD6�fD7ffD7�fD8ffD8�fD9l�D9��D:l�D:��D;l�D;�fD<ffD<��D=ffD=�fD>ffD>�fD?ffD?�fD@l�D@�fDA` DA� DBffDB�fDCffDC��DDffDD�fDEffDE�fDFffDF� DGffDG�fDHffDH�fDIffDI��DJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN� DO` DO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT��DUl�DU�fDV` DV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[l�D[�fD\` D\� D]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg��DhffDh� Di` Di�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDw` Dy�3D���D�33D�  D��fD��D�C3D�ffD��3D��3D�0 D�` DǦfD���D�fD�ffD��D��3D�3D�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��!A��!A��-A��!A���A���A���A���A���A��\A��7A�~�A�|�A�v�A�r�A�p�A�n�A�l�A�jA�l�A�n�A�z�A���A��FA��
A��HA��TA��HA���A���A��RA���A�z�A�A�A�"�A��A�l�A�|�A��A���A��A���A�\)A�t�A��A��A��`A�%A�`BA��TA���A�I�A��A�1'A�jA���A���A�O�A�|�A���A�ĜA�A���A�K�A�bA�;dA�ffA��A�ZA��A�bA�XA��DA�A�A��9A��A�^5A���A�XA��FA��`A��A���A��;A��A��A��A�ZA��#A��A��A���A���A��7A��uA�hsA�wA}�-A|�Ay��Aw�FAuhsAsAp-AnI�Am;dAl�Ak��Ai\)Af(�AdZAbZA_��A\�A[?}AX9XAU�;AU�hAUK�AT��AS�AO��AOANn�AM�wAL5?AJ��AHJAFȴAF{AE�AD��AC�AC&�AA�A@�`A@{A?oA>Q�A>A=��A<�jA:�DA9l�A8��A7��A6Q�A41'A2�9A1�-A1"�A0ZA/��A/?}A.bNA,��A*ĜA)�#A)+A(�9A((�A(JA'��A'hsA'A&r�A&=qA&{A%VA#��A"bNA!`BA�wAZAl�AbNA��A�A�A5?AA�wA�-A|�A~�A��A%AVAI�AA�A��A�A��A\)AE�A7LAr�AƨA%A(�A�\A/A
�+A
1A	+A�FAĜA��A�`A��A7LA�\A��A ��@�\)@�X@�1'@���@�hs@��P@�@��m@��@�l�@��#@�j@�-@�(�@旍@�?}@���@��m@�@���@��
@��@�@�z�@�{@��@�@Դ9@�  @�l�@ҸR@ёh@�S�@���@�%@˶F@���@ʇ+@ʇ+@ɺ^@�Q�@�dZ@Ƨ�@�@�hs@��`@�1'@�dZ@�n�@���@��@��@�o@���@�G�@��@�\)@���@���@��-@��@�&�@��F@�?}@��@��#@�%@��/@���@��@���@�A�@��P@���@�@���@�dZ@���@�l�@��y@�J@�/@��`@��9@��u@�V@���@��D@�Q�@��;@�33@�ff@�5?@���@��!@��!@���@��+@�V@��T@���@�1'@�S�@��@���@�E�@��^@�`B@�G�@�&�@���@�I�@���@��@�o@��!@�ff@�$�@�O�@���@� �@��w@�"�@���@���@�K�@�ƨ@�9X@�A�@�Q�@�Z@�j@�j@��w@�C�@���@��R@��R@��R@�n�@��T@���@��7@��-@��#@���@�O�@�/@��@�V@�%@��`@��j@��u@�bN@� �@�b@��@�
=@��+@�^5@�E�@�5?@�{@���@��-@�hs@�7L@�&�@�%@��/@��u@��D@�bN@�b@��m@���@���@�;d@��\@�-@��@�@�p�@�7L@�%@���@��j@��D@�Z@�  @��m@��@��P@�|�@�t�@�S�@�@���@���@�5?@��^@�G�@��@��9@��u@��@�j@�9X@� �@��;@�t�@�+@�
=@�ȴ@���@�~�@�M�@��@���@��@��@��@��@��-@�`B@�/@��/@��u@�r�@�Z@�9X@�9X@�1@�  @��m@�ƨ@���@�S�@�@���@�5?@��T@��@�O�@�7L@���@���@��9@��u@�z�@�bN@�A�@�b@��w@���@�t�@�C�@��@���@��@��@��y@��@�ȴ@��R@���@��\@�ff@�=q@�$�@���@�@�p�@�O�@��@�Ĝ@���@��D@�z�@��@v@lj@bM�@[dZ@T�j@NV@J=q@D(�@<1@5��@0bN@+ƨ@%��@ A�@j@l�@�@ �@z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��!A��!A��-A��!A���A���A���A���A���A��\A��7A�~�A�|�A�v�A�r�A�p�A�n�A�l�A�jA�l�A�n�A�z�A���A��FA��
A��HA��TA��HA���A���A��RA���A�z�A�A�A�"�A��A�l�A�|�A��A���A��A���A�\)A�t�A��A��A��`A�%A�`BA��TA���A�I�A��A�1'A�jA���A���A�O�A�|�A���A�ĜA�A���A�K�A�bA�;dA�ffA��A�ZA��A�bA�XA��DA�A�A��9A��A�^5A���A�XA��FA��`A��A���A��;A��A��A��A�ZA��#A��A��A���A���A��7A��uA�hsA�wA}�-A|�Ay��Aw�FAuhsAsAp-AnI�Am;dAl�Ak��Ai\)Af(�AdZAbZA_��A\�A[?}AX9XAU�;AU�hAUK�AT��AS�AO��AOANn�AM�wAL5?AJ��AHJAFȴAF{AE�AD��AC�AC&�AA�A@�`A@{A?oA>Q�A>A=��A<�jA:�DA9l�A8��A7��A6Q�A41'A2�9A1�-A1"�A0ZA/��A/?}A.bNA,��A*ĜA)�#A)+A(�9A((�A(JA'��A'hsA'A&r�A&=qA&{A%VA#��A"bNA!`BA�wAZAl�AbNA��A�A�A5?AA�wA�-A|�A~�A��A%AVAI�AA�A��A�A��A\)AE�A7LAr�AƨA%A(�A�\A/A
�+A
1A	+A�FAĜA��A�`A��A7LA�\A��A ��@�\)@�X@�1'@���@�hs@��P@�@��m@��@�l�@��#@�j@�-@�(�@旍@�?}@���@��m@�@���@��
@��@�@�z�@�{@��@�@Դ9@�  @�l�@ҸR@ёh@�S�@���@�%@˶F@���@ʇ+@ʇ+@ɺ^@�Q�@�dZ@Ƨ�@�@�hs@��`@�1'@�dZ@�n�@���@��@��@�o@���@�G�@��@�\)@���@���@��-@��@�&�@��F@�?}@��@��#@�%@��/@���@��@���@�A�@��P@���@�@���@�dZ@���@�l�@��y@�J@�/@��`@��9@��u@�V@���@��D@�Q�@��;@�33@�ff@�5?@���@��!@��!@���@��+@�V@��T@���@�1'@�S�@��@���@�E�@��^@�`B@�G�@�&�@���@�I�@���@��@�o@��!@�ff@�$�@�O�@���@� �@��w@�"�@���@���@�K�@�ƨ@�9X@�A�@�Q�@�Z@�j@�j@��w@�C�@���@��R@��R@��R@�n�@��T@���@��7@��-@��#@���@�O�@�/@��@�V@�%@��`@��j@��u@�bN@� �@�b@��@�
=@��+@�^5@�E�@�5?@�{@���@��-@�hs@�7L@�&�@�%@��/@��u@��D@�bN@�b@��m@���@���@�;d@��\@�-@��@�@�p�@�7L@�%@���@��j@��D@�Z@�  @��m@��@��P@�|�@�t�@�S�@�@���@���@�5?@��^@�G�@��@��9@��u@��@�j@�9X@� �@��;@�t�@�+@�
=@�ȴ@���@�~�@�M�@��@���@��@��@��@��@��-@�`B@�/@��/@��u@�r�@�Z@�9X@�9X@�1@�  @��m@�ƨ@���@�S�@�@���@�5?@��T@��@�O�@�7L@���@���@��9@��u@�z�@�bN@�A�@�b@��w@���@�t�@�C�@��@���@��@��@��y@��@�ȴ@��R@���@��\@�ff@�=q@�$�@���@�@�p�@�O�@��@�Ĝ@���@��D@�z�@��@v@lj@bM�@[dZ@T�j@NV@J=q@D(�@<1@5��@0bN@+ƨ@%��@ A�@j@l�@�@ �@z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�)B�5B�fB�B��B��B��B  BB+B1B	7B
=B1B1B%B��B�sB�;B�#B��BB�qB�}B�LB��B��B��B��B��B��B�{B�oB�=B�B|�Bz�Bt�Bl�BffBbNBT�BA�B0!B'�B�B{BJB+B��B�B�yB�/B�B��BȴB��B�LB�B��B�BiyBQ�BB�B7LB#�BbB
��B
�BB
ÖB
�B
��B
�PB
v�B
dZB
S�B
@�B
&�B
�B
%B	�B	�TB	��B	�qB	�B	��B	��B	��B	�7B	o�B	\)B	N�B	<jB	0!B	"�B	�B	VB	PB	DB	
=B		7B��B�B�B�sB�NB�)B��B��BȴBƨBƨB��B��BƨBBŢBB�qB�}B�}B�wB�LB�9B�!B��B��B��B�{B�uB�uB�VB�PB�PB�1Bz�Bm�BhsBk�Bq�Bw�B�%B�B�%B�=B�=B�7B�1B�B� Bw�Bo�Bl�BjBiyBjBhsBgmBffBhsBjBjBjBgmBffB`BB`BBaHB`BB_;B^5B\)BYBYBT�BS�BR�BP�BO�BL�BO�BJ�BH�BC�B?}B?}BA�B>wB<jB<jB;dB:^B9XB5?B6FB6FB5?B33B33B2-B1'B1'B/B/B-B-B/B/B/B.B.B-B.B0!B1'B0!B0!B0!B/B1'B2-B33B2-B2-B1'B0!B-B.B0!B49B7LB<jB?}B@�B@�B@�B@�BA�BB�BC�BE�BH�BI�BL�BL�BN�BS�BVBVBW
B[#BaHBaHB`BB_;B]/B]/B^5BW
BXB^5BcTBffBiyBiyBiyBiyBhsBm�Bs�B}�B�B�+B�+B�+B�=B�JB�PB�hB��B��B��B��B��B��B��B��B��B�B�9B�RB�^B�dB�wBÖBǮB��B��B��B��B��B��B�
B�B�/B�HB�TB�`B�mB�sB�B�B�B�B�B��B��B��B��B	%B	
=B	\B	{B	�B	�B	�B	#�B	%�B	)�B	,B	.B	1'B	49B	8RB	=qB	C�B	H�B	L�B	R�B	YB	[#B	\)B	]/B	]/B	^5B	_;B	`BB	aHB	bNB	e`B	hsB	p�B	w�B	|�B	}�B	~�B	� B	�B	�%B	�1B	�=B	�DB	�PB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�3B	�9B	�9B	�FB	�XB	�dB	�jB	�jB	�qB	�qB	�qB	�wB	�wB	�wB	�}B	��B	ĜB	ƨB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�HB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
	7B
�B
!�B
+B
0!B
5?B
9XB
?}B
E�B
H�B
O�B
T�B
]/B
bNB
gmB
k�B
o�B
r�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�#B�/B�`B�B��B��B��BBB1B
=BDBPB
=BJBPBB�B�`B�mB�fB��B��B��B��B�?B��B��B��B��B��B��B��B�{B�JB�%B�B}�Bs�Bl�Bo�BffBS�B?}B33B)�B�B{B\B	7B��B�B�TB�BB�#B��BȴB�}B�jB�LB��Bx�B_;BN�BF�B5?B&�B
=B
��B
��B
�qB
�'B
��B
�B
v�B
gmB
M�B
5?B
,B
�B
B	��B	�NB	ȴB	�?B	�B	�B	��B	��B	~�B	m�B	cTB	O�B	?}B	6FB	$�B	hB	hB	oB	�B	�B	B��B��B��B�B�B�B��B��B��B��B��B��B��BɺB��BǮB��BŢB��B��B��B�jB�^B�?B�B��B��B��B��B�{B�uB��B��B�+Bt�Bm�Bo�Bu�By�B�7B�1B�=B�VB�PB�PB�hB�PB�DB�B{�Bv�Bq�Bp�Bn�Bk�Bl�Bl�Bk�Bk�Bl�Bn�Bo�Bm�Be`BdZBbNBbNBbNBdZBdZBcTBaHB\)BZBYBXBXBYBYBO�BN�BK�BJ�BG�BH�BE�BC�BB�BA�BA�B@�B?}B=qB;dB;dB9XB;dB9XB9XB9XB7LB5?B49B6FB6FB5?B49B1'B2-B49B33B49B5?B5?B7LB9XB7LB8RB7LB6FB5?B6FB7LB8RB2-B2-B5?B7LB8RB>wBC�BE�B@�B@�BC�BD�BE�BC�BE�BL�BN�BL�BO�BN�BS�BYBZB\)B[#BbNBbNB`BB_;B]/BffBe`B\)B[#B_;BcTBffBk�Bl�Bl�Bm�Bl�Br�Bv�B}�B�B�=B�+B�+B�=B�JB�PB�hB��B��B��B��B��B��B��B��B��B�B�?B�XB�dB�qB�wBÖBǮB��B��B��B��B��B�B�B�#B�;B�TB�TB�mB�yB�yB�B�B�B�B��B��B��B��B��B	B		7B	\B	{B	�B	�B	�B	#�B	'�B	+B	-B	.B	2-B	5?B	:^B	=qB	C�B	H�B	L�B	R�B	ZB	\)B	\)B	]/B	]/B	_;B	`BB	aHB	bNB	cTB	e`B	hsB	p�B	w�B	|�B	~�B	� B	� B	�B	�+B	�7B	�DB	�JB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�B	�'B	�-B	�9B	�9B	�?B	�9B	�LB	�^B	�jB	�qB	�jB	�qB	�wB	�wB	�}B	�wB	�wB	��B	ÖB	ŢB	ǮB	ɺB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�/B	�5B	�5B	�5B	�/B	�;B	�5B	�NB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B

=B
�B
!�B
+B
0!B
5?B
9XB
?}B
E�B
I�B
O�B
T�B
]/B
bNB
gmB
k�B
o�B
s�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<���<�C�<T��<�t�<�1<T��<#�
<#�
<#�
<#�
<#�
<T��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<�C�<�C�<u<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<�9X<�9X<u<T��<49X<u<�C�<�9X<��
<�9X<�C�<e`B<�o<u<e`B<�t�<���<T��<e`B<�C�<u<�C�<�t�<�C�<49X<#�
<#�
<49X<���<�1<u<�C�<��
<�t�<u<���<T��<#�
<#�
<#�
<T��<���<#�
<#�
<#�
<D��<T��<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<e`B<#�
<#�
<#�
<D��<u<49X<#�
<#�
<#�
<#�
<#�
<#�
<T��<D��<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452122012011014521220120110145212  AO  ARGQ                                                                        20111130144054  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144054  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145212  IP                  G�O�G�O�G�O�                