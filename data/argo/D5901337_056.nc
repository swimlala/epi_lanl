CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:39Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               8A   AO  20111205113302  20190522121836  1901_5055_056                   2C  D   APEX                            2140                            040306                          846 @Դ�m˯�1   @Դ�����@--V��cn�x���1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  BffB ffB(ffB0  B7��B@  BH  BO��BX  B`  BhffBp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B���B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B���C   C�C  C  C  C	�fC�fC�fC�fC�fC  C  C�C  C  C  C   C"  C$  C&  C(  C*�C,�C-�fC/�fC2�C4�C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn�Cp  Cq�fCt  Cv  Cw�fCy�fC|  C~  C��C�  C�  C��C�  C�  C��C�  C�  C��3C��3C��3C�  C��C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C��C�  C��3C�  C�  C��C��C��C��C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C��3C�  C��C��C��C��C��C��C��C��C�  C��3C��3C��3C�  C�  C��C�  C��3C�  C��C�  C��3C�  C�  C�  C��C��C��C��C��C��C��C��C��C��C��C�  C�  C��3C�  C��C��C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C��3C��3C��3C�  C��C��C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C��C��C�  C��C�  C��3D � D  D� D  D� DfD�fDfD�fDfD�fD  Dy�D  Dy�D��D� D	fD	�fD
fD
� D  D� D  D�fD  Dy�D  D� D��D� D  D� D��D� DfD� D  D� DfD� D��D� D  D� D  D� D  D� D��D� D  D� D��Dy�D  D�fD  D� D  D� D  D� D fD �fD!  D!y�D!��D"y�D"��D#y�D#��D$y�D%  D%� D&  D&y�D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/�fD0fD0� D0��D1� D2  D2� D3fD3�fD4  D4� D5  D5� D6fD6� D6��D7� D8fD8� D8��D9� D:  D:� D;fD;� D<  D<� D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DB� DC  DC� DD  DD� DD��DE� DF  DFy�DG  DG� DG��DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN� DN��DOy�DO��DP� DQ  DQy�DQ��DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\y�D]  D]� D^  D^� D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl�fDm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dys3D�fD�33D�i�D���D��D�<�D�� D�ɚD���D�  D��3DǶfD���D�#3D�\�D�` D��fD�0 D�VfD�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @l��@�  @�33A��A9��AY��Ay��A���A���A���A���A���A���A홚A���BffBffB��B��B&��B.ffB6  B>ffBFffBN  BVffB^ffBf��BnffBvffB~ffB�  B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�  B�  B�33B�33B�  B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�  B�33B�33B�  B�33C�3C��C��C��C	� C� C� C� C� C��C��C�3C��C��C��C��C!��C#��C%��C'��C)�3C+�3C-� C/� C1�3C3�3C5��C7��C9��C;��C=��C?��CA��CC��CE�3CG��CI� CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]�3C_��Ca��Cc��Ce��Cg�3Ci��Ck��Cm�3Co��Cq� Cs��Cu��Cw� Cy� C{��C}��C�3C���C���C�ٚC���C���C�ٚC���C���C�� C�� C�� C���C�ٚC���C���C�� C�� C�� C���C���C���C���C�ٚC�ٚC���C�� C���C���C�ٚC�ٚC�ٚC�ٚC���C���C�� C���C���C�� C���C�ٚC���C���C�� C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�� C�� C�� C���C���C�ٚC���C�� C���C�ٚC���C�� C���C���C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C���C�� C���C�ٚC�ٚC���C���C���C�� C���C���C���C���C�ٚC�ٚC���C���C���C�� C�� C�� C���C�ٚC�ٚC���C���C�ٚC���C�� C���C���C�� C���C���C�� C���C���C���C�ٚC���C���C���C�ٚC�ٚC���C�ٚC���C�� D ffD �fDffD�fDffD��Dl�D��Dl�D��Dl�D�fD` D�fD` D� DffD��D	l�D	��D
ffD
�fDffD�fDl�D�fD` D�fDffD� DffD�fDffD� DffD��DffD�fDffD��DffD� DffD�fDffD�fDffD�fDffD� DffD�fDffD� D` D�fDl�D�fDffD�fDffD�fDffD��D l�D �fD!` D!� D"` D"� D#` D#� D$` D$�fD%ffD%�fD&` D&�fD'l�D'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.��D/l�D/��D0ffD0� D1ffD1�fD2ffD2��D3l�D3�fD4ffD4�fD5ffD5��D6ffD6� D7ffD7��D8ffD8� D9ffD9�fD:ffD:��D;ffD;�fD<ffD<� D=` D=� D>` D>� D?` D?� D@` D@� DA` DA� DBffDB�fDCffDC�fDDffDD� DEffDE�fDF` DF�fDGffDG� DHffDH�fDIffDI�fDJ` DJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN� DO` DO� DPffDP�fDQ` DQ� DRffDR��DSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[��D\` D\�fD]ffD]�fD^ffD^� D_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj��DkffDk�fDll�Dl�fDmffDm�fDnl�Dn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsl�Ds�fDtffDt�fDuffDu�fDvffDyY�D�	�D�&fD�\�D���D�  D�0 D�s3D���D�� D�3D�vfDǩ�D�� D�fD�P D�S3D��D�#3D�I�D�	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A���AǅA�x�A�`BA�9XA�A��mA��/A���A���A���A���A���A�ƨA���A���A���A���A��A��HA��A�  A��A�;dA�\)A�r�AǇ+Aǰ!A��/A��A�1A� �A�=qA�`BA�l�A�ffA�ffA�K�A���A��;A�\)A���A��DA��uA���A�;dA�bNA���A� �A��RA���A��hA�z�A���A�+A�G�A�hsA���A�5?A�JA��hA�S�A���A��A���A�JA�5?A���A�K�A��+A���A��+A�oA��yA�v�A�JA�n�A�A�A�{A��A��`A�bNA�\)A�"�A���A�`BA|�jAy�At  Ap{Am�PAi��Ae��AdQ�Aa��A^bNA[�TAY��AU��AS`BAQp�AOG�AM��AL�yAK��AI33AG7LAF1'ACC�A@�`A?��A>9XA:��A8�!A7/A4�RA2jA.Q�A-S�A+�^A+/A*�\A)�A'hsA'\)A($�A(-A(9XA+O�A,jA+�A*��A)�^A(��A'x�A%t�A$��A#�PA#\)A#ƨA#��A#33A#dZA"��A"jA"-A!K�A jA��A bNA 1'Av�A��AA�;A��A��A��A��A�#Ax�A�jA�uA7LA��A��AE�A�
A|�A�PA��A�TA`BA��AI�A�A&�AVA
=A�HA�DAAl�A?}A�A�A��Az�A`BA��AM�A�A�AS�A"�A
=A�A�A�/AVAƨA�FA�7A"�A
��A
�!A	�
A�AM�A9XAr�Av�A�A�
AS�A�HAn�Az�AVA{A�A�hAdZA33A�yA�+AI�A��A33A��A�/Ar�AJAXA ��A r�A �@���@�"�@��\@�=q@�-@�@��@���@��+@�x�@�  @���@�+@�ff@�J@��@�bN@��;@�"�@��@��@�"�@@�ff@���@���@�(�@�S�@�\@�@�@�w@��y@�G�@��/@�@�ƨ@��H@�5?@�^@�`B@�j@߅@ް!@���@ܣ�@���@�|�@���@�-@�7L@�I�@�  @���@�C�@��@Չ7@�7L@�1@Ӆ@�|�@�+@��@�V@Ѻ^@�A�@�dZ@Χ�@�5?@�@�hs@�O�@�?}@���@�z�@�1'@˶F@��@�E�@��#@ɡ�@�`B@��@���@ȼj@�bN@�dZ@�V@��@���@ċD@��@�dZ@�@¸R@�v�@�M�@���@��#@���@�I�@��m@��
@��@�|�@�v�@�$�@���@�O�@��`@�I�@��@�S�@���@�n�@�=q@���@�hs@�&�@��@���@�Ĝ@���@�r�@��@�\)@��@��\@��@��#@���@���@�/@���@�b@�ƨ@��w@�C�@�
=@��@���@�v�@�5?@���@��h@�hs@�7L@�I�@���@��w@���@�@��R@���@�E�@�@�`B@�X@�`B@�G�@�%@���@��@�A�@�b@���@�|�@�S�@�"�@�ȴ@�ff@�{@�J@�J@��@��@�?}@��D@��m@�S�@�"�@��H@�~�@��-@��@��j@�9X@�1@�1@�b@�b@�  @��
@���@��@��@�-@�J@��-@���@�X@��@��D@�(�@���@��w@�\)@�33@��H@�v�@�E�@�-@���@���@�/@���@�Q�@�9X@�1@��@��m@�ƨ@���@�dZ@�K�@��@��H@�v�@�@��^@�p�@�&�@���@��9@�A�@�1@��@�dZ@�C�@�@��@��h@�p�@��@��9@��@�9X@���@�C�@�o@��H@���@�E�@�J@��j@��h@�ƨ@{ƨ@s��@i%@^��@V�R@Nff@F$�@=O�@6$�@0bN@*=q@$�@z�@K�@�!@��@
�!@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��#A���AǅA�x�A�`BA�9XA�A��mA��/A���A���A���A���A���A�ƨA���A���A���A���A��A��HA��A�  A��A�;dA�\)A�r�AǇ+Aǰ!A��/A��A�1A� �A�=qA�`BA�l�A�ffA�ffA�K�A���A��;A�\)A���A��DA��uA���A�;dA�bNA���A� �A��RA���A��hA�z�A���A�+A�G�A�hsA���A�5?A�JA��hA�S�A���A��A���A�JA�5?A���A�K�A��+A���A��+A�oA��yA�v�A�JA�n�A�A�A�{A��A��`A�bNA�\)A�"�A���A�`BA|�jAy�At  Ap{Am�PAi��Ae��AdQ�Aa��A^bNA[�TAY��AU��AS`BAQp�AOG�AM��AL�yAK��AI33AG7LAF1'ACC�A@�`A?��A>9XA:��A8�!A7/A4�RA2jA.Q�A-S�A+�^A+/A*�\A)�A'hsA'\)A($�A(-A(9XA+O�A,jA+�A*��A)�^A(��A'x�A%t�A$��A#�PA#\)A#ƨA#��A#33A#dZA"��A"jA"-A!K�A jA��A bNA 1'Av�A��AA�;A��A��A��A��A�#Ax�A�jA�uA7LA��A��AE�A�
A|�A�PA��A�TA`BA��AI�A�A&�AVA
=A�HA�DAAl�A?}A�A�A��Az�A`BA��AM�A�A�AS�A"�A
=A�A�A�/AVAƨA�FA�7A"�A
��A
�!A	�
A�AM�A9XAr�Av�A�A�
AS�A�HAn�Az�AVA{A�A�hAdZA33A�yA�+AI�A��A33A��A�/Ar�AJAXA ��A r�A �@���@�"�@��\@�=q@�-@�@��@���@��+@�x�@�  @���@�+@�ff@�J@��@�bN@��;@�"�@��@��@�"�@@�ff@���@���@�(�@�S�@�\@�@�@�w@��y@�G�@��/@�@�ƨ@��H@�5?@�^@�`B@�j@߅@ް!@���@ܣ�@���@�|�@���@�-@�7L@�I�@�  @���@�C�@��@Չ7@�7L@�1@Ӆ@�|�@�+@��@�V@Ѻ^@�A�@�dZ@Χ�@�5?@�@�hs@�O�@�?}@���@�z�@�1'@˶F@��@�E�@��#@ɡ�@�`B@��@���@ȼj@�bN@�dZ@�V@��@���@ċD@��@�dZ@�@¸R@�v�@�M�@���@��#@���@�I�@��m@��
@��@�|�@�v�@�$�@���@�O�@��`@�I�@��@�S�@���@�n�@�=q@���@�hs@�&�@��@���@�Ĝ@���@�r�@��@�\)@��@��\@��@��#@���@���@�/@���@�b@�ƨ@��w@�C�@�
=@��@���@�v�@�5?@���@��h@�hs@�7L@�I�@���@��w@���@�@��R@���@�E�@�@�`B@�X@�`B@�G�@�%@���@��@�A�@�b@���@�|�@�S�@�"�@�ȴ@�ff@�{@�J@�J@��@��@�?}@��D@��m@�S�@�"�@��H@�~�@��-@��@��j@�9X@�1@�1@�b@�b@�  @��
@���@��@��@�-@�J@��-@���@�X@��@��D@�(�@���@��w@�\)@�33@��H@�v�@�E�@�-@���@���@�/@���@�Q�@�9X@�1@��@��m@�ƨ@���@�dZ@�K�@��@��H@�v�@�@��^@�p�@�&�@���@��9@�A�@�1@��@�dZ@�C�@�@��@��h@�p�@��@��9@��@�9X@���@�C�@�o@��H@���@�E�@�J@��j@��h@�ƨ@{ƨ@s��@i%@^��@V�R@Nff@F$�@=O�@6$�@0bN@*=q@$�@z�@K�@�!@��@
�!@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	%B		7B	\B	�B	#�B	33B	J�B	\)B	ffB	r�B	�B	��B	��B	��B	�B	�^B	ȴB	��B	��B
DB
�jB
�BDB"�B�B\B
=B
=B�B:^B9XB5?BH�B_;Bt�B|�B}�B�=B�oB��B��B�B�B�wB�}B��BȴB��B��B��B��B�dB��B�PBu�B[#B@�B&�B
��B
��B
�qB
�RB
�!B
��B
}�B
e`B
K�B
,B
VB	��B	�RB	��B	�B	o�B	W
B	B�B	5?B	"�B	hB	B��B�B�ZB�;B�)B�B��BɺB�}B�?B�B��B��B��B�{B�{B�{B�hB�oB�uB��B��B��B��B�B�FB�jB�B��B	DB	�B	}�B	�FB	�^B	��B	�dB	�FB	�jB	�wB	�qB	�wB	��B	�HB	�sB	�B	�B	��B	��B	��B	��B	��B	��B
+B
1B	��B	��B
%B
uB
�B
�B
�B
#�B
$�B
#�B
#�B
/B
+B
,B
5?B
49B
49B
33B
6FB
9XB
9XB
7LB
33B
33B
6FB
9XB
8RB
49B
7LB
6FB
49B
5?B
5?B
49B
33B
33B
2-B
-B
)�B
(�B
,B
-B
.B
0!B
0!B
/B
/B
.B
.B
.B
0!B
0!B
.B
-B
+B
(�B
#�B
"�B
"�B
&�B
-B
-B
,B
+B
(�B
&�B
)�B
+B
+B
+B
)�B
)�B
)�B
(�B
'�B
&�B
$�B
"�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
oB
uB
uB
oB
hB
bB
\B
VB
JB
DB
DB

=B
DB
DB
DB

=B
1B
+B
%B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
%B
+B
+B
+B
%B
+B
1B
1B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B
	7B
1B
	7B
	7B
	7B
	7B
	7B
1B
	7B
	7B
1B
+B
%B
%B
+B
1B
1B
	7B

=B
DB
DB
DB
DB

=B

=B

=B
DB
DB
DB
JB
JB
PB
JB
PB
PB
VB
VB
PB
VB
VB
VB
VB
bB
bB
bB
bB
\B
bB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
+B
2-B
7LB
@�B
E�B
I�B
P�B
VB
[#B
`BB
dZB
hsB
k�B
q�B
u�B
z�B
~�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	%B		7B	\B	�B	"�B	2-B	I�B	\)B	e`B	q�B	�B	��B	��B	��B	�B	�XB	ȴB	��B	�B
PB
��B
��B0!BS�B33B�B#�B�B.BI�BE�BM�BW
Bs�B� B�B�%B�{B��B��B�B�B�!BBƨB��B�
B�B�B�#B�;B��B�LB��B�1Bl�BXBK�B$�B
�mB
��B
ĜB
ɺB
�FB
�\B
z�B
m�B
L�B
C�B	�B	�5B	�RB	��B	�=B	l�B	Q�B	H�B	9XB	#�B	�B	uB��B�B�B�`B�HB�)B�#B��B��B�wB�3B��B��B�B��B��B��B��B�B��B��B��B�B�?BB�dB�B��B	B	\B	z�B	�qB	��B	ȴB	ŢB	��B	ɺB	ŢB	ÖB	�}B	��B	�TB	�B	�B	��B	��B
B
%B
B	��B	��B
JB
{B
B	��B
%B
uB
�B
�B
�B
%�B
(�B
(�B
&�B
7LB
-B
.B
:^B
7LB
7LB
5?B
<jB
?}B
=qB
<jB
7LB
7LB
=qB
@�B
@�B
6FB
:^B
:^B
8RB
7LB
7LB
7LB
6FB
7LB
:^B
2-B
.B
,B
/B
/B
0!B
1'B
1'B
0!B
1'B
2-B
2-B
/B
2-B
33B
1'B
1'B
2-B
1'B
$�B
"�B
!�B
'�B
0!B
0!B
0!B
.B
,B
&�B
,B
-B
.B
,B
,B
,B
-B
,B
+B
+B
(�B
$�B
$�B
%�B
$�B
$�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
{B
�B
uB
oB
hB
hB
bB
\B
JB
DB
DB
DB

=B
1B
+B
	7B
B
%B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B
  B
  B
B
  B
  B
  B
  B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
1B
+B
1B
	7B
+B
+B
+B
1B
	7B

=B

=B
DB
DB
DB
DB

=B
DB
DB
DB
DB
	7B
	7B
	7B

=B
	7B
	7B
1B
DB
DB
1B
+B
1B
%B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
DB
DB
JB
PB
JB
JB
PB
PB
PB
JB
VB
VB
\B
\B
PB
\B
\B
\B
\B
hB
hB
oB
oB
bB
hB
uB
uB
oB
uB
uB
uB
uB
{B
{B
{B
{B
{B
�B
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
+B
2-B
7LB
A�B
E�B
I�B
P�B
VB
\)B
`BB
dZB
hsB
k�B
q�B
u�B
{�B
~�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o=\)=D��<��
<e`B<���<�o<��
<u<D��<ě�<e`B<��
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
<e`B<#�
<#�
<#�
<���<�1<���<�t�<�t�<�C�<�j=t�=��<ě�<u<D��<���<�/<�C�<�1=+=o=T��<ě�=�P<���<���<���<�1<u<���<�9X<�C�<���<ě�<u<T��<e`B<#�
<#�
<49X<�C�<T��<D��<�t�<u<#�
<u<�9X<e`B<e`B<���<�t�<ě�<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<�o<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250272012011312502720120113125027  AO  ARGQ                                                                        20111205113302  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113302  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125027  IP                  G�O�G�O�G�O�                