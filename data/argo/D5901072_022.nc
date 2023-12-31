CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:45Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       j    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       r   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143714  20190522121828  1728_5048_022                   2C  D   APEX                            2142                            040306                          846 @�a��j��1   @�a�@4�Z�1�c!p��
=1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�  @�  A   A   AA��A`  A�  A���A���A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@ffBH  BO��BX  B`  Bg��Bp  Bx  B�  B���B�  B�  B�  B�  B���B���B���B���B���B���B���B���B�  B�  B�  B�  B���B���B�  B�33B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C�C  C  C�fC  C�C�C �C"�C$�C&�C(�C*  C,  C.  C/�fC2  C4�C6  C7�fC:  C<  C=�fC@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CW�fCY�fC\�C^�C_�fCb  Cd�Cf  Cg�fCj  Cl  Cn  Co�fCq�fCt  Cv  Cw�fCz  C|  C~�C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C��3C�  C�  C��C�  C��3C�  C��C�  C��3C�  C�  C��C��C�  C�  C��C��C��C��C��C��C��C��C�  C�  C�  C��3C��3C��3C��3C��3C�  C��C��C��C��C��C��C��C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��C��3C��3C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C�  C�  D fD � D  D� D  D� D  D� D��D� D  D� D  Dy�DfD� D  D� D��D	� D
  D
� D  Dy�DfD� D  D� D��D�fDfD� D  D� D  D� D  D� D  D� D��Dy�D��Dy�D��D� DfD� D  D� D  Dy�D  D�fD  D� DfD�fDfD�fDfD� D  D� D   D � D!  D!�fD"fD"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(  D(�fD(��D)y�D*  D*� D+  D+y�D,  D,� D-  D-� D-��D.� D/fD/� D0  D0y�D1  D1� D2  D2� D3  D3�fD4fD4� D5  D5�fD6  D6� D7  D7� D7��D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDBfDB�fDCfDC� DD  DD� DD��DEy�DE��DF� DG  DG� DHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDN  DN� DO  DO� DPfDP�fDQfDQ�fDR  DR� DS  DS�fDT  DT� DU  DU� DV  DV�fDW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D[��D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dc��Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy��D�3D�33D�i�D��3D�  D�&fD���D��fD���D�,�D�ffDǦfD��D��D�p D��D��D�3D�C3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@,��@l��@�ff@�ffA33A<��A[33A{33A�ffA�ffA���A���A͙�Aݙ�A홚A���B��B33B��B��B&��B.��B6��B?33BF��BNffBV��B^��BfffBn��Bv��B~��B�33B�ffB�ffB�ffB�ffB�33B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�ffB�ffB�33B�33B�ffBә�B�ffB�ffBߙ�B�ffB�ffB뙚B�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	��C�3C�3C�3C��C�3C�3C��C�3C��C��C��C!��C#��C%��C'��C)�3C+�3C-�3C/��C1�3C3��C5�3C7��C9�3C;�3C=��C?�3CA��CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS��CU�3CW��CY��C[��C]��C_��Ca�3Cc��Ce�3Cg��Ci�3Ck�3Cm�3Co��Cq��Cs�3Cu�3Cw��Cy�3C{�3C}��C��C�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC�ٚC���C���C�ٚC�ٚC��fC�ٚC���C�ٚC��fC�ٚC���C�ٚC�ٚC��fC��fC�ٚC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC�ٚC�ٚC���C���C���C���C���C�ٚC��fC��fC��fC��fC��fC��fC��fC�ٚC�ٚC�ٚC���C�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC��fC��fC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC��fC�ٚC�ٚC��fC���C���C��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC�ٚC�ٚC�ٚC��fC�ٚC�ٚC��fC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC��fD l�D ��Dl�D��Dl�D��Dl�D�fDl�D��Dl�D��DffD�3Dl�D��Dl�D�fD	l�D	��D
l�D
��DffD�3Dl�D��Dl�D�fDs3D�3Dl�D��Dl�D��Dl�D��Dl�D��Dl�D�fDffD�fDffD�fDl�D�3Dl�D��Dl�D��DffD��Ds3D��Dl�D�3Ds3D�3Ds3D�3Dl�D��Dl�D��D l�D ��D!s3D!�3D"l�D"��D#l�D#�3D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(s3D(�fD)ffD)��D*l�D*��D+ffD+��D,l�D,��D-l�D-�fD.l�D.�3D/l�D/��D0ffD0��D1l�D1��D2l�D2��D3s3D3�3D4l�D4��D5s3D5��D6l�D6��D7l�D7�fD8l�D8��D9ffD9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAs3DA�3DBs3DB�3DCl�DC��DDl�DD�fDEffDE�fDFl�DF��DGl�DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM��DNl�DN��DOl�DO�3DPs3DP�3DQs3DQ��DRl�DR��DSs3DS��DTl�DT��DUl�DU��DVs3DV��DWffDW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[�fD\ffD\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��DcffDc�fDdl�Dd��Del�De�3Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dy�fD���D�)�D�` D���D��fD��D�� D���D��3D�#3D�\�Dǜ�D�� D�3D�ffD�3D�� D�	�D�9�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A��A��A��A��A��A��A���A���A���A���A��TA�ƨA���A���A��hA��PA�x�A�ffA�dZA�bNA�`BA�VA�Q�A�I�A�A�A�?}A�?}A�ffA��PA���A���A��uA���A���A�r�A�^5A�(�A��mA�ƨA��FA��PA�XA�E�A�A�A�(�A��TA���A��7A�"�A�ƨA��A��9A�1A�hsA�  A�t�A�33A��`A��A��HA��A��wA�z�A��jA�|�A��wA�G�A��
A�ffA�JA�t�A��7A�ȴA�7LA�K�A��A�?}A�1A��A���A�C�A���A�?}A�A��A�`BA���A��A��A�z�A��A��^A��-A��
A�\)A��#A���A��;A��#A���A�I�A��RA�v�A���A�ZA�n�A��/A���A�I�A��/A��
A��A�;dA�7LA}�wA{��Aw�Aq��Al��Aj�\Ah�yAg�-AfE�Ac�A`(�A^�HA]�^A\�uA[�AX�uAU�FAS��AQ;dAO�TAN��AM?}AK��AKdZAJ��AI�-AH5?AE�7AD��AC��AB��AAS�A?�^A>z�A=�A<��A;��A:��A:^5A:A9�hA9VA7��A6��A6E�A5��A5
=A4�\A3�FA2M�A1t�A17LA0�yA0=qA//A.5?A-;dA,bA*�\A)dZA(v�A&�`A$M�A#C�A#�A"�`A"~�A"�A!G�A n�A��AI�A��AoA{A�uA{A�
A33Av�A
=A��A�A��AE�A�PA��A`BA�+A �AG�A��A��A��A�A
��A	�PAĜAJAv�A�AA�A��AȴA�A�9AA n�@�|�@�p�@�"�@�p�@��F@�ff@�O�@�@�@@���@�Ĝ@�I�@��@�\)@���@��#@��`@�D@��m@�M�@��@䛦@�1@�S�@�\@ᙚ@�I�@���@�7L@ە�@�^5@�X@�33@֟�@�S�@�ƨ@׾w@ץ�@׍P@�S�@ָR@��/@�{@Η�@̓u@��@��@�@�\)@���@͡�@ύP@ϕ�@�;d@��H@�{@�V@̃@� �@���@��@ʗ�@�ff@�O�@� �@�ƨ@ǍP@�\)@�
=@��H@�ff@�=q@��T@�V@��y@���@���@��h@�G�@��@���@��\@��T@���@�n�@��m@���@���@�O�@�7L@��9@��D@�A�@���@��!@��^@�O�@���@���@�b@�l�@�;d@��@��@��T@��7@�O�@��@���@��@�9X@��@�  @��
@�;d@�^5@���@��9@�9X@�(�@��m@�33@�-@�^5@�n�@���@��@��@��F@�@�v�@�@��^@���@�7L@��@��j@��@��`@��@�(�@��F@�K�@��H@���@�^5@�=q@�{@���@��h@�?}@���@� �@��@��F@��w@�|�@��H@�E�@���@�p�@���@���@��j@��9@���@�A�@��
@���@�K�@�"�@�o@�o@�o@��@�
=@���@��@��@�;d@�S�@�33@��@��R@�v�@�$�@���@�&�@��/@�Q�@���@�;d@��@���@���@�=q@��-@��@�O�@�&�@�%@��@���@�Ĝ@��u@�I�@�b@���@�t�@�;d@��y@�^5@���@���@�X@�/@��9@�j@�A�@�  @��P@���@��R@���@�^5@��@�O�@���@���@��9@��D@�(�@��P@��@��@��R@���@��+@�n�@��@��h@�X@�?}@�&�@���@��/@��j@��D@�bN@�(�@��;@��P@�S�@�S�@�33@�
=@���@���@�M�@��@���@��7@�`B@�7L@��`@�j@�9X@�  @�n�@��@{C�@r�@k�m@dz�@Z��@S�
@M@HbN@@��@9��@1%@+�@&��@"J@�T@�7@Z@1'@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A��A��A��A��A��A��A���A���A���A���A��TA�ƨA���A���A��hA��PA�x�A�ffA�dZA�bNA�`BA�VA�Q�A�I�A�A�A�?}A�?}A�ffA��PA���A���A��uA���A���A�r�A�^5A�(�A��mA�ƨA��FA��PA�XA�E�A�A�A�(�A��TA���A��7A�"�A�ƨA��A��9A�1A�hsA�  A�t�A�33A��`A��A��HA��A��wA�z�A��jA�|�A��wA�G�A��
A�ffA�JA�t�A��7A�ȴA�7LA�K�A��A�?}A�1A��A���A�C�A���A�?}A�A��A�`BA���A��A��A�z�A��A��^A��-A��
A�\)A��#A���A��;A��#A���A�I�A��RA�v�A���A�ZA�n�A��/A���A�I�A��/A��
A��A�;dA�7LA}�wA{��Aw�Aq��Al��Aj�\Ah�yAg�-AfE�Ac�A`(�A^�HA]�^A\�uA[�AX�uAU�FAS��AQ;dAO�TAN��AM?}AK��AKdZAJ��AI�-AH5?AE�7AD��AC��AB��AAS�A?�^A>z�A=�A<��A;��A:��A:^5A:A9�hA9VA7��A6��A6E�A5��A5
=A4�\A3�FA2M�A1t�A17LA0�yA0=qA//A.5?A-;dA,bA*�\A)dZA(v�A&�`A$M�A#C�A#�A"�`A"~�A"�A!G�A n�A��AI�A��AoA{A�uA{A�
A33Av�A
=A��A�A��AE�A�PA��A`BA�+A �AG�A��A��A��A�A
��A	�PAĜAJAv�A�AA�A��AȴA�A�9AA n�@�|�@�p�@�"�@�p�@��F@�ff@�O�@�@�@@���@�Ĝ@�I�@��@�\)@���@��#@��`@�D@��m@�M�@��@䛦@�1@�S�@�\@ᙚ@�I�@���@�7L@ە�@�^5@�X@�33@֟�@�S�@�ƨ@׾w@ץ�@׍P@�S�@ָR@��/@�{@Η�@̓u@��@��@�@�\)@���@͡�@ύP@ϕ�@�;d@��H@�{@�V@̃@� �@���@��@ʗ�@�ff@�O�@� �@�ƨ@ǍP@�\)@�
=@��H@�ff@�=q@��T@�V@��y@���@���@��h@�G�@��@���@��\@��T@���@�n�@��m@���@���@�O�@�7L@��9@��D@�A�@���@��!@��^@�O�@���@���@�b@�l�@�;d@��@��@��T@��7@�O�@��@���@��@�9X@��@�  @��
@�;d@�^5@���@��9@�9X@�(�@��m@�33@�-@�^5@�n�@���@��@��@��F@�@�v�@�@��^@���@�7L@��@��j@��@��`@��@�(�@��F@�K�@��H@���@�^5@�=q@�{@���@��h@�?}@���@� �@��@��F@��w@�|�@��H@�E�@���@�p�@���@���@��j@��9@���@�A�@��
@���@�K�@�"�@�o@�o@�o@��@�
=@���@��@��@�;d@�S�@�33@��@��R@�v�@�$�@���@�&�@��/@�Q�@���@�;d@��@���@���@�=q@��-@��@�O�@�&�@�%@��@���@�Ĝ@��u@�I�@�b@���@�t�@�;d@��y@�^5@���@���@�X@�/@��9@�j@�A�@�  @��P@���@��R@���@�^5@��@�O�@���@���@��9@��D@�(�@��P@��@��@��R@���@��+@�n�@��@��h@�X@�?}@�&�@���@��/@��j@��D@�bN@�(�@��;@��P@�S�@�S�@�33@�
=@���@���@�M�@��@���@��7@�`B@�7L@��`@�j@�9X@�  @�n�@��@{C�@r�@k�m@dz�@Z��@S�
@M@HbN@@��@9��@1%@+�@&��@"J@�T@�7@Z@1'@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�9B
�qB
ƨB
��B
��B
��B
�/B
�fB
�sB
�B
�B
��B
��BB+B	7BbBQ�B�RB"�BM�BS�BbNBo�Bn�Bo�Bo�Bo�Bo�Bq�Bt�Bv�Bx�B�B�\B��B��B�'B�LB�-B�3B�FB�RB�jB�wB�wB�wB�}B��B��BBĜBĜBÖBĜBB��B��BB�}B�^B�B��B�oB�B{�BiyB^5BYB`BB_;BYBE�B-B$�B�B�BbB1B��B��B�B�NB��BƨB�^B��B��B�7Bs�BVB>wB+B�B�B+B
�B
�B
��B
�B
��B
� B
hsB
I�B
'�B
uB	��B	��B	�'B	��B	��B	�bB	�+B	|�B	jB	_;B	YB	S�B	N�B	H�B	=qB	7LB	49B	49B	2-B	/B	+B	%�B	 �B	�B	�B	�B	\B		7B	B��B��B�B�B�B�ZB�5B�B�)B�#B�
B��B��BɺBƨBB�}B�RB�9B�B��B��B��B��B��B��B��B�hB�\B�JB�=B�DB�JB�JB�=B�=B�1B�7B�7B�+B�+B�B�B�B� B~�B}�B{�Bz�Bz�Bv�Bt�Bs�Br�Bo�Bn�Bl�BiyBffBcTBcTB^5B]/B[#BW
BXBZBhsB|�B�B�B�B� Bx�Bn�B{�B� B{�By�Bw�Bx�By�Bw�Bu�Bs�Bt�Bu�By�Bz�B~�B� B�B�B�1B�hB��B��B��B��B��B��B��B��B��B��B��B�-B�XB�RB�FB�FB�FB�RB�}BŢBŢBŢBƨBĜBĜB��B�dB�9B�3B�RB�wBÖBɺB��B�TB��B	B	B	B	+B	PB	hB	oB	{B	�B	!�B	!�B	#�B	$�B	&�B	)�B	.B	6FB	>wB	C�B	D�B	G�B	E�B	C�B	D�B	H�B	H�B	H�B	H�B	J�B	L�B	L�B	O�B	W
B	cTB	e`B	cTB	cTB	hsB	jB	n�B	q�B	r�B	q�B	q�B	u�B	v�B	w�B	|�B	~�B	� B	�B	�B	�7B	�DB	�DB	�DB	�JB	�VB	�bB	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�?B	�?B	�FB	�XB	�dB	�dB	�jB	�jB	�jB	�jB	�qB	�qB	�}B	��B	��B	��B	��B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�ZB	�ZB	�TB	�TB	�ZB	�ZB	�fB	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�mB	�sB	�yB	�yB	�yB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
\B
�B
�B
!�B
'�B
0!B
8RB
<jB
D�B
I�B
N�B
W
B
^5B
aHB
gmB
l�B
p�B
t�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�?B
�wB
ǮB
��B
��B
��B
�5B
�fB
�sB
�B
�B
��B
��BB+B	7B\BP�B�RB"�BM�BS�BcTBq�Bo�Br�Br�Bp�Bp�Bs�Bv�Bw�By�B�B�oB��B��B�FB�jB�LB�qB�wBB��BĜBBÖBŢB��B��B��B��B��B��B��BȴBƨBǮBȴBɺBǮB�jB�dB��B��B�oBx�BiyB`BBgmBk�Bk�BR�B2-B)�B#�B�B�BbBB��B��B�B�
B��BǮB�RB��B��B�7BjBL�B5?B'�B(�B�B
��B
�B
��B
�dB
�B
��B
�%B
e`B
=qB
7LB
�B	�B	�wB	�B	��B	��B	��B	�hB	t�B	hsB	bNB	aHB	dZB	]/B	M�B	F�B	>wB	>wB	<jB	7LB	1'B	.B	+B	(�B	)�B	�B	�B	uB	bB		7B	B��B��B�B�B�NB�5B�HB�HB�BB�B��B��B��BǮBǮB��B�XB�B�B�B�B��B��B��B��B��B��B��B��B�hB�VB�\B�VB�\B�\B�bB�bB�bB�PB�DB�DB�DB�B�B�B�B�B�B{�Bw�Bx�Bx�Bv�Bv�Br�Bm�Bl�BiyBjBffBdZBdZB^5B^5B^5BgmB~�B�+B�=B�VB�7B�Bq�B�B�7B�B�B� B� B� B|�Bz�Bz�B|�B~�B}�B}�B�B�B�B�%B�JB��B��B��B�B��B��B��B��B��B��B��B�B�RB�XB�qB�dB�jB�LB�FB�wBƨBŢBƨBȴBȴB��B��B�dB�dB�LB�RB�wB��BƨB��B�5B��B	B	B	+B	DB	PB	hB	oB	�B	�B	#�B	%�B	'�B	&�B	'�B	+B	/B	6FB	>wB	C�B	D�B	G�B	K�B	C�B	E�B	I�B	J�B	K�B	H�B	M�B	L�B	L�B	M�B	W
B	cTB	e`B	cTB	cTB	jB	k�B	p�B	t�B	v�B	t�B	q�B	u�B	x�B	y�B	~�B	~�B	�B	�B	�%B	�=B	�JB	�JB	�JB	�PB	�VB	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�9B	�9B	�FB	�FB	�?B	�FB	�^B	�qB	�qB	�qB	�jB	�jB	�jB	�qB	�qB	��B	��B	��B	��B	��B	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�B	�)B	�B	�B	�B	�
B	�B	�B	�B	�B	�#B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�;B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�fB	�fB	�TB	�TB	�`B	�`B	�mB	�fB	�mB	�mB	�sB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
\B
�B
�B
!�B
'�B
0!B
8RB
<jB
D�B
I�B
N�B
W
B
^5B
aHB
gmB
l�B
p�B
t�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<T��<u<e`B<D��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<T��<T��<���<T��<�t�<�9X<u<49X<#�
<#�
<D��<�C�<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<T��<e`B<T��<u<�1<��
<e`B<#�
<#�
<u<���<�C�<���<���<u<��
<�9X<�h<�/<�1=\)=��<�h<T��<D��<#�
<D��<�t�<��
<#�
<#�
<#�
<T��<�1<��
<�o<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<�o<#�
<#�
<#�
<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452002012011014520020120110145200  AO  ARGQ                                                                        20111130143714  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143714  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145200  IP                  G�O�G�O�G�O�                