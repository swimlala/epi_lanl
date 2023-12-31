CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:53Z UW 3.1 conversion   
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               3A   AO  20111130144021  20190522121829  1728_5048_051                   2C  D   APEX                            2142                            040306                          846 @ԩ�.�	1   @ԩ����	@6M����c��v�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�33@�33A   A   A@  Aa��A���A���A���A�  A���A���A�  A���B   B  B  B  B   B(  B0  B8ffB@ffBH  BP  BX  B`  BhffBp  Bx  B�  B�  B���B�  B�33B�  B�  B�  B���B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B���B���B�  B�  B�  B�33B�33B�33B�33B���B���B���C�fC�fC�fC  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C9�fC<  C>  C@  CA�fCD  CF�CH  CJ  CL  CM�fCO�fCR  CT�CV�CX  CZ  C\�C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCq�fCt  Cv�Cx  Cz  C|�C~  C�fC�  C��C�  C��3C�  C��C�  C��3C�  C��C��C�  C�  C�  C��3C��3C�  C�  C��3C��3C��3C��3C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C�  C��C��C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C��C��C��C��C�  C�  C��3C�  C��C��C�  C��3C��3C��3C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C��C��C�  C�  C�  C��3C��3C��3C��3C��3C��3C��3D   D �fDfD� D  D� D��D� DfD� D  D� D  D�fDfD�fD  Dy�D	  D	� D
  D
y�D  D�fDfD� D  D� D  D� D  D� D  Dy�D  D� D��D� D  D� DfD� D��D� DfD� D��D� D  D�fD  D� D  D� D  D� D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#fD#�fD$  D$y�D$��D%y�D&  D&� D'fD'�fD(fD(� D(��D)y�D)��D*y�D*��D+y�D+��D,� D-  D-� D.fD.�fD/fD/�fD0fD0�fD1  D1� D2  D2� D2��D3� D4fD4� D5  D5� D6  D6�fD7fD7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF�fDG  DGy�DG��DH� DI  DI� DJfDJ� DJ��DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D\��D]y�D]��D^� D_  D_� D`  D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Dfy�Df��Dg� Dh  Dh� DifDi� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du�fDv  Dv� Dw  Dy� D�3D�I�D�i�D�ɚD���D�&fD�c3D���D���D�,�D�|�Dǳ3D�ٚD�#3Dڀ D��D�ٚD��D�S3D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @   @s33@���@�ffA33A;33A\��A|��A�ffA�ffA���A�ffA�ffAݙ�A�ffA���B��B��B��B��B&��B.��B733B?33BF��BN��BV��B^��Bg33Bn��Bv��B~��B�ffB�33B�ffB���B�ffB�ffB�ffB�33B�ffB���B���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�33B�33B�ffB�ffB�ffB癚B뙚BB�B�33B�33B�33C��C��C��C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C��C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1��C3�3C5�3C7�3C9��C;�3C=�3C?�3CA��CC�3CE��CG�3CI�3CK�3CM��CO��CQ�3CS��CU��CW�3CY�3C[��C]��C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co��Cq��Cs�3Cu��Cw�3Cy�3C{��C}�3C��C�ٚC��fC�ٚC���C�ٚC��fC�ٚC���C�ٚC��fC��fC�ٚC�ٚC�ٚC���C���C�ٚC�ٚC���C���C���C���C�ٚC��fC�ٚC�ٚC���C���C�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC�ٚC�ٚC��fC��fC�ٚC���C���C�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC���C���C�ٚC��fC��fC��fC��fC�ٚC�ٚC���C�ٚC��fC��fC�ٚC���C���C���C�ٚC�ٚC�ٚC��fC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC��fC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC���C�ٚC��fC��fC�ٚC�ٚC�ٚC���C���C���C���C���C���C���C�ٚD s3D �3Dl�D��Dl�D�fDl�D�3Dl�D��Dl�D��Ds3D�3Ds3D��DffD��D	l�D	��D
ffD
��Ds3D�3Dl�D��Dl�D��Dl�D��Dl�D��DffD��Dl�D�fDl�D��Dl�D�3Dl�D�fDl�D�3Dl�D�fDl�D��Ds3D��Dl�D��Dl�D��Dl�D�fDl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"s3D"�3D#s3D#��D$ffD$�fD%ffD%��D&l�D&�3D's3D'�3D(l�D(�fD)ffD)�fD*ffD*�fD+ffD+�fD,l�D,��D-l�D-�3D.s3D.�3D/s3D/�3D0s3D0��D1l�D1��D2l�D2�fD3l�D3�3D4l�D4��D5l�D5��D6s3D6�3D7l�D7�fD8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<�3D=s3D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE�3DFs3DF��DGffDG�fDHl�DH��DIl�DI�3DJl�DJ�fDKl�DK��DLl�DL��DMl�DM��DNl�DN��DOffDO��DPs3DP��DQl�DQ��DRl�DR��DSl�DS��DTffDT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\ffD\�fD]ffD]�fD^l�D^��D_l�D_��D`ffD`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De�fDfffDf�fDgl�Dg��Dhl�Dh�3Dil�Di��Djl�Dj�3Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr�3Dsl�Ds��Dtl�Dt��Dus3Du��Dvl�Dv��Dyl�D���D�@ D�` D�� D��3D��D�Y�D��3D��3D�#3D�s3Dǩ�D�� D��D�vfD�3D�� D�3D�I�D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�C�A�I�A�I�A�K�A�M�A�S�A�VA�XA�XA�XA�ZA�ZA�\)A�\)A�^5A�^5A�\)A�S�A�Q�A�Q�A�O�A�G�A�O�A�O�A�M�A�O�A�M�A�K�A�?}AǇ+A�$�AËDA�ffA�;dA��#A��A��A��\A�|�A�x�A�n�A�ȴA�dZA�VA�\)A�1A��DA�hsA�z�A���A��A�M�A�"�A��A��wA��7A��;A�^5A���A��A�x�A���A���A���A��-A��+A�JA�G�A��-A�oA�$�A��PA�E�A��A�XA��yA�JA�7LA�hsA��mA�C�A��^A�=qA��\A�1'A�hsA���A�n�A�Q�A��A�|�A���A��A�ffA��jA���A�jA���A���A��+A�~�A��A���A�bA���A�?}A��\A��A�;dA��^A}K�Aw�At�\Apr�An�Al�Ah��Af  AdE�A_��A]�^A\E�AZȴAY�^AV�ATĜASK�AQ`BAO�AN��AMVAJ�`AI&�AGG�AFn�AEAE7LAD�AC�7A@��A=A<JA;
=A:1'A9\)A8�HA8�A6(�A4Q�A3t�A3�A2M�A1�;A0��A/S�A.�A-��A-S�A,jA+�PA*�DA(��A(JA'\)A&��A&�A%S�A$r�A#��A#�7A"�/A"jA!��A!`BA �\AdZA�AjA��A`BAC�A
=A�/A�AffA1Ax�A��A\)A~�AƨA�AbA�9A��A��A��A�A�hA��A=qA�TA"�A
v�A	�
A��A(�AXA�uA�TA\)A��Ar�A��A&�A��AffA;dA ĜA 5?@��@�^5@��j@��@��@��@�b@�
=@�`B@��@�
=@��T@���@�!@�1@�&�@�V@�%@���@� �@�!@�G�@��@�+@�J@�^5@�7L@�~�@Ѓ@ϥ�@�o@Ͳ-@̼j@�1@�o@�G�@�Z@���@��@�+@+@��7@��j@�1'@�C�@�x�@��9@�1'@���@�
=@���@��j@�J@���@��R@�
=@��y@�n�@���@��/@��9@�A�@�33@�?}@���@���@���@�O�@��/@��9@�Z@� �@�9X@�  @�ƨ@�ƨ@���@�~�@�V@�-@���@��@�O�@��@�p�@�x�@�G�@��j@��@�Q�@��@���@��9@���@�Z@�o@�p�@��9@��-@���@�l�@�M�@��@��j@��+@���@�~�@�ff@���@�/@��/@��9@�Z@�(�@���@��@�;d@�+@�@���@���@���@�=q@�@��^@��P@���@��@�x�@�`B@�/@��@��u@�b@�1@�|�@�ȴ@�@�|�@��@��F@�l�@��!@�ȴ@��H@���@��R@��\@�v�@�^5@�=q@�@�O�@�Z@��P@��y@�^5@�=q@�{@��#@��^@��7@�O�@���@�z�@�ƨ@�t�@�l�@�K�@�
=@��!@�=q@��-@�&�@��u@�9X@��m@���@�C�@��R@�5?@��@��^@��7@�`B@�G�@�V@��@���@�Q�@��
@�t�@�K�@��@��R@��\@�n�@�n�@�~�@�V@�@��h@�p�@���@��h@�p�@�hs@�O�@��@�%@��@���@��9@��@���@�z�@�Q�@�(�@��m@��F@�C�@��@���@���@�M�@��@�@���@��@�?}@�&�@��@���@�Ĝ@���@�j@�1'@� �@�  @��
@��@��P@�S�@�o@��@���@�~�@�n�@�5?@�{@��T@�@���@���@�hs@��@��@�V@�%@��`@�I�@�1@��@��
@��F@��@���@�K�@�o@��!@�v�@�n�@�ff@���@{t�@r��@j��@_�@X�@R=q@J�@C�F@=�T@7|�@1�7@-V@(��@%?}@ �u@�@1'@�m@�@`B11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�A�C�A�I�A�I�A�K�A�M�A�S�A�VA�XA�XA�XA�ZA�ZA�\)A�\)A�^5A�^5A�\)A�S�A�Q�A�Q�A�O�A�G�A�O�A�O�A�M�A�O�A�M�A�K�A�?}AǇ+A�$�AËDA�ffA�;dA��#A��A��A��\A�|�A�x�A�n�A�ȴA�dZA�VA�\)A�1A��DA�hsA�z�A���A��A�M�A�"�A��A��wA��7A��;A�^5A���A��A�x�A���A���A���A��-A��+A�JA�G�A��-A�oA�$�A��PA�E�A��A�XA��yA�JA�7LA�hsA��mA�C�A��^A�=qA��\A�1'A�hsA���A�n�A�Q�A��A�|�A���A��A�ffA��jA���A�jA���A���A��+A�~�A��A���A�bA���A�?}A��\A��A�;dA��^A}K�Aw�At�\Apr�An�Al�Ah��Af  AdE�A_��A]�^A\E�AZȴAY�^AV�ATĜASK�AQ`BAO�AN��AMVAJ�`AI&�AGG�AFn�AEAE7LAD�AC�7A@��A=A<JA;
=A:1'A9\)A8�HA8�A6(�A4Q�A3t�A3�A2M�A1�;A0��A/S�A.�A-��A-S�A,jA+�PA*�DA(��A(JA'\)A&��A&�A%S�A$r�A#��A#�7A"�/A"jA!��A!`BA �\AdZA�AjA��A`BAC�A
=A�/A�AffA1Ax�A��A\)A~�AƨA�AbA�9A��A��A��A�A�hA��A=qA�TA"�A
v�A	�
A��A(�AXA�uA�TA\)A��Ar�A��A&�A��AffA;dA ĜA 5?@��@�^5@��j@��@��@��@�b@�
=@�`B@��@�
=@��T@���@�!@�1@�&�@�V@�%@���@� �@�!@�G�@��@�+@�J@�^5@�7L@�~�@Ѓ@ϥ�@�o@Ͳ-@̼j@�1@�o@�G�@�Z@���@��@�+@+@��7@��j@�1'@�C�@�x�@��9@�1'@���@�
=@���@��j@�J@���@��R@�
=@��y@�n�@���@��/@��9@�A�@�33@�?}@���@���@���@�O�@��/@��9@�Z@� �@�9X@�  @�ƨ@�ƨ@���@�~�@�V@�-@���@��@�O�@��@�p�@�x�@�G�@��j@��@�Q�@��@���@��9@���@�Z@�o@�p�@��9@��-@���@�l�@�M�@��@��j@��+@���@�~�@�ff@���@�/@��/@��9@�Z@�(�@���@��@�;d@�+@�@���@���@���@�=q@�@��^@��P@���@��@�x�@�`B@�/@��@��u@�b@�1@�|�@�ȴ@�@�|�@��@��F@�l�@��!@�ȴ@��H@���@��R@��\@�v�@�^5@�=q@�@�O�@�Z@��P@��y@�^5@�=q@�{@��#@��^@��7@�O�@���@�z�@�ƨ@�t�@�l�@�K�@�
=@��!@�=q@��-@�&�@��u@�9X@��m@���@�C�@��R@�5?@��@��^@��7@�`B@�G�@�V@��@���@�Q�@��
@�t�@�K�@��@��R@��\@�n�@�n�@�~�@�V@�@��h@�p�@���@��h@�p�@�hs@�O�@��@�%@��@���@��9@��@���@�z�@�Q�@�(�@��m@��F@�C�@��@���@���@�M�@��@�@���@��@�?}@�&�@��@���@�Ĝ@���@�j@�1'@� �@�  @��
@��@��P@�S�@�o@��@���@�~�@�n�@�5?@�{@��T@�@���@���@�hs@��@��@�V@�%@��`@�I�@�1@��@��
@��F@��@���@�K�@�o@��!@�v�@�n�@�ff@���@{t�@r��@j��@_�@X�@R=q@J�@C�F@=�T@7|�@1�7@-V@(��@%?}@ �u@�@1'@�m@�@`B11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BC�B^5Bn�B{�B�7B�\B�JB��B�B�jB�^B�XB�RB�jB�wB�XB�!B��B�bB{�Bp�Bv�B�JB�+B�By�Bq�BiyBn�Bm�Bk�BhsBgmBffBcTBaHB\)BXBQ�BJ�BA�B>wB<jB8RB2-B)�B#�B�BhB1B��B��B�B��BÖB�FB��B�1Bw�BbNBR�BG�B=qB49B.B+B%�B�B
��B
�B
�ZB
��B
��B
�B
��B
�JB
o�B
M�B
-B
B	�B	��B	��B	�oB	� B	m�B	W
B	C�B	.B	!�B	�B	bB	+B��B�B�B�TB�NB�ZB�BB�B��BǮBÖB�}B�jB�XB�'B��B��B�oB�\B�DB�7B�+B�B�B�1B�PB�hB��B��B��B��B��B��B��B��B��B�oB�\B�DB�=B�7B�1B�=B�%B�B�B�B�B�B~�B|�Bw�Bw�Bu�Bx�Bx�Bx�Bw�Bv�Bu�Bs�Bq�Bo�Bm�Bq�Bn�Bl�BjBhsBdZB`BB^5BZBZBXBVBVBT�BR�BR�BQ�BS�BQ�BR�BP�BP�BP�BP�BO�BP�BP�BO�BN�BO�BN�BN�BM�BN�BM�BL�BK�BK�BK�BK�BL�BL�BL�BL�BH�BH�BD�B=qB8RB9XBA�B@�B?}B>wB:^B6FB6FB.B$�B�B �B"�B$�B(�B)�B+B2-B9XB:^B<jB=qB>wB>wB@�BC�BE�BG�BI�BK�BL�BN�BQ�B[#BgmBs�B�B�1B�JB�\B�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�FB�wB�wB��BǮB��B��B��B��B��B��B��B�
B�)B�BB�mB�B�B�B��B	  B	B	B	B��B��B	B	
=B	B	B	B	  B��B�B��B��B��B��B	  B	B	%B	
=B	PB	oB	�B	�B	�B	�B	!�B	%�B	&�B	)�B	.B	9XB	F�B	K�B	L�B	M�B	M�B	N�B	N�B	M�B	O�B	Q�B	S�B	[#B	`BB	bNB	e`B	hsB	gmB	jB	l�B	m�B	m�B	n�B	n�B	o�B	p�B	s�B	x�B	z�B	z�B	}�B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�VB	�bB	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�?B	�LB	�RB	�^B	�jB	�}B	�}B	��B	��B	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�5B	�;B	�BB	�NB	�TB	�TB	�ZB	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
%B
\B
�B
%�B
+B
0!B
6FB
=qB
B�B
J�B
P�B
T�B
YB
\)B
bNB
ffB
l�B
o�B
s�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B'�BA�Bs�B|�B�B�+B�hB��B��B��B�?BÖB�}B�}BÖB��B��BǮB�qB�FB��B�Br�Bx�B�bB�JB�JB�Bz�Bq�Bt�Br�Bm�BiyBiyBjBjBk�BdZBaHB\)BQ�BE�BD�BD�B@�B=qB5?B-B"�B�B\B+BB��B�/B��BŢB�B�uB�%Bm�B\)BO�BG�B>wB7LB5?B5?B'�BDB
��B
�B
�#B
��B
�RB
�B
��B
�7B
hsB
M�B
(�B	�B	�)B	�?B	��B	��B	�B	iyB	_;B	<jB	,B	#�B	�B	�B	DB	  B��B�B�B�B�B�fB�/B��BȴBĜBĜBƨBƨB�^B��B��B��B�hB�\B�bB�oB�\B�VB�hB��B��B�B��B��B��B��B��B��B��B��B��B�hB�\B�\B�VB�bB�=B�7B�7B�7B�+B�+B�+B�+B�B� Bz�Bz�By�Bz�By�Bx�Bx�Bw�Bw�Bw�Bv�Bx�Bt�Bs�Bs�Bq�Bl�BhsBffBdZBbNB^5BZBZB[#BYBYBYB[#BXBYBVBVBT�BT�BS�BW
BS�BT�BVBS�BS�BR�BR�BT�BQ�BQ�BQ�BQ�BP�BQ�BQ�BP�BQ�BR�BP�BS�BM�BF�B<jB;dBE�BE�BD�BC�B>wB=qBD�B>wB.B%�B#�B%�B)�B-B-B/B8RB9XB:^BB�BA�BA�B>wBC�BC�BI�BM�BL�BM�BL�BN�BQ�BW
BgmBr�B�B�1B�JB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�LB�}B�wBÖBǮB��B��B��B��B��B��B��B�B�/B�NB�sB�B�B�B��B	B	1B	
=B	
=B	B��B	%B	oB	1B	B	+B	  B��B�B��B��B��B��B	B	B	%B	
=B	PB	uB	�B	�B	�B	�B	!�B	%�B	&�B	(�B	'�B	5?B	F�B	K�B	L�B	M�B	M�B	N�B	P�B	N�B	Q�B	S�B	S�B	ZB	`BB	cTB	ffB	jB	gmB	jB	l�B	n�B	m�B	o�B	o�B	p�B	q�B	v�B	{�B	}�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�1B	�DB	�\B	�bB	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�3B	�FB	�LB	�RB	�^B	�qB	��B	��B	��B	��B	ĜB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�;B	�BB	�BB	�TB	�TB	�TB	�`B	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B
%B
\B
�B
%�B
,B
0!B
6FB
=qB
C�B
J�B
P�B
T�B
ZB
\)B
bNB
ffB
l�B
o�B
s�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X=�w=@�<�<�1<49X<#�
<#�
<D��<49X<#�
<#�
<#�
<#�
<49X<�o<�o<T��<T��<�C�<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<�o<49X<D��<e`B<e`B<49X<e`B<49X<#�
<#�
<#�
<#�
<#�
<#�
<u<�C�<T��<#�
<u<49X<T��<D��<e`B<�9X<ě�<���=o=t�<�j<���<�C�<�t�<�j<��
<�t�<���<e`B<#�
<49X<49X<�C�<u<D��<T��<#�
<49X<T��<u<D��<49X<#�
<#�
<#�
<#�
<T��<�1<�t�<49X<#�
<#�
<#�
<#�
<#�
<e`B<49X<#�
<#�
<#�
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
<e`B<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452112012011014521120120110145211  AO  ARGQ                                                                        20111130144021  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144021  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145211  IP                  G�O�G�O�G�O�                