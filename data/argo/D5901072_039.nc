CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:50Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               'A   AO  20111130143904  20190522121828  1728_5048_039                   2C  D   APEX                            2142                            040306                          846 @ԋ���?�1   @ԋ�)���@5�bM���c4��E�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   AffA@  A`  A�  A���A���A�  A�33A�33A�  A�  B ffB  B  B  B   B(  B/��B8  B@  BH  BPffBX  B_��Bg��Bp  BxffB�33B�33B�33B�33B�33B�  B�  B�  B�33B�  B�  B�33B�  B���B���B�  B�33B�  B���B���B�  B�33B�  B�  B�  B�33B�  B�  B�33B�  B�  B�33C   C  C  C  C  C
  C  C�fC  C  C�fC  C�C  C�fC�fC   C"�C$  C&  C(�C*�C,�C.  C0  C2  C4  C6  C8  C9�fC<  C>�C@  CB  CD  CE�fCG�fCI�fCK�fCM�fCO�fCQ�fCT  CV�CX�CZ�C\�C^�C`  Ca�fCc�fCe�fCh  Cj�Cl  Cn  Co�fCr  Ct  Cv  Cx�Cz  C{�fC~  C��C�  C�  C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C�  C�  C�  C��C��C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C��C�  C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C��3C�  C�  C��C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��3C��3C��C��C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  D   D � D  D� DfD� D  D� DfD� D��D� D  Dy�D  D� D��D� D	fD	� D
  D
� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D�fD  Dy�D  D� D  D� D��D� D  Dy�D  D�fD  D� D  D� D��D� D  D� DfD� D  D� D  D� D��D �fD!  D!� D"  D"�fD#fD#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,�fD-  D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1� D2fD2� D3  D3� D3��D4� D5  D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:�fD;  D;� D<  D<� D=  D=� D>  D>y�D?  D?�fD@fD@� D@��DA� DB  DB� DB��DC� DD  DD� DD��DE� DFfDF� DG  DG� DG��DH� DIfDI�fDJfDJ�fDK  DK� DL  DLy�DM  DM� DN  DN� DN��DOy�DO��DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV�fDW  DWy�DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\y�D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Dh��Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy��D�fD�&fD�\�D���D���D�#3D�� D�� D���D�#3D�� DǶfD�� D��DچfD� D��fD�  D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @l��@�ff@�ffA��A;33A[33A{33A�ffA�ffA���A���A���Aݙ�A홚A�ffB��B��B��B��B&��B.ffB6��B>��BF��BO33BV��B^ffBfffBn��Bw33B33B���B���B���B���B�ffB�ffB�ffB���B�ffB�ffB���B�ffB�33B�33B�ffB���B�ffB�33B�33B�ffBә�B�ffB�ffB�ffB㙚B�ffB�ffBB�ffB�ffB���B�ffC�3C�3C�3C�3C	�3C�3C��C�3C�3C��C�3C��C�3C��C��C�3C!��C#�3C%�3C'��C)��C+��C-�3C/�3C1�3C3�3C5�3C7�3C9��C;�3C=��C?�3CA�3CC�3CE��CG��CI��CK��CM��CO��CQ��CS�3CU��CW��CY��C[��C]��C_�3Ca��Cc��Ce��Cg�3Ci��Ck�3Cm�3Co��Cq�3Cs�3Cu�3Cw��Cy�3C{��C}�3C��C�ٚC�ٚC���C���C���C���C�ٚC���C���C���C���C���C�ٚC�ٚC�ٚC��fC��fC�ٚC���C�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC�ٚC���C�ٚC��fC�ٚC�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC���C�ٚC�ٚC���C���C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC��fC��fC���C�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC�ٚC�ٚC�ٚC���C���C��fC��fC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC���C�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC��fC�ٚC���C�ٚC�ٚD l�D ��Dl�D�3Dl�D��Dl�D�3Dl�D�fDl�D��DffD��Dl�D�fDl�D�3D	l�D	��D
l�D
��Ds3D�3Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Ds3D��DffD��Dl�D��Dl�D�fDl�D��DffD��Ds3D��Dl�D��Dl�D�fDl�D��Dl�D�3Dl�D��Dl�D��Dl�D�fD s3D ��D!l�D!��D"s3D"�3D#s3D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+�3D,s3D,��D-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1l�D1�3D2l�D2��D3l�D3�fD4l�D4��D5ffD5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:s3D:��D;l�D;��D<l�D<��D=l�D=��D>ffD>��D?s3D?�3D@l�D@�fDAl�DA��DBl�DB�fDCl�DC��DDl�DD�fDEl�DE�3DFl�DF��DGl�DG�fDHl�DH�3DIs3DI�3DJs3DJ��DKl�DK��DLffDL��DMl�DM��DNl�DN�fDOffDO�fDPffDP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU�3DVs3DV��DWffDW��DXl�DX��DYl�DY�fDZl�DZ��D[l�D[��D\ffD\�fD]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��DgffDg��Dhl�Dh�fDil�Di��Djl�Dj�fDkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dss3Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dy�fD���D��D�S3D��3D��3D��D��fD��fD��3D��D�vfDǬ�D��fD� D�|�D�fD���D�fD�0 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�C�A���A��`AήA΍PA�x�A�`BA�M�A�=qA�1'A�(�A��A�VA�A���A��A��mA���ÁA���A��#A�
=A�{A�dZAć+AÏ\A�C�A�5?A��-A���A�VA�jA���A�1A�A�{A�A�A��PA��A�O�A���A�^5A�VA��A�9XA�O�A���A��A��A�O�A���A�$�A�z�A��7A��TA�n�A�"�A��A��!A�/A���A���A�1'A�M�A�XA��A�G�A��DA���A�/A���A��A�$�A���A��HA�`BA��A�~�A���A��-A�VA�z�A�l�A�+A�K�A��+A�&�A�-A��uA���A�C�A���A���A���A�K�A�VA�"�A�(�A��jA�O�A��uA��wA�bA�E�A�oA�%A��9A�7LA���A��#A��\A�~�A�A�+A~ȴA{�AzJAy�Ay%Awt�Au%As+AqƨAn��Al�DAj�Ae��AbE�A_�A\�AZA�AX��AW%ATv�AQ�hANVAK��AIt�AFjAC?}AA`BA@�+A?��A>��A=|�A;��A9��A7��A7K�A6�RA6A4�yA3�A3
=A1ƨA0��A/��A.n�A-�#A-;dA,��A*�RA)�FA({A&��A&A�A%��A%O�A$~�A#��A"�HA" �A!XA �A��A�A~�A-A�hA�\A�;A��A�FA=qAM�Al�A��A5?A�^A�HAZA�A��AoAA�9A��A+A��A�9A��Az�A�A��A
�`A
$�A	oAffA�TA�FA�A/AĜA�A�PA&�Az�A�AVA ��A b@�\)@��R@���@�j@�l�@�7L@�J@�9X@���@�Ĝ@�33@@���@�D@�"�@�-@��/@�;d@���@�A�@�t�@��@�-@��@�;d@ݡ�@�1@�M�@���@�|�@֧�@���@��
@���@��@�O�@�Q�@ϥ�@�=q@̃@�33@�M�@ɉ7@ȣ�@���@Ɵ�@��@���@�J@��F@�V@���@��/@��F@���@��@�G�@���@�1@�K�@���@���@�`B@��D@���@��@�5?@���@��u@��w@�K�@�ȴ@�@�&�@�r�@���@�;d@�
=@��y@��R@�ff@�5?@��@���@�hs@�Ĝ@��@���@�o@��H@�ȴ@��h@�7L@���@���@���@��@��^@��^@���@�7L@���@�1'@�  @���@�9X@��u@��@��u@�z�@�Z@�A�@�(�@��
@�K�@��H@��!@��\@�ff@�{@�x�@��@���@���@��u@�Q�@��
@�|�@�@���@�-@���@�`B@�?}@���@��/@��9@���@���@�bN@�1@��@�dZ@�@�~�@�E�@��@���@�`B@�&�@��@��u@��@��F@��P@�t�@�K�@�"�@�
=@��@��!@�~�@�^5@�5?@�J@��@���@�O�@��@��j@��j@�%@�%@��@�Q�@�b@���@�M�@��@�/@��@��9@�9X@�1@���@��m@���@�K�@�
=@��H@���@��!@���@�E�@�@��T@���@�@���@���@��h@��h@���@�x�@��@��9@���@���@��9@��j@���@��D@���@��D@�I�@��@��w@��@���@���@�
=@���@��\@���@��@��!@�ȴ@��H@��!@�v�@��@��T@���@���@��@�/@���@��;@�K�@��@��\@�-@�{@��^@��@�7L@�V@�V@���@�Ĝ@��@�+@�"�@�K�@�K�@��@���@��H@��R@�v�@��@���@���@���@�@��T@���@�hs@�?}@�%@�Ĝ@�/@��@��@u�@q&�@h�9@`1'@W�@N�R@F�R@?+@8�9@3t�@+t�@&ȴ@"�@�w@��@@M�@�@�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�C�A���A��`AήA΍PA�x�A�`BA�M�A�=qA�1'A�(�A��A�VA�A���A��A��mA���ÁA���A��#A�
=A�{A�dZAć+AÏ\A�C�A�5?A��-A���A�VA�jA���A�1A�A�{A�A�A��PA��A�O�A���A�^5A�VA��A�9XA�O�A���A��A��A�O�A���A�$�A�z�A��7A��TA�n�A�"�A��A��!A�/A���A���A�1'A�M�A�XA��A�G�A��DA���A�/A���A��A�$�A���A��HA�`BA��A�~�A���A��-A�VA�z�A�l�A�+A�K�A��+A�&�A�-A��uA���A�C�A���A���A���A�K�A�VA�"�A�(�A��jA�O�A��uA��wA�bA�E�A�oA�%A��9A�7LA���A��#A��\A�~�A�A�+A~ȴA{�AzJAy�Ay%Awt�Au%As+AqƨAn��Al�DAj�Ae��AbE�A_�A\�AZA�AX��AW%ATv�AQ�hANVAK��AIt�AFjAC?}AA`BA@�+A?��A>��A=|�A;��A9��A7��A7K�A6�RA6A4�yA3�A3
=A1ƨA0��A/��A.n�A-�#A-;dA,��A*�RA)�FA({A&��A&A�A%��A%O�A$~�A#��A"�HA" �A!XA �A��A�A~�A-A�hA�\A�;A��A�FA=qAM�Al�A��A5?A�^A�HAZA�A��AoAA�9A��A+A��A�9A��Az�A�A��A
�`A
$�A	oAffA�TA�FA�A/AĜA�A�PA&�Az�A�AVA ��A b@�\)@��R@���@�j@�l�@�7L@�J@�9X@���@�Ĝ@�33@@���@�D@�"�@�-@��/@�;d@���@�A�@�t�@��@�-@��@�;d@ݡ�@�1@�M�@���@�|�@֧�@���@��
@���@��@�O�@�Q�@ϥ�@�=q@̃@�33@�M�@ɉ7@ȣ�@���@Ɵ�@��@���@�J@��F@�V@���@��/@��F@���@��@�G�@���@�1@�K�@���@���@�`B@��D@���@��@�5?@���@��u@��w@�K�@�ȴ@�@�&�@�r�@���@�;d@�
=@��y@��R@�ff@�5?@��@���@�hs@�Ĝ@��@���@�o@��H@�ȴ@��h@�7L@���@���@���@��@��^@��^@���@�7L@���@�1'@�  @���@�9X@��u@��@��u@�z�@�Z@�A�@�(�@��
@�K�@��H@��!@��\@�ff@�{@�x�@��@���@���@��u@�Q�@��
@�|�@�@���@�-@���@�`B@�?}@���@��/@��9@���@���@�bN@�1@��@�dZ@�@�~�@�E�@��@���@�`B@�&�@��@��u@��@��F@��P@�t�@�K�@�"�@�
=@��@��!@�~�@�^5@�5?@�J@��@���@�O�@��@��j@��j@�%@�%@��@�Q�@�b@���@�M�@��@�/@��@��9@�9X@�1@���@��m@���@�K�@�
=@��H@���@��!@���@�E�@�@��T@���@�@���@���@��h@��h@���@�x�@��@��9@���@���@��9@��j@���@��D@���@��D@�I�@��@��w@��@���@���@�
=@���@��\@���@��@��!@�ȴ@��H@��!@�v�@��@��T@���@���@��@�/@���@��;@�K�@��@��\@�-@�{@��^@��@�7L@�V@�V@���@�Ĝ@��@�+@�"�@�K�@�K�@��@���@��H@��R@�v�@��@���@���@���@�@��T@���@�hs@�?}@�%@�Ĝ@�/@��@��@u�@q&�@h�9@`1'@W�@N�R@F�R@?+@8�9@3t�@+t�@&ȴ@"�@�w@��@@M�@�@�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B�B�B�B�B�B��B��B��B��B��B��B�B�B�B�B�B�B�ZB�BB�B  BbB$�B?}Bp�B|�B� B�bB��B��B��B�B�^B��B��B��B�B�)B�;B�BB�TB�HB�NB�NB�NB�TB�ZB�B�B�B�B�B�`B�BB�5B�5B��B��B��BĜB�wB�RB�B�XB�dB�B��B��B�{B�{B��B�hB�DB�1B�Bz�Br�BbNBT�BF�B49B�BVB+B�B�HB�B��B�}B�!B��B�%BhsBM�B;dB�B
��B
�TB
��B
ƨB
�dB
�B
��B
�1B
v�B
l�B
ffB
T�B
I�B
8RB
,B
"�B
�B
	7B
B
B
B	��B	�sB	�B	��B	�B	�uB	� B	XB	:^B	%�B	�B	PB��B�B�ZB��B�RB��B��B�hB�By�Bx�Bv�Br�Bk�BjBk�Bk�Bs�By�Bz�B{�B{�B|�B}�By�By�B|�Bz�By�Bw�Bx�Br�Bp�Bo�Bm�Bk�BhsBffBffBffBe`Be`Be`Be`BcTBbNBaHB_;B`BB_;B^5B_;B]/BZB\)B[#BZBZBYBXBVBS�BS�BR�BT�BQ�BP�BQ�BR�BS�BS�BS�BR�BR�BQ�BR�BQ�BR�BQ�BP�BP�BP�BN�BM�BM�BK�BF�BC�BB�BA�B@�B?}B>wB;dB:^B:^B6FB33B33B49B6FB9XB;dB:^B<jB>wB?}B<jB8RB7LB6FB6FB6FB5?B33B49B5?B49B33B5?B7LB5?B6FB8RB9XB8RB6FB:^B;dB9XB9XB8RB8RB8RB9XB;dB<jB<jB=qB@�BD�BE�BF�BJ�BN�BO�BP�BQ�BS�BXBZB\)B_;BcTBffBhsBl�Bn�Bp�Bs�Bu�Bw�By�B{�B~�B�B�=B�PB�\B�oB��B��B��B��B��B��B��B��B�B�9B�^B�^B�wB�}B��BBȴB��B��B��B�B�B�B�)B�5B�ZB�B�B�B��B��B��B��B	B	1B	DB	JB	PB	\B	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	&�B	&�B	'�B	&�B	'�B	(�B	.B	1'B	2-B	33B	33B	49B	5?B	7LB	:^B	>wB	B�B	E�B	H�B	M�B	S�B	XB	[#B	\)B	^5B	aHB	bNB	e`B	hsB	k�B	n�B	o�B	q�B	s�B	v�B	x�B	y�B	{�B	}�B	�B	�+B	�=B	�JB	�\B	�uB	�uB	�\B	�bB	�hB	�hB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�!B	�-B	�9B	�RB	�^B	�dB	�qB	�}B	��B	��B	ĜB	ĜB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�#B	�#B	�/B	�5B	�5B	�/B	�)B	�)B	�)B	�5B	�BB	�;B	�BB	�HB	�TB	�ZB	�ZB	�ZB	�ZB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	��B	��B
B
\B
�B
�B
)�B
1'B
8RB
<jB
D�B
G�B
Q�B
W
B
]/B
e`B
l�B
p�B
s�B
w�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B�B�B��B��B��B��B��B��B��B��B��B�B��B�B��B��B��B�B��B1B�B2-BVB{�B�B�=B��B��B��B��B�!BÖB�#B�B�#B�BB�TB�ZB�ZB�yB�sB�B�mB�ZB�fB�sB�B��B��B��B�B�B�`B�mB�mB�B�
B��B��B��BB�?B��BŢB�FB��B��B��B��B��B��B�oB�\B�7B�+B}�Bk�B_;BT�BD�B,B�B�B  B�B�NB�B��B�wB�B��Bz�B`BBVB1'BoB
��B
�;B
��B
ȴB
�jB
��B
��B
~�B
v�B
u�B
gmB
YB
C�B
6FB
0!B
'�B
{B
1B

=B
\B
	7B	��B	�fB	�HB	��B	��B	��B	o�B	N�B	9XB	.B	�B	VB	1B��B�yB��B�qB�dB��B�VB�B� B� B� B{�Bx�Bu�Bq�By�B�B�B�B�B�%B�B�B�B�B�B�B�B�B}�Bx�Bs�Bq�Bp�Bo�Bl�Bl�Bl�Bk�Bl�Bk�BjBgmBffBgmBffBffBgmBiyBk�BiyB`BB`BBaHB^5B`BB]/B[#BZBZB\)B\)B[#BVBR�BR�BS�BVBW
BYBYBZBYBXBVBT�BS�BS�BT�BVBS�BR�BT�BS�BP�BM�BG�BD�BC�BD�BC�BA�BC�BD�B=qB9XB:^B9XB9XB=qB@�B?}BA�BD�BF�BB�B=qB;dB9XB;dB;dB;dB:^B:^B<jB:^B8RB:^B=qB:^B:^B;dB=qB<jB:^B:^BB�B>wB=qB8RB<jB8RB?}BA�B<jBC�BE�B@�BG�BE�BK�BN�BQ�BR�BP�BQ�BW
B[#B]/B_;BbNBgmBjBk�Bo�Br�Bs�Bu�Bu�Bz�By�B~�B�B�B�DB�PB�bB�uB��B��B��B��B��B��B��B��B�B�FB�wB�^B��B�}B��B��BǮB��B��B��B�B�B�B�)B�5B�TB�B�B�B��B��B��B��B	B	
=B	JB	PB	VB	hB	uB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	'�B	&�B	(�B	'�B	(�B	(�B	/B	33B	2-B	49B	5?B	6FB	6FB	8RB	;dB	?}B	C�B	F�B	H�B	O�B	T�B	YB	\)B	]/B	_;B	bNB	bNB	ffB	hsB	k�B	o�B	o�B	r�B	t�B	w�B	x�B	z�B	{�B	}�B	�B	�1B	�=B	�PB	�hB	��B	�uB	�\B	�bB	�hB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�!B	�!B	�-B	�9B	�RB	�^B	�dB	�wB	�}B	��B	B	ŢB	ĜB	ǮB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�#B	�B	�#B	�)B	�)B	�/B	�/B	�)B	�#B	�/B	�;B	�5B	�5B	�/B	�)B	�)B	�;B	�NB	�HB	�BB	�HB	�TB	�`B	�ZB	�ZB	�`B	�ZB	�ZB	�ZB	�`B	�`B	�fB	�fB	�sB	�sB	�sB	�yB	�B	�B	�B	��B	��B
B
\B
�B
�B
)�B
1'B
8RB
<jB
D�B
G�B
Q�B
W
B
^5B
e`B
l�B
p�B
s�B
w�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<���<�o<#�
<#�
<#�
<T��<�9X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<49X<#�
<#�
<e`B<�o<D��<T��<�C�<D��<#�
<#�
<#�
<49X<e`B<e`B<���<�t�<�C�<���<��
<��
<��
<D��<49X<T��<�o<e`B<u<#�
<#�
<u<�t�<u<49X<#�
<T��<�o<49X<#�
<#�
<T��<u<T��<T��<�1<�C�<�1<�/<�j<��
<���<�t�<T��<u<��
<�9X<�j<���<���<�9X<��
<D��<#�
<#�
<#�
<T��<�o<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452062012011014520620120110145206  AO  ARGQ                                                                        20111130143904  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143904  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145206  IP                  G�O�G�O�G�O�                