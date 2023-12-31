CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:52Z UW 3.1 conversion   
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
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135446  20190522121825  1727_5046_002                   2C  D   APEX                            2143                            040306                          846 @�?eԿ�1   @�?�5 @6��S����c�?|�h1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   B   @��@�  @�  @���A   A@  A`  A�  A�  A�  A�  A���A���A�  A�33A�33B  B  B��B��B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�33B�  B�  B�33B�  B���B�  B�33B�  B���B���B���B���B���B�  B�33B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C�fC�fC  C
  C  C  C�fC�fC  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C5�fC7�fC9�fC;�fC=�fC?�fCA�fCD  CF�CH�CJ�CL�CN�CP�CR  CS�fCV  CX�CZ  C\  C^�C`  Cb  Cd�Cf�Ch  Cj�Cl�Cn  Co�fCq�fCt  Cv  Cx  Cz  C|  C~�C��C��C��C��C��C��C�  C��3C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C��C��C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C��3C�  C��C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C��C��C�  C��3C�  C�  C�  C�  C��C�  C�  C��C�  C��3C�  C��C�  C��3C�  C�  C�  C��3C�  C��C��3C��3C�  C�  C�  C�  C��3C��C��C�  C��C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C��C��C��C��C��3C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	�fD
  D
y�D  D� D��D� D  D� D  D� DfD� D  Dy�D��D� D  D� D  D� D��D� DfD�fD  D�fDfD�fDfD�fDfD�fD  D� D  D� D  D� D  D�fDfD�fDfD�fD fD �fD!  D!� D"  D"� D"��D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(fD(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-�fD.  D.� D/fD/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6� D6��D7� D8  D8� D9  D9� D:fD:� D;  D;� D<  D<y�D=  D=�fD>fD>� D?  D?�fD@  D@� DAfDA� DA��DB� DC  DC� DD  DD�fDE  DE� DE��DFy�DF��DGy�DH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL�fDM  DMy�DN  DN� DOfDO� DO��DP� DQ  DQ�fDR  DR� DS  DS� DT  DT�fDU  DUy�DV  DV� DW  DWy�DX  DX� DY  DY�fDZ  DZ� D[  D[� D\fD\� D]  D]� D]��D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dk��Dly�Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Dr��Dsy�Dt  Dt� Du  Du� DvfDv� Dw  Dws3Dy��D�@ D�vfD���D��3D��D�vfD���D���D�&fD�vfD�� D��3D�&fD�VfDډ�D��D�)�D�` D� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@s33@���@�ffA��A<��A\��A|��A�ffA�ffA�ffA�33A�33A�ffA홚A���B33B33B��B��B'33B/33B733B>��BG33BO33BW33B_33Bg33Bo33Bw��B33B���B���B���B���B���B���B�ffB���B���B���B�ffB�ffB�ffB�ffB�ffB���B���BǙ�B˙�B���B���Bי�Bۙ�Bߙ�B㙚B癚B뙚B���B�B���B���B���C��C�3C�3C��C	��C��C��C�3C�3C��C��C��C��C��C�fC��C!��C#��C%��C'��C)��C+��C-��C/��C1�fC3��C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC��CE�fCG�fCI�fCK�fCM�fCO�fCQ��CS�3CU��CW�fCY��C[��C]�fC_��Ca��Cc�fCe�fCg��Ci�fCk�fCm��Co�3Cq�3Cs��Cu��Cw��Cy��C{��C}�fC�fC��3C��3C��3C��3C��3C��fC�ٚC��fC��fC��fC��fC��fC��fC��3C��3C��3C��3C��3C��fC�ٚC��fC��fC��fC��fC��3C��fC�ٚC��fC��fC��fC��fC��fC��fC�ٚC��3C��3C�ٚC�ٚC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��3C��fC�ٚC�ٚC��fC��3C��fC��fC��fC��3C��fC�ٚC��fC��fC�ٚC��fC��fC��fC��fC�ٚC�ٚC��fC��fC��3C��3C��fC�ٚC��fC��fC��fC��fC��3C��fC��fC��3C��fC�ٚC��fC��3C��fC�ٚC��fC��fC��fC�ٚC��fC��3C�ٚC�ٚC��fC��fC��fC��fC�ٚC��3C��3C��fC��3C�ٚC�ٚC��fC��fC��3C��fC��fC��fC��fC��fC��3C��3C��3C��3C�ٚC��fC��fC��fC��fC��3C��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dl�D�3Ds3D�3D	y�D	�3D
l�D
�3Ds3D��Ds3D�3Ds3D�3Ds3D��Ds3D�3Dl�D��Ds3D�3Ds3D�3Ds3D��Ds3D��Dy�D�3Dy�D��Dy�D��Dy�D��Dy�D�3Ds3D�3Ds3D�3Ds3D�3Dy�D��Dy�D��Dy�D��D y�D �3D!s3D!�3D"s3D"��D#s3D#�3D$s3D$�3D%y�D%�3D&s3D&�3D's3D'��D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,l�D,�3D-y�D-�3D.s3D.��D/y�D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5��D6s3D6��D7s3D7�3D8s3D8�3D9s3D9��D:s3D:�3D;s3D;�3D<l�D<�3D=y�D=��D>s3D>�3D?y�D?�3D@s3D@��DAs3DA��DBs3DB�3DCs3DC�3DDy�DD�3DEs3DE��DFl�DF��DGl�DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKl�DK�3DLy�DL�3DMl�DM�3DNs3DN��DOs3DO��DPs3DP�3DQy�DQ�3DRs3DR�3DSs3DS�3DTy�DT�3DUl�DU�3DVs3DV�3DWl�DW�3DXs3DX�3DYy�DY�3DZs3DZ�3D[s3D[��D\s3D\�3D]s3D]��D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De��Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di��Djs3Dj�3Dks3Dk��Dll�Dl��Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drl�Dr��Dsl�Ds�3Dts3Dt�3Dus3Du��Dvs3Dv�3DwffDy� D�9�D�p D��fD���D�3D�p D��3D��fD�  D�p D���D���D�  D�P Dڃ3D��3D�#3D�Y�D�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�(�A�v�A°!A�l�A�JA�Q�A��A�A���A��HA�ȴA��9A�`BA���A���A��A���A�r�A���A��A��mA�=qA���A�A��9A���A���A�|�A�/A�A�ƨA���A��DA�x�A��A���A��!A�ffA�9XA��A�A��A���A��A��hA���A�ZA��A��!A��#A�A���A���A�z�A�r�A��7A�z�A��/A��A���A��PA�JA�/A���A��^A�C�A�C�A�A�C�A���A���A�G�A�ZA��A��jA���A��A��PA�
=A��jA�(�A�I�A�K�A�p�A�{A���A�r�A�ȴA�oA�x�A�+A�r�A�7LA�"�A�x�A�"�A�9XA�
=A}��A}S�A|v�A{oAx�HAwdZAul�Ar�\Ap1An�DAm�AmhsAlA�Ak`BAi�AhbNAfQ�Ad��Ad�A`�\A]`BA\�AZM�AY��AYS�AX�AW�AS�^AR~�AQƨAP��AOx�AM�mAM+AL��AL^5AK�TAJ��AG�;AGoAF�jAFAEK�ADI�ACXAB��AB1'AAS�A@Q�A?l�A=�PA;%A9�TA9�wA9oA85?A8  A7l�A6�/A6��A6$�A5�A4�uA3�mA2�9A0��A/A.jA-A-&�A,1A*9XA(�/A'��A&�9A&=qA&�A%�mA%|�A$�A$��A$�A$A�A#�TA#�A"A!�
A!��A!/A �uA�jA �At�A��A-A�A�mA"�A��A�-A�jA��A��A�HA�#AG�A��AQ�AAbNA	��A5?A�hAK�A��A��A=qA��Ar�AC�AbNA  A@��F@��H@�ff@��@��@�\)@�^5@��`@��@�@�b@�$�@���@�R@�^@�dZ@�h@���@��@���@�o@��@ܓu@���@ۅ@۶F@���@�S�@��@�v�@�=q@�r�@ם�@�n�@�|�@Ͼw@ͩ�@�%@��@˝�@˕�@���@�9X@�M�@���@��m@ģ�@Ł@��@�/@�t�@���@�1'@�1@��@�b@��@��y@�|�@�b@�?}@��@�o@��9@�5?@�%@�1'@��@�j@��@��j@���@��@��#@��@�5?@���@���@���@�K�@���@�Ĝ@���@�V@�=q@�@�E�@�=q@��T@��@���@���@�ȴ@��!@�v�@�@��7@��m@�Z@��T@�@��^@���@��h@��@���@�E�@��!@��H@��H@��\@�X@�|�@�+@��`@��@�|�@�$�@�%@��9@��@���@�X@���@��@�?}@�&�@�x�@��h@���@��F@�`B@�7L@�  @�ff@��h@��/@��@�Z@�z�@��@���@�V@��w@��!@��7@��@�%@��9@�A�@���@�33@���@���@���@��+@�^5@�$�@���@���@��@�G�@���@��@��@�$�@���@�/@���@�Q�@� �@��
@���@�K�@�"�@�o@�
=@��@��@���@�=q@�$�@�-@��#@��h@��@�&�@�Q�@�1'@��@��F@�dZ@�33@���@���@���@�=q@��@�J@��T@��7@��7@�x�@�7L@��9@�bN@�A�@�1'@�b@��m@��P@�@�M�@�V@��#@�/@�&�@�hs@�x�@��@�%@�r�@�j@�A�@�1@��F@�t�@�"�@��R@�^5@�M�@�=q@�$�@��^@�/@���@��@��9@��D@�9X@�1'@�(�@��
@��P@�
=@���@�M�@�$�@��T@���@��/@�Q�@��@;d@~V@}V@{dZ@z��@z��@zM�@y��@xbN@w�w@w;d@w�@vȴ@vff@v5?@v@t�/@t�@t1@s��@s@r~�@k�F@jn�@d�@Y&�@VV@M/@Fff@A�^@:^5@2��@,j@'�;@"M�@|�@dZ@�y@��@��@�-@	G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�(�A�v�A°!A�l�A�JA�Q�A��A�A���A��HA�ȴA��9A�`BA���A���A��A���A�r�A���A��A��mA�=qA���A�A��9A���A���A�|�A�/A�A�ƨA���A��DA�x�A��A���A��!A�ffA�9XA��A�A��A���A��A��hA���A�ZA��A��!A��#A�A���A���A�z�A�r�A��7A�z�A��/A��A���A��PA�JA�/A���A��^A�C�A�C�A�A�C�A���A���A�G�A�ZA��A��jA���A��A��PA�
=A��jA�(�A�I�A�K�A�p�A�{A���A�r�A�ȴA�oA�x�A�+A�r�A�7LA�"�A�x�A�"�A�9XA�
=A}��A}S�A|v�A{oAx�HAwdZAul�Ar�\Ap1An�DAm�AmhsAlA�Ak`BAi�AhbNAfQ�Ad��Ad�A`�\A]`BA\�AZM�AY��AYS�AX�AW�AS�^AR~�AQƨAP��AOx�AM�mAM+AL��AL^5AK�TAJ��AG�;AGoAF�jAFAEK�ADI�ACXAB��AB1'AAS�A@Q�A?l�A=�PA;%A9�TA9�wA9oA85?A8  A7l�A6�/A6��A6$�A5�A4�uA3�mA2�9A0��A/A.jA-A-&�A,1A*9XA(�/A'��A&�9A&=qA&�A%�mA%|�A$�A$��A$�A$A�A#�TA#�A"A!�
A!��A!/A �uA�jA �At�A��A-A�A�mA"�A��A�-A�jA��A��A�HA�#AG�A��AQ�AAbNA	��A5?A�hAK�A��A��A=qA��Ar�AC�AbNA  A@��F@��H@�ff@��@��@�\)@�^5@��`@��@�@�b@�$�@���@�R@�^@�dZ@�h@���@��@���@�o@��@ܓu@���@ۅ@۶F@���@�S�@��@�v�@�=q@�r�@ם�@�n�@�|�@Ͼw@ͩ�@�%@��@˝�@˕�@���@�9X@�M�@���@��m@ģ�@Ł@��@�/@�t�@���@�1'@�1@��@�b@��@��y@�|�@�b@�?}@��@�o@��9@�5?@�%@�1'@��@�j@��@��j@���@��@��#@��@�5?@���@���@���@�K�@���@�Ĝ@���@�V@�=q@�@�E�@�=q@��T@��@���@���@�ȴ@��!@�v�@�@��7@��m@�Z@��T@�@��^@���@��h@��@���@�E�@��!@��H@��H@��\@�X@�|�@�+@��`@��@�|�@�$�@�%@��9@��@���@�X@���@��@�?}@�&�@�x�@��h@���@��F@�`B@�7L@�  @�ff@��h@��/@��@�Z@�z�@��@���@�V@��w@��!@��7@��@�%@��9@�A�@���@�33@���@���@���@��+@�^5@�$�@���@���@��@�G�@���@��@��@�$�@���@�/@���@�Q�@� �@��
@���@�K�@�"�@�o@�
=@��@��@���@�=q@�$�@�-@��#@��h@��@�&�@�Q�@�1'@��@��F@�dZ@�33@���@���@���@�=q@��@�J@��T@��7@��7@�x�@�7L@��9@�bN@�A�@�1'@�b@��m@��P@�@�M�@�V@��#@�/@�&�@�hs@�x�@��@�%@�r�@�j@�A�@�1@��F@�t�@�"�@��R@�^5@�M�@�=q@�$�@��^@�/@���@��@��9@��D@�9X@�1'@�(�@��
@��P@�
=@���@�M�@�$�@��T@���@��/@�Q�@��@;d@~V@}V@{dZ@z��@z��@zM�@y��@xbN@w�w@w;d@w�@vȴ@vff@v5?@v@t�/@t�@t1@s��@s@r~�@k�F@jn�@d�@Y&�@VV@M/@Fff@A�^@:^5@2��@,j@'�;@"M�@|�@dZ@�y@��@��@�-@	G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B�}B��B��B��BĜBǮBȴB��B��B��B��B�B�5B�BB�ZB�BB�#B�B��B��BƨBBÖBĜBĜBĜBɺB��B�
B�B�/B�5B�5B�)B�B�B�B�B�B�#B�/B�;B�ZB�ZB�HB�NB�)B�-B��By�Bm�BiyBbNBVBG�B;dB#�B{B	7B��B�B�TB�;B�B��BB�dB�3B�B��B��B�{B�B|�Bu�BbNBL�B:^B&�BoBDB
��B
�B
�B
�ZB
�TBVBB
�B
�
B
��B
��B
��B
�7B
|�B
x�B
hsB
]/B
ZB
Q�B
F�B
5?B
'�B
�B
B	�B	�B	�B	��B	�B	�BB	��B	��B	ĜB	�XB	�LB	��B	�1B	�B	o�B	o�B	k�B	dZB	^5B	P�B	M�B	K�B	H�B	F�B	N�B	P�B	L�B	J�B	E�B	@�B	6FB	1'B	.B	(�B	#�B	 �B	�B	�B	{B	bB	JB	+B	B��B��B�B�B�B�B�B�sB�fB�NB�5B�5B�#B��B��B��BĜB�}B�jB�LB�3B�B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�oB�oB��B�oB�PB�7B�B�B� B~�B|�Bw�Bt�Bo�Bk�BiyBhsBjBiyBgmBffBdZB`BB\)B[#BYBXBW
BVBS�BQ�BO�BL�BK�BI�BG�BG�BE�BC�B@�B=qB8RB9XB8RB5?B2-B1'B/B.B.B-B+B+B,B)�B'�B)�B(�B)�B)�B-B1'B8RB@�B;dBL�BM�BK�BD�B?}B8RB/B+B+B+B+B/B8RB8RB8RB<jB>wBA�BF�BL�BP�BN�BJ�BL�BP�BP�BS�BR�BVB_;BdZBiyBp�Bp�Bk�Be`B`BB_;BdZBgmBm�Bs�B|�B�B�7B�oB�{B��B��B��B��B��B��B��B��B�B�B�-B�?B�FB�9B�XB�dB�wB�wB�qB�dB�RB�9B�B��B��B��B��B��B��B��B�B�XB�}BÖBŢBǮBȴB��B�
B�BB�;B�/B�;B�HB�HB�TB�B�B�B�B��B��B��B	  B	  B��B��B��B��B��B��B��B��B	B	B	B	hB	�B	�B	�B	�B	�B	 �B	!�B	!�B	%�B	,B	/B	0!B	2-B	33B	5?B	9XB	=qB	=qB	?}B	B�B	F�B	I�B	M�B	R�B	S�B	S�B	W
B	ZB	\)B	\)B	]/B	^5B	_;B	aHB	bNB	cTB	e`B	hsB	k�B	p�B	r�B	r�B	s�B	s�B	v�B	y�B	y�B	y�B	y�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�+B	�7B	�=B	�DB	�PB	�bB	�hB	�oB	�oB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�FB	�FB	�LB	�XB	�XB	�^B	�dB	�qB	�}B	B	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�NB	��B
bB
�B
�B
+B
)�B
6FB
>wB
F�B
K�B
O�B
VB
\)B
aHB
ffB
jB
n�B
p�B
t�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BɺBŢBÖBŢBƨBƨBȴBɺB��B��B��B��B�)B�HB�`B�sB�`B�ZB�/B�B��B��BÖBĜBŢBŢBƨB��B�B�B�)B�;B�HB�`B�mB�)B�/B�5B�BB�ZB�NB�NB�mB�B�B�B��B�BǮB�'B�Bp�Bo�Bn�BbNBVBL�B0!B�B{B  B��B�fB�TB�HB�)BȴB��B�RB�!B�B�B��B�7B�B�=Bs�B_;BK�B:^B�B�B%B
�B
�B
�yB
�G�O�B�B
��B
�`B
�yB
�sB
�jB
��B
�DB
�1B
t�B
bNB
bNB
^5B
VB
B�B
9XB
-B
hB	��B	�B	��B
  B	��B	�B	�/B	�/B	��B	ĜB	��B	�-B	�hB	�bB	s�B	t�B	s�B	t�B	r�B	ZB	T�B	S�B	R�B	P�B	S�B	T�B	P�B	Q�B	P�B	Q�B	;dB	5?B	49B	/B	,B	'�B	"�B	�B	�B	�B	�B	�B	hB	B��B��B��B�B�B�B�B�B�sB�`B�`B�ZB�BB�B��BɺBƨBŢBĜB�jB�LB�!B�B��B��B��B��B��B��B��B��B��B��B�uB�{B��B��B��B�oB�VB�7B�%B�B�B�%B�%B~�By�Bz�Bw�Bv�Bq�Bn�Bl�BjBk�Bn�Bn�BdZB^5B[#B[#BYBXBYBYBT�BR�BN�BO�BN�BI�BG�BI�BF�B@�B>wB?}B<jB9XB5?B6FB6FB33B2-B49B1'B1'B1'B2-B0!B/B-B-B.B2-B=qBH�B49BL�BP�BQ�BI�BF�BC�B<jB1'B.B.B-B1'B?}B?}B@�BB�B>wB?}BD�BL�BVBN�BP�BP�BQ�BP�BVBR�BT�B]/BdZBgmBs�Bx�Bt�Bm�B`BBbNBdZBgmBm�Bs�B|�B~�B�+B�oB�{B��B��B��B��B��B��B��B��B�B�B�-B�FB�XB�dB�jB�dB�wB�}B�qB�dB�RB�9B�B��B��B��B��B��B��B��B�B�RB�}BĜBŢB��B��B��B��B�BB�TB�HB�NB�NB�HB�HB�B�B�B�B��B��B��B	  B	  B	B��B	B	B��B��B��B��B	B	B	B	hB	�B	�B	�B	�B	�B	"�B	#�B	#�B	%�B	-B	/B	1'B	33B	33B	6FB	9XB	=qB	>wB	?}B	B�B	E�B	I�B	M�B	T�B	S�B	T�B	YB	[#B	]/B	\)B	^5B	_;B	_;B	aHB	bNB	cTB	ffB	iyB	k�B	p�B	s�B	s�B	t�B	s�B	x�B	y�B	z�B	y�B	z�B	{�B	{�B	}�B	}�B	� B	�B	�B	�%B	�1B	�7B	�DB	�JB	�PB	�hB	�hB	�oB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�?B	�LB	�LB	�RB	�^B	�XB	�dB	�jB	�}B	�}B	B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�B	�)B	�)B	�#B	�NB	��B
bB
�B
�B
+B
)�B
6FB
>wB
F�B
K�B
O�B
VB
]/B
aHB
ffB
jB
n�B
p�B
u�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<#�
<#�
<#�
<#�
<#�
<T��<�/<�j<�1<��
<#�
<#�
<#�
<D��<D��<e`B<�C�<D��<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<��
<�C�<�t�<�C�<�t�<49X<D��<#�
<#�
<#�
<#�
<#�
G�O�<�1<�C�<e`B<�1<�/<�9X<�t�<e`B<u<D��<#�
<#�
<D��<u<T��<�C�<�1<u<#�
<#�
<#�
<#�
<#�
<u<#�
<u<T��<49X<ě�<���<#�
<T��<#�
<#�
<#�
<�o<��
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<u<u<#�
<#�
<#�
<#�
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
<e`B<#�
<#�
<u<e`B<T��<#�
<#�
<#�
<#�
<#�
<T��<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446342012010314463420120103144634  AO  ARGQ                                                                        20111130135446  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135446  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144634  IP                  G�O�G�O�G�O�                