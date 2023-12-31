CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:40Z UW 3.1 conversion   
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               <A   AO  20111205113400  20190522121836  1901_5055_060                   2C  D   APEX                            2140                            040306                          846 @Ծm��?�1   @Ծn��@-�C��%�c}��l�D1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  Aa��A�  A�33A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B���B���B���B���B���B���B���B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B���B���B���C�fC�fC�fC�fC	�fC  C�C�C  C  C  C  C�C  C  C   C"�C$�C&  C'�fC)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF�CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C]�fC`  Cb�Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cu�fCw�fCy�fC|  C~  C��C�  C��3C�  C��C�  C��3C�  C�  C��C�  C��3C��3C�  C�  C�  C��C��C��C��C��C��C��C�  C��3C��3C�  C�  C��C��C�  C��3C��3C��3C��3C��3C�  C��C�  C�  C��C��C��C��C��3C��3C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C��C��C��C�  C��3C��3C�  C��C��C��C��C�  C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��C�  C��3C�  C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C��3D fD � D  D�fD  D� D  D�fDfD� D��D� D  D�fD  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D�fD  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  Dy�D  D� D   D �fD!  D!y�D"  D"�fD#  D#� D$  D$� D%  D%� D&fD&� D&��D'� D(fD(� D)  D)� D*  D*y�D+  D+� D+��D,� D-fD-� D.  D.� D/fD/� D0  D0�fD1  D1� D2  D2y�D3  D3�fD4fD4� D4��D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DDy�DE  DE� DF  DF� DG  DG�fDH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQfDQ� DQ��DRy�DS  DS�fDTfDT�fDU  DU� DV  DVy�DW  DW� DX  DX� DX��DY� DZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy� D���D�6fD�|�D�� D��D�33D�� D�� D�� D��D�` Dǳ3D�3D�,�D�L�D��D��fD�#3D�c3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @s33@�33@�33A��A9��A[33Ay��A�  A���A���A���A���A���A�  A���BffBffBffBffB&ffB.  B6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~��B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�  B�  B�  C� C� C� C� C	� C��C�3C�3C��C��C��C��C�3C��C��C��C!�3C#�3C%��C'� C)� C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC�3CE�3CG��CI��CK��CM��CO��CQ��CS��CU��CW� CY��C[��C]� C_��Ca�3Cc�3Ce��Cg��Ci��Ck��Cm��Co��Cq�3Cs��Cu� Cw� Cy� C{��C}��C�3C���C�� C���C�ٚC���C�� C���C���C�ٚC���C�� C�� C���C���C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�� C�� C���C���C�ٚC�ٚC���C�� C�� C�� C�� C�� C���C�ٚC���C���C�ٚC�ٚC�ٚC�ٚC�� C�� C���C���C�ٚC���C���C�ٚC���C���C�ٚC���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C�� C���C���C���C�ٚC�ٚC�ٚC�ٚC�ٚC���C�� C�� C���C�ٚC�ٚC�ٚC�ٚC���C�� C�� C�� C�� C�� C�� C�� C�� C���C���C�ٚC���C�� C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C���C���C���C���C���C���C�ٚC�ٚC���C�� C���C���C���C���C�� C�ٚD ffD �fDl�D�fDffD�fDl�D��DffD� DffD�fDl�D�fD` D�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD��DffD�fDl�D�fDffD�fDffD��DffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD� DffD�fD` D�fDffD�fD l�D �fD!` D!�fD"l�D"�fD#ffD#�fD$ffD$�fD%ffD%��D&ffD&� D'ffD'��D(ffD(�fD)ffD)�fD*` D*�fD+ffD+� D,ffD,��D-ffD-�fD.ffD.��D/ffD/�fD0l�D0�fD1ffD1�fD2` D2�fD3l�D3��D4ffD4� D5ffD5�fD6` D6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAl�DA�fDBffDB�fDCffDC�fDD` DD�fDEffDE�fDFffDF�fDGl�DG�fDHffDH�fDI` DI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDO` DO�fDPffDP��DQffDQ� DR` DR�fDSl�DS��DTl�DT�fDUffDU�fDV` DV�fDWffDW�fDXffDX� DYffDY��DZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_� D`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDml�Dm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwffDy�fD�� D�)�D�p D��3D�  D�&fD�s3D��3D��3D�  D�S3DǦfD��fD�  D�@ D� D��D�fD�VfD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A� �A�(�A�(�A�/A�33A�33A�33A�33A�5?A�7LA�9XA�;dA�;dA�=qA�=qA�?}A�?}A�?}A�=qA�=qA�=qA�?}A�?}A�;dA�9XA�=qA�A�A�=qA�?}A�=qA�;dA�9XA�+A���A�$�A���A�n�A�&�A�XA� �A�-A�v�A��hA�oA��DA�x�A��A�M�A�O�A��HA���A��hA�n�A�?}A��7A���A���A�z�A��+A�(�A��A���A�1'A�A��jA�1'A�v�A��
A��TA�S�A��A�A�oA���A�jA�ȴA���A���A��A�^5A��A��+A�-A���A�hsA��#A�`BA�A�33A��A���A�%A�+A��wA���A�x�A�&�AyO�Aq��Al(�Ag�^Ad�/Ab��A]dZAV��AQ33ALVAK�AKG�AJVAG�AB�/AA�A@jA?A?x�A>�`A>ffA:5?A7dZA7�A7�;A6VA3��A2-A2E�A2�uA1�mA/�A.�9A,�A*Q�A(��A&�HA#p�A"-A!"�A  �Ap�AoA%AȴA7LAn�A{A�A\)A�
A�A�HA  A�A�A1A5?AbNA?}A��AE�AA=qA
=A�A��A`BAx�A�A��A;dAVA7LA�A?}A�A5?AA�A  A�hAM�AdZA  A�A�AA��Av�A��Ax�Ar�A-AI�A��A/Av�A�;A�A��A9XAS�A-A/A
E�A
��A
��A
^5A	�TA	O�A�uA~�A-A�AĜA�Ax�A;dAVAv�A�
A�7At�AK�A�uAA�AAdZA ��A ffA {A 1@��m@���@��@�V@�x�@�Z@��@�`B@���@���@��@�$�@�p�@���@�;d@�n�@�@�j@��@�j@��@��@�-@��^@���@���@�"�@�-@陚@�hs@�@��@��@�l�@�C�@�@���@�(�@�o@�v�@��@��@���@�Z@���@�@݁@�G�@�&�@�I�@ۍP@�C�@�
=@�o@��@�v�@�-@�`B@�V@��/@ؼj@�r�@�(�@ם�@�\)@��T@ՙ�@�%@ԋD@�(�@�t�@��@��#@љ�@�p�@�O�@�G�@�/@�Q�@�\)@θR@�v�@�ff@�E�@ͺ^@�X@�&�@���@�Ĝ@�Z@�b@��
@�|�@�@���@ʟ�@�v�@�-@ɺ^@�hs@��@ȴ9@�A�@�  @�\)@Ƨ�@�@��/@ģ�@�bN@���@Ý�@�S�@��@���@���@�$�@���@��h@�p�@�V@�b@���@��P@���@��+@�5?@�{@���@�hs@�/@���@��/@�bN@��w@�S�@�o@��@�v�@�J@�@�`B@��@��9@��@��@���@�S�@�
=@��!@���@�V@��@���@�`B@��@��D@�  @��F@�K�@�o@��@���@�E�@��@�/@�V@�%@���@�%@�%@�9X@���@��
@��m@���@���@�n�@�{@��@���@�x�@��@�z�@��;@�C�@�o@���@��R@�M�@��T@�?}@��j@�j@�1@��m@���@���@���@��@��-@�p�@�%@�Ĝ@�r�@�9X@� �@�  @��w@�l�@�K�@��@��R@�^5@��T@�hs@�%@��9@�Q�@��@���@�"�@��H@��R@��!@���@�~�@�5?@��7@�O�@�V@�Ĝ@�b@���@�K�@�+@�@�v�@�=q@��@���@�`B@���@��9@��@�Z@�1'@��@���@�K�@�@���@�E�@��@��-@�X@���@��u@�A�@�  @���@�l�@�K�@�+@��#@�1'@��H@���@wK�@m@cƨ@[ƨ@Qhs@IX@AG�@:�@4�D@,�D@'\)@!�@�/@l�@��@��@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A� �A�(�A�(�A�/A�33A�33A�33A�33A�5?A�7LA�9XA�;dA�;dA�=qA�=qA�?}A�?}A�?}A�=qA�=qA�=qA�?}A�?}A�;dA�9XA�=qA�A�A�=qA�?}A�=qA�;dA�9XA�+A���A�$�A���A�n�A�&�A�XA� �A�-A�v�A��hA�oA��DA�x�A��A�M�A�O�A��HA���A��hA�n�A�?}A��7A���A���A�z�A��+A�(�A��A���A�1'A�A��jA�1'A�v�A��
A��TA�S�A��A�A�oA���A�jA�ȴA���A���A��A�^5A��A��+A�-A���A�hsA��#A�`BA�A�33A��A���A�%A�+A��wA���A�x�A�&�AyO�Aq��Al(�Ag�^Ad�/Ab��A]dZAV��AQ33ALVAK�AKG�AJVAG�AB�/AA�A@jA?A?x�A>�`A>ffA:5?A7dZA7�A7�;A6VA3��A2-A2E�A2�uA1�mA/�A.�9A,�A*Q�A(��A&�HA#p�A"-A!"�A  �Ap�AoA%AȴA7LAn�A{A�A\)A�
A�A�HA  A�A�A1A5?AbNA?}A��AE�AA=qA
=A�A��A`BAx�A�A��A;dAVA7LA�A?}A�A5?AA�A  A�hAM�AdZA  A�A�AA��Av�A��Ax�Ar�A-AI�A��A/Av�A�;A�A��A9XAS�A-A/A
E�A
��A
��A
^5A	�TA	O�A�uA~�A-A�AĜA�Ax�A;dAVAv�A�
A�7At�AK�A�uAA�AAdZA ��A ffA {A 1@��m@���@��@�V@�x�@�Z@��@�`B@���@���@��@�$�@�p�@���@�;d@�n�@�@�j@��@�j@��@��@�-@��^@���@���@�"�@�-@陚@�hs@�@��@��@�l�@�C�@�@���@�(�@�o@�v�@��@��@���@�Z@���@�@݁@�G�@�&�@�I�@ۍP@�C�@�
=@�o@��@�v�@�-@�`B@�V@��/@ؼj@�r�@�(�@ם�@�\)@��T@ՙ�@�%@ԋD@�(�@�t�@��@��#@љ�@�p�@�O�@�G�@�/@�Q�@�\)@θR@�v�@�ff@�E�@ͺ^@�X@�&�@���@�Ĝ@�Z@�b@��
@�|�@�@���@ʟ�@�v�@�-@ɺ^@�hs@��@ȴ9@�A�@�  @�\)@Ƨ�@�@��/@ģ�@�bN@���@Ý�@�S�@��@���@���@�$�@���@��h@�p�@�V@�b@���@��P@���@��+@�5?@�{@���@�hs@�/@���@��/@�bN@��w@�S�@�o@��@�v�@�J@�@�`B@��@��9@��@��@���@�S�@�
=@��!@���@�V@��@���@�`B@��@��D@�  @��F@�K�@�o@��@���@�E�@��@�/@�V@�%@���@�%@�%@�9X@���@��
@��m@���@���@�n�@�{@��@���@�x�@��@�z�@��;@�C�@�o@���@��R@�M�@��T@�?}@��j@�j@�1@��m@���@���@���@��@��-@�p�@�%@�Ĝ@�r�@�9X@� �@�  @��w@�l�@�K�@��@��R@�^5@��T@�hs@�%@��9@�Q�@��@���@�"�@��H@��R@��!@���@�~�@�5?@��7@�O�@�V@�Ĝ@�b@���@�K�@�+@�@�v�@�=q@��@���@�`B@���@��9@��@�Z@�1'@��@���@�K�@�@���@�E�@��@��-@�X@���@��u@�A�@�  @���@�l�@�K�@�+@��#@�1'@��H@���@wK�@m@cƨ@[ƨ@Qhs@IX@AG�@:�@4�D@,�D@'\)@!�@�/@l�@��@��@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	[#B	[#B	[#B	[#B	ZB	[#B	[#B	[#B	[#B	[#B	[#B	[#B	[#B	\)B	\)B	\)B	\)B	\)B	\)B	\)B	\)B	]/B	^5B	]/B	[#B	[#B	\)B	\)B	]/B	_;B	`BB	aHB	cTB	k�B	��B`BB�B��B�Bw�Bx�BB�;B�B�sB�BDBuBVB��B�B��B��B�B�TB�ZB�`B�mB�BɺBƨB�sB  BB%B�B�BPBVBhB��B��B��B�)BƨBÖB�FB��B��B��B�%Bu�Bo�BgmB^5BK�B.B�B�B	7B
�B
�qB
��B
��B
�B
[#B
$�B	��B	�LB	v�B	VB	C�B	/B	�B��B��B�dB��B��B��B��B�hB�hB��B�B�RB�wBĜBŢBB�RB��B�B�fB�B�B�/B��B�B�B�ZB�B��BB�FB�B��B�B�-B�XB�}B��B��BŢB�}B�^B�XB�?B�9B�LB�LB�dB��BǮB��B��B�)B�B	{B	L�B	�+B	�oB	��B	�RB	ĜB	��B	�B	�/B	�;B	�B	�BB	��B	��B	��B	��B
%B
B	��B	��B
%B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
&�B
'�B
'�B
(�B
.B
/B
.B
,B
'�B
"�B
�B
�B
"�B
$�B
#�B
"�B
 �B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
hB
\B
\B
\B
VB
VB
PB
JB
DB
DB
JB
JB
DB
DB
PB
JB

=B
1B
B
B
B	��B	��B	��B
B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
	7B
1B
	7B

=B
DB
DB
JB
JB
JB
JB
JB
JB
DB
JB
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
hB
oB
uB
uB
oB
uB
uB
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
(�B
/B
5?B
:^B
A�B
G�B
N�B
S�B
XB
^5B
bNB
gmB
jB
m�B
r�B
w�B
y�B
~�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	[#B	[#B	[#B	[#B	ZB	[#B	[#B	[#B	[#B	[#B	[#B	[#B	[#B	\)B	\)B	\)B	\)B	\)B	\)B	\)B	\)B	]/B	^5B	]/B	[#B	[#B	\)B	\)B	]/B	_;B	`BB	aHB	dZB	o�B	�BdZB�BB�HB�jBw�Bp�B��B�BB�B�B�BbB�B�B\B	7B�B
=B��B�B�B��B��B�;B��B��B�B1B
=B
=B'�B0!B�B�B#�BBBDB�yB��B�BÖB�!B��B��B�uB|�Bu�Bq�Bn�B^5B=qB �B%�B#�B
�B
��B
�B
�dB
��B
�DB
H�B
(�B	�yB	��B	r�B	XB	E�B	C�B	$�B��B�B�B�B�B�!B�B��B��B�-B�jBŢB��B�BB��B�RB��B��B��B�HB�B�;B	B	  B��B��B�mB�#B��B��B�XB�3B�3B�LB�jB��BƨB��B��B��BĜBƨB�}B�^B�dB�wBÖBĜBǮB��B��B��B�NB		7B	B�B	�B	�JB	��B	�3B	��B	��B	��B	�;B	�TB	�B	�HB	��B
	7B
B	��B

=B
hB	��B	�B
B
"�B
�B
"�B
$�B
#�B
!�B
!�B
 �B
!�B
�B
�B
,B
,B
.B
.B
33B
2-B
33B
33B
0!B
(�B
 �B
�B
"�B
(�B
'�B
&�B
$�B
!�B
$�B
%�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
bB
\B
bB
\B
\B
\B
bB
hB
hB
PB
PB
hB
hB
PB
PB
1B
%B
B
B	��B
  B
B
B
  B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
%B
%B
B
%B
%B
%B
1B
+B
1B
1B
	7B

=B

=B
	7B
JB
JB
JB
PB
JB
PB
JB
PB
PB
JB
PB
PB
PB
PB
PB
PB
PB
VB
PB
VB
VB
\B
\B
bB
bB
\B
\B
bB
bB
hB
hB
hB
hB
oB
oB
uB
{B
{B
uB
{B
{B
�B
{B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
(�B
/B
5?B
:^B
A�B
H�B
N�B
T�B
YB
^5B
cTB
gmB
k�B
n�B
r�B
w�B
z�B
~�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�C�<���<�h<��
<T��<#�
<T��<�j<�1<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<��
<�C�<D��<�t�<u<u<��
<T��<#�
<�t�<T��<#�
<#�
<e`B<T��<#�
<#�
<#�
<�o<�t�<u<#�
<u<���<���<D��<�j<�9X=+=@�=C�=<j=D��=+<�`B<��
<�9X=#�
='�=�P<���<#�
<#�
<D��<�1<�`B<49X<#�
<#�
<#�
<#�
<D��<���<e`B<#�
<#�
<T��<�o<#�
<#�
<#�
<#�
<u<T��<���<u<T��<�t�<�1<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<�o<#�
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
<D��<49X<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250282012011312502820120113125028  AO  ARGQ                                                                        20111205113400  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113400  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125028  IP                  G�O�G�O�G�O�                