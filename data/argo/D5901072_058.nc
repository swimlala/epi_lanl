CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               :A   AO  20111130144106  20190522121829  1728_5048_058                   2C  D   APEX                            2142                            040306                          846 @Ժ�.�	1   @Ժ�� @5��Q��c�E���1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @&ff@�  @�  A   AffA@  A`  A�  A�  A���A�  A�  A�33A�  A���B ffB  B��B  B ffB(  B0  B8ffB@  BG��BP  BX  B`ffBh  Bo��Bx  B�33B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�33B�  B���B�  B�  B�33B�  B���C�fC  C  C  C
  C�C  C  C�C  C  C  C  C  C  C   C!�fC#�fC%�fC(  C*  C+�fC.  C0  C2  C4  C6�C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CU�fCX  CZ  C\  C^�C`  Cb  Cd�Cf  Ch  Cj  Ck�fCn  Cp  Cq�fCt  Cv  Cw�fCz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��3C��3C�  C�  C�  C��C�  C��3C�  C��C�  C�  C��3C�  C��C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C��3C�  C��C�  C��3C��3C��3C��3C�  C��C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C��C��C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C��C��3C��3C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C�  C��3C�  C�  C��C�  C��3C��3D � DfD� D  D� D  D� D��D� DfD� D��Dy�D��Dy�D  D�fD	fD	�fD
fD
�fD  Dy�D  D� D  D�fDfD�fD  D� D  D�fD  Dy�D  D� DfD� D  D� DfD�fD  D� D��D� D  Dy�D  D� D  D� DfD� D  D�fD  Dy�D  D� D  D� D��D � D!fD!�fD"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(y�D)  D)� D*  D*� D+fD+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D>  D>� D?  D?� D@fD@�fDA  DA� DB  DB� DC  DC� DC��DD� DE  DE� DFfDF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DMy�DM��DN� DO  DO� DP  DP� DQ  DQ�fDR  DRy�DS  DS� DT  DT�fDU  DU� DV  DV�fDW  DWy�DX  DX�fDY  DY� DZ  DZ� DZ��D[y�D[��D\y�D\��D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dgy�Dg��Dhy�Dh��Diy�Di��Dj� Dk  Dk� Dk��Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Dr��Dsy�Dt  Dt� Du  Du� Dv  Dv�fDw  Dwl�Dy�3D�  D�@ D�vfD�� D�� D�,�D�y�D�� D�ٚD�&fD�&fD��fD�� D�3D�vfD๚D��fD��D�P 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@fff@�33@�33A  A9��AY��Ay��A���A���A���A���A�  A���A홚A���BffB  BffB��B&ffB.ffB6��B>ffBF  BNffBVffB^��BfffBn  BvffB~��B�33B�33B�ffB�33B�33B�ffB�33B�33B�33B�33B�  B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�ffB�33B�  B�33B�ffB�33B�  B�33B�33B�ffB�33B�  C� C��C��C��C	��C�3C��C��C�3C��C��C��C��C��C��C��C!� C#� C%� C'��C)��C+� C-��C/��C1��C3��C5�3C7��C9��C;�3C=��C?��CA��CC��CE��CG��CI��CK��CM� CO��CQ��CS��CU� CW��CY��C[��C]�3C_��Ca��Cc�3Ce��Cg��Ci��Ck� Cm��Co��Cq� Cs��Cu��Cw� Cy��C{��C}��C��C�� C���C���C���C���C���C���C���C�ٚC�� C�� C���C���C���C�ٚC���C�� C���C�ٚC���C���C�� C���C�ٚC�ٚC���C���C���C�� C�� C���C���C���C���C���C�� C�� C���C�ٚC���C�� C���C���C�� C���C���C���C�ٚC���C���C�ٚC���C�� C���C���C���C�� C���C���C���C�� C���C�ٚC���C�� C�� C�� C�� C���C�ٚC���C���C���C�ٚC���C���C�ٚC���C���C���C���C���C���C���C���C���C���C�ٚC�ٚC���C���C�ٚC���C���C�ٚC�ٚC���C�� C���C�ٚC���C�� C�� C���C���C���C�ٚC�� C�� C���C���C�� C���C���C�� C���C���C�� C���C���C���C�� C���C���C�ٚC���C�� C�� D ffD ��DffD�fDffD�fDffD� DffD��DffD� D` D� D` D�fDl�D��D	l�D	��D
l�D
�fD` D�fDffD�fDl�D��Dl�D�fDffD�fDl�D�fD` D�fDffD��DffD�fDffD��Dl�D�fDffD� DffD�fD` D�fDffD�fDffD��DffD�fDl�D�fD` D�fDffD�fDffD� D ffD ��D!l�D!�fD"ffD"�fD#ffD#�fD$ffD$�fD%l�D%�fD&ffD&�fD'ffD'�fD(` D(�fD)ffD)�fD*ffD*��D+ffD+�fD,l�D,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0��D1ffD1�fD2ffD2�fD3l�D3�fD4ffD4�fD5ffD5�fD6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=�fD>ffD>�fD?ffD?��D@l�D@�fDAffDA�fDBffDB�fDCffDC� DDffDD�fDEffDE��DFffDF�fDGl�DG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK� DLffDL�fDM` DM� DNffDN�fDOffDO�fDPffDP�fDQl�DQ�fDR` DR�fDSffDS�fDTl�DT�fDUffDU�fDVl�DV�fDW` DW�fDXl�DX�fDYffDY�fDZffDZ� D[` D[� D\` D\� D]` D]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDel�De�fDfffDf�fDg` Dg� Dh` Dh� Di` Di� DjffDj�fDkffDk� DlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr� Ds` Ds�fDtffDt�fDuffDu�fDvl�Dv�fDwS3Dyy�D��3D�33D�i�D��3D��3D�  D�l�D��3D���D��D��Dǹ�D��3D�fD�i�D��D��D��D�C311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�r�A�v�A�z�A�z�A�z�A�v�A�v�A�t�A�v�A�x�A�x�A�x�A�l�A�VA��wA�z�A��`A���A�A���A��FA�-A�&�A��A�oA�  A��yA���A��9A���A��uA�v�A�S�A�-A�1A���A�z�A�XA�/A��A��/A���A���A��TA��A���A���A��yA��TA��HA��A���A��^A�A��-A���A���A���A���A���A��A��A���A��A�1'A�\)A��A�1'A�"�A�9XA�A�A�+A�ȴA��A�9XA���A��A��^A��A�ĜA��#A�7LA�p�A��A�z�A�ȴA�G�A�%A���A��uA��9A��7A��A��`A�&�A�Q�A���A��A���A���A�z�A�9XA�C�A�&�A�bA���A���A��A���A���A�v�A}�
Az�Ay�Ay�7Ax�Ax-Aw�At��As33AqS�Ap�Ao�Amx�Ak"�AjI�Ai|�Af��Ac/Ab1A`��A`ffA^�A]S�A\9XAZ��AXjAV�`ATffAS��AR��AQXAO
=AL��AK/AI�wAHM�AGl�AF��AFM�AE�AD�!ACG�AA�wA@(�A>�A=��A<JA:�!A8r�A6~�A4=qA3�A21'A17LA0��A/p�A-�-A,{A*��A)A)/A(�jA(ffA'�A&�\A$  A"�A!"�AXAM�AG�A��An�AE�A1AXA��A�\AA�A�PA�A��A^5Al�A��A�A33A�9A�A��A�A�A
��A
n�A	��A��A=qAC�A��A��A�jA(�A�PA
=A��A ff@�$�@��`@��;@�o@��@��9@��y@���@� �@�\@��T@�G�@� �@���@�M�@��@�"�@�M�@�`B@�D@��@���@�`B@�|�@���@�V@�Q�@�33@���@��@ۮ@ڏ\@��T@؋D@�K�@ְ!@�$�@��@ӕ�@�=q@щ7@�/@д9@϶F@�v�@�O�@�z�@ˍP@ʧ�@�@���@ɉ7@ȴ9@�A�@�l�@ź^@å�@§�@��@���@���@�K�@��+@��7@���@��@��@���@��9@��m@�
=@��\@�E�@��@���@��@�G�@�&�@��@�Ĝ@�bN@�j@��D@��j@�/@�&�@��j@�|�@���@��m@�;d@��R@���@�o@��H@�M�@��@��-@�A�@�o@�M�@�hs@�A�@�dZ@�V@�J@��h@���@�(�@��F@���@��@���@��+@�=q@�{@���@��@��u@�1'@�1@��w@�dZ@�C�@�+@���@���@��!@���@�V@�J@���@��@��@�r�@�Q�@�Z@�r�@�b@��;@���@��F@�t�@�C�@�+@�+@�C�@�;d@�33@�S�@���@�A�@�j@��D@�r�@�b@�dZ@�o@�l�@�t�@�"�@�ȴ@��y@��@�
=@���@��@���@�V@�@���@�x�@�hs@�?}@���@��9@�bN@�I�@�A�@�1'@�1'@�1'@�(�@��@�1@���@�ƨ@���@�l�@�C�@�o@��@���@��\@�~�@�^5@�J@���@��@�p�@��@���@�j@�1'@�  @��
@���@�C�@��R@�ff@�V@�E�@��@��7@�G�@��@���@�1'@��w@�dZ@�33@�
=@���@�M�@��@���@�O�@�&�@��@���@���@��u@�A�@��@��;@���@�l�@�33@��@��!@��+@�M�@��@��@���@��-@���@��@�`B@�/@�%@��/@��j@��u@��@�bN@�A�@� �@�1@���@���@�S�@�C�@�C�@�33@�o@�
=@�@��@��!@���@�~�@�ff@�M�@�$�@���@�hs@�9X@xb@n{@fv�@`A�@Y�@P��@I7L@C��@>E�@8r�@49X@/
=@(��@#ƨ@ b@J@�j@��@V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�v�A�r�A�v�A�z�A�z�A�z�A�v�A�v�A�t�A�v�A�x�A�x�A�x�A�l�A�VA��wA�z�A��`A���A�A���A��FA�-A�&�A��A�oA�  A��yA���A��9A���A��uA�v�A�S�A�-A�1A���A�z�A�XA�/A��A��/A���A���A��TA��A���A���A��yA��TA��HA��A���A��^A�A��-A���A���A���A���A���A��A��A���A��A�1'A�\)A��A�1'A�"�A�9XA�A�A�+A�ȴA��A�9XA���A��A��^A��A�ĜA��#A�7LA�p�A��A�z�A�ȴA�G�A�%A���A��uA��9A��7A��A��`A�&�A�Q�A���A��A���A���A�z�A�9XA�C�A�&�A�bA���A���A��A���A���A�v�A}�
Az�Ay�Ay�7Ax�Ax-Aw�At��As33AqS�Ap�Ao�Amx�Ak"�AjI�Ai|�Af��Ac/Ab1A`��A`ffA^�A]S�A\9XAZ��AXjAV�`ATffAS��AR��AQXAO
=AL��AK/AI�wAHM�AGl�AF��AFM�AE�AD�!ACG�AA�wA@(�A>�A=��A<JA:�!A8r�A6~�A4=qA3�A21'A17LA0��A/p�A-�-A,{A*��A)A)/A(�jA(ffA'�A&�\A$  A"�A!"�AXAM�AG�A��An�AE�A1AXA��A�\AA�A�PA�A��A^5Al�A��A�A33A�9A�A��A�A�A
��A
n�A	��A��A=qAC�A��A��A�jA(�A�PA
=A��A ff@�$�@��`@��;@�o@��@��9@��y@���@� �@�\@��T@�G�@� �@���@�M�@��@�"�@�M�@�`B@�D@��@���@�`B@�|�@���@�V@�Q�@�33@���@��@ۮ@ڏ\@��T@؋D@�K�@ְ!@�$�@��@ӕ�@�=q@щ7@�/@д9@϶F@�v�@�O�@�z�@ˍP@ʧ�@�@���@ɉ7@ȴ9@�A�@�l�@ź^@å�@§�@��@���@���@�K�@��+@��7@���@��@��@���@��9@��m@�
=@��\@�E�@��@���@��@�G�@�&�@��@�Ĝ@�bN@�j@��D@��j@�/@�&�@��j@�|�@���@��m@�;d@��R@���@�o@��H@�M�@��@��-@�A�@�o@�M�@�hs@�A�@�dZ@�V@�J@��h@���@�(�@��F@���@��@���@��+@�=q@�{@���@��@��u@�1'@�1@��w@�dZ@�C�@�+@���@���@��!@���@�V@�J@���@��@��@�r�@�Q�@�Z@�r�@�b@��;@���@��F@�t�@�C�@�+@�+@�C�@�;d@�33@�S�@���@�A�@�j@��D@�r�@�b@�dZ@�o@�l�@�t�@�"�@�ȴ@��y@��@�
=@���@��@���@�V@�@���@�x�@�hs@�?}@���@��9@�bN@�I�@�A�@�1'@�1'@�1'@�(�@��@�1@���@�ƨ@���@�l�@�C�@�o@��@���@��\@�~�@�^5@�J@���@��@�p�@��@���@�j@�1'@�  @��
@���@�C�@��R@�ff@�V@�E�@��@��7@�G�@��@���@�1'@��w@�dZ@�33@�
=@���@�M�@��@���@�O�@�&�@��@���@���@��u@�A�@��@��;@���@�l�@�33@��@��!@��+@�M�@��@��@���@��-@���@��@�`B@�/@�%@��/@��j@��u@��@�bN@�A�@� �@�1@���@���@�S�@�C�@�C�@�33@�o@�
=@�@��@��!@���@�~�@�ff@�M�@�$�@���@�hs@�9X@xb@n{@fv�@`A�@Y�@P��@I7L@C��@>E�@8r�@49X@/
=@(��@#ƨ@ b@J@�j@��@V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�VB�\B�\B�\B�\B�\B�\B�\B�\B�bB��B��B��B�!B��BPB)�B]/BhsBo�Bt�B�%B�-B��B��B��BB�}B�wB�jB�jB�jB�qB�qB�qB�jB�dB�9B�3B�?B�-B�!B�'B�9B�RB�jB�wBBƨB��B��B��B��B�
B�B�B�B�)B�5B�/B�/B�5B�/B�#B�B��B�XB�B�uB�PB}�Br�Bk�Be`BVBL�BE�B33B&�B#�B�BoB	7BB��B�B�NB�B��B��B�qB�!B��B�1Bv�BiyBZBO�BC�B1'B�BB
�B
�B
��B
�?B
��B
��B
�PB
|�B
k�B
VB
E�B
2-B
-B
)�B
"�B
�B
�B
1B	��B	�B	�B	�fB	�)B	��B	��B	ǮB	�qB	�B	��B	��B	��B	�oB	�\B	�1B	�B	z�B	v�B	m�B	hsB	cTB	ZB	M�B	E�B	;dB	2-B	33B	1'B	2-B	0!B	+B	'�B	�B	�B	JB	B	  B��B�B�yB�BB��B��B��BɺBŢB�wB�LB�B�B��B��B��B��B��B��B�JB�B�B�B�B}�B{�Bz�By�Bv�Bs�Bm�BgmBdZBdZBcTBbNBaHB^5B[#BXBVBT�BS�BP�BL�BN�BL�BJ�BG�BF�BC�BE�BB�BA�B?}B=qB;dB9XB6FB8RB8RB6FB5?B33B1'B0!B0!B0!B/B0!B1'B49B2-B1'B49B2-B/B0!B1'B2-B1'B1'B0!B/B.B-B,B)�B(�B(�B(�B&�B%�B+B0!B1'B2-B49B5?B6FB6FB5?B5?B49B5?B8RB;dB;dB;dB:^B:^B;dB>wB@�B@�BC�BH�BJ�BK�BO�BQ�BS�BVBXBZB\)B_;BbNBdZBgmBjBm�Bo�Bq�Bt�Bw�B� B�%B�1B�7B�=B�JB�bB�{B��B��B��B��B��B��B��B��B�B�3B�9B�3B�3B�B�B�B�B�B�B�-B�jBŢB��B��B��B��B��B��B�B�)B�5B�BB�TB�fB�B�B�B�B��B��B��B��B��B��B��B	B	+B	JB	DB	DB	VB	\B	oB	�B	�B	�B	�B	�B	"�B	%�B	(�B	-B	2-B	8RB	;dB	>wB	C�B	M�B	R�B	W
B	\)B	_;B	aHB	cTB	iyB	m�B	q�B	s�B	v�B	y�B	|�B	~�B	�B	�+B	�JB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�-B	�9B	�9B	�?B	�?B	�LB	�RB	�^B	�^B	�dB	�^B	�dB	�jB	�qB	�wB	�}B	�}B	��B	��B	ÖB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�BB	�HB	�NB	�TB	�TB	�`B	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
1B
uB
�B
�B
,B
33B
:^B
?}B
D�B
J�B
N�B
S�B
XB
^5B
dZB
iyB
n�B
r�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�VB�\B�\B�\B�\B�\B�\B�\B�\B�bB��B��B��B�-B��BVB&�B\)BhsBq�Bx�B�DB�3BBBBÖB��B�}B�qB�qB�wB�}B�}B�}B�}B�wB�FB�?B�LB�3B�'B�'B�3B�RB�jB�wBÖBƨB��B��B��B��B�
B�#B�B�B�)B�5B�/B�/B�;B�5B�5B�5B�#BŢB�dB��B��B��B��B�%Bx�BaHBXB[#BB�B2-B33B+B�B�B\B��B�B�sB�;B�)B�#B��B��B�3B��B�Bs�BdZB\)BQ�BC�B2-B�B
��B
�sB
�/B
ǮB
�!B
��B
��B
�VB
� B
k�B
VB
6FB
1'B
/B
)�B
&�B
$�B
�B
DB	��B	��B	�B	�yB	�B	�B	�)B	��B	�?B	�B	��B	��B	��B	��B	�{B	�oB	�+B	�B	s�B	p�B	p�B	k�B	]/B	P�B	E�B	;dB	9XB	5?B	8RB	6FB	33B	33B	+B	!�B	�B	PB	PB	B	  B��B�B�)B��B��B��B��B��BÖB�RB�'B�B��B��B��B��B��B��B�bB�hB�=B�1B�B}�B|�B}�B~�B� Bx�Bt�BiyBhsBgmBhsBk�Bk�Be`B_;B[#B\)B]/BXBP�BQ�BP�BP�BN�BL�BJ�BK�BI�BH�BD�BB�BA�BB�BB�BA�B=qB:^B9XB8RB7LB7LB6FB6FB49B33B5?B9XB7LB5?B:^B8RB33B49B5?B6FB6FB7LB7LB5?B1'B1'B1'B/B-B.B-B)�B+B/B33B49B7LB9XB:^B6FB8RB8RB:^B9XB:^B8RB?}B;dB=qB:^B:^B;dB>wBD�B@�BC�BH�BJ�BK�BS�BQ�BW
BZB[#BZB\)B_;BffBgmBjBjBo�Bo�Br�Bu�Bw�B�B�+B�7B�7B�=B�JB�bB�{B��B��B��B�B�!B��B��B��B�B�?B�9B�dB�3B�9B�-B�'B�'B�B�-B�-B�jBǮB��B��B��B��B��B��B�B�/B�;B�NB�`B�sB�B�B�B�B��B��B��B��B��B��B	  B	B		7B	VB	PB	JB	\B	\B	oB	�B	�B	�B	�B	 �B	#�B	&�B	(�B	-B	2-B	8RB	;dB	>wB	C�B	M�B	R�B	W
B	\)B	_;B	aHB	cTB	iyB	m�B	q�B	s�B	v�B	z�B	}�B	~�B	�B	�1B	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�?B	�?B	�FB	�FB	�LB	�XB	�^B	�dB	�jB	�dB	�dB	�wB	�wB	�wB	�}B	��B	��B	B	ÖB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�ZB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
1B
{B
�B
�B
-B
33B
:^B
?}B
D�B
J�B
N�B
S�B
XB
^5B
dZB
iyB
o�B
r�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�o<�C�<�/=��=C�<���<���<49X<49X<��
<u<49X<u<D��<#�
<D��<e`B<#�
<#�
<#�
<#�
<#�
<e`B<e`B<�C�<�t�<T��<#�
<#�
<#�
<D��<e`B<�t�<��
<���<e`B<u<�o<�t�<D��<49X<�o<�o<��
<��
<�o<#�
<#�
<#�
<#�
<#�
<T��<T��<D��<#�
<#�
<D��<T��<#�
<#�
<��
<��
<#�
<#�
<#�
<49X<#�
<#�
<49X<�o<D��<T��<#�
<#�
<T��<�C�<u<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<D��<49X<#�
<T��<T��<�C�<u<e`B<#�
<#�
<#�
<#�
<#�
<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<T��<�C�<49X<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<49X<T��<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452132012011014521320120110145213  AO  ARGQ                                                                        20111130144106  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144106  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145213  IP                  G�O�G�O�G�O�                