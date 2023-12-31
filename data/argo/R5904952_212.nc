CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:53Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190553  20181005190553  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i�.41   @��j����@1kƧ�c��j~��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�  @���A   A!��AA��Aa��A���A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B ffB(ffB0  B8  B@  BH  BPffBX  B`  Bg��Bo��Bx  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C��C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  D   D �fD  D� D  D� DfD�fD  D� D  D� D  Dy�D  D� D��D� D	  D	�fD
fD
�fDfD� D  D� D  Dy�D��D� D  D� DfD� D  D�fD  D�fDfD� D  D� D  D� D  D� D  D� D  D�fDfD�fD  D� D  D�fD  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#fD#�fD$  D$� D%fD%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*y�D*��D+� D,  D,� D-  D-�fD.  D.� D/  D/� D0  D0�fD1fD1� D1��D2� D3  D3y�D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9fD9�fD:  D:y�D;  D;� D;��D<� D=  D=y�D>  D>� D?  D?�fD@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD��DE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR��DS� DTfDT� DU  DUy�DU��DVy�DW  DW�fDW��DXy�DY  DY� DY��DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf�fDg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Dsy�Ds��Dt� DufDu� DvfDv� Dw  Dw� Dw��Dy��D�T)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�33A33A$��AD��Ad��A�fgA���A���A���A���Aљ�AᙚA�B ��B��B33B��B!33B)33B0��B8��B@��BH��BQ33BX��B`��BhfgBpfgBx��B���B�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�33B�33B�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffC.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC�&gC��C��C��C��C��C��C��C��C��C��C�&gC�&gC��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��D �D �3D�D��D�D��D3D�3D�D��D�D��D�D�gD�D��DgD��D	�D	�3D
3D
�3D3D��D�D��D�D�gDgD��D�D��D3D��D�D�3D�D�3D3D��D�D��D�D��D�D��D�D��D�D�3D3D�3D�D��D�D�3D�D��D�D��D�D��D3D��D �D ��D!�D!��D"�D"��D#3D#�3D$�D$��D%3D%��D&gD&��D'�D'��D(�D(��D)�D)��D*�D*�gD+gD+��D,�D,��D-�D-�3D.�D.��D/�D/��D0�D0�3D13D1��D2gD2��D3�D3�gD4�D4��D5�D5��D6�D6��D7�D7��D8gD8��D93D9�3D:�D:�gD;�D;��D<gD<��D=�D=�gD>�D>��D?�D?�3D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DEgDE��DF�DF��DG�DG�gDH�DH��DI�DI��DJ�DJ��DK�DK�gDL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DSgDS��DT3DT��DU�DU�gDVgDV�gDW�DW�3DXgDX�gDY�DY��DZgDZ�gD[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��DagDa��Db�Db��Dc�Dc��Dd�Dd��De�De��Df3Df�3Dg�Dg��Dh�Dh��Di�Di�3Dj�Dj��Dk�Dk��Dl�Dl�3Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr�3Ds�Ds�gDtgDt��Du3Du��Dv3Dv��Dw�Dw��DwٚDy��D�Z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7LA�5?A�33A�/A�-A�/A�1'A�33A�/A�/A�/A�/A�/A�/A�-A�/A�5?A�=qA�?}A�=qA�K�A�VA�^5A�I�A�K�A�M�A�Q�AǴ9A���A��A�%A�
=A�
=A�oA�+A�33A�33A�5?A�=qA�A�A�A�A�C�A�A�A�?}A�;dA�7LA��AǙ�Aƺ^Aƙ�A��A��yA��TA��HA��TA��mA��A�  A��A���A�JAƴ9A��mA�1'A���A���A�oA���A�1'A���A��!A�%A���A��FA�v�A���A�v�A�$�A��HA���A�~�A��A��A�Q�A���A��wA�bNA�ĜA���A�jA�`BA��jA��A�;dAw�wAuC�At�As�An��Akx�Ahz�Af�Ad-Aax�A`jA_�wA^�/A\��AXn�AU�;AR�RAP��AO��AMAL  AI
=AE��AD�/AChsAA�A=��A;A:^5A9�#A7oA4ffA1��A0ffA/�A.bA-C�A,=qA*jA(bNA%x�A#�#A!�A!\)A!/A;dA/A~�AƨA�hA�9A$�A�jA�wAn�A��A��A�wA��A�#A�jA/A��A	�wA��AbNAZA^5AXA��AA�A�/AA�A\)A�A�yA��Az�A-A|�A�A��A bN@��w@�\)@��+@�v�@�V@�M�@�{@��@��w@���@��#@�?}@�K�@���@��@��@�Ĝ@�u@�Q�@�K�@��H@@��@�S�@���@�5?@�z�@�l�@�h@�P@���@��D@� �@���@܃@۝�@ۅ@�t�@�n�@��#@�{@�5?@�v�@���@��@�&�@�dZ@�j@�(�@ߝ�@��y@�V@�t�@��@�-@�`B@�hs@�7L@���@�33@��@�Z@ӍP@�=q@�Z@��@·+@͙�@̋D@��@ˍP@��H@ʇ+@ʗ�@ʰ!@��H@���@ʏ\@ɲ-@�p�@���@�@Ł@��@ċD@�I�@�bN@�Q�@�I�@�Q�@ēu@�bN@���@�K�@�I�@�  @���@Õ�@���@�{@���@�bN@�A�@�  @�ƨ@�dZ@�Z@���@�5?@���@��P@��@�E�@��R@���@��;@��R@�v�@�{@��T@��#@���@���@�O�@�Ĝ@�9X@�(�@�I�@�Ĝ@�7L@��@��@�Z@�A�@�Q�@�I�@��@�l�@���@�M�@��#@��#@�`B@�1@��
@��;@��F@�+@�ff@�-@��@��^@��@�7L@��@�z�@�1@�l�@�"�@��@�@���@��R@��R@�{@��-@���@���@���@���@�hs@�V@���@���@�Ĝ@�r�@� �@��@�C�@�C�@�33@�+@��@�^5@�{@��@�{@���@��7@�V@�bN@��@�  @��;@��@�^5@�~�@�V@��^@��h@�x�@�p�@�`B@�G�@��@��`@��@�t�@�
=@��H@���@��@��R@�^5@���@�X@��@���@��9@�1'@�b@�1@�1@��w@�t�@�C�@��@�5?@��@�hs@�%@���@���@��@�Q�@�b@�  @�  @���@�\)@�o@�
=@�
=@���@��!@���@�E�@���@���@�?}@�V@�j@�(�@��;@���@�l�@�C�@��@��y@���@��@��7@�`B@�7L@�V@���@��D@�Q�@���@�|�@�l�@�C�@��@�M�@�-@�@��@���@��-@���@��h@��@�x�@�`B@�?}@�7L@��@�j@���@���@���@��P@�33@��@��R@�%�@{J#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7LA�5?A�33A�/A�-A�/A�1'A�33A�/A�/A�/A�/A�/A�/A�-A�/A�5?A�=qA�?}A�=qA�K�A�VA�^5A�I�A�K�A�M�A�Q�AǴ9A���A��A�%A�
=A�
=A�oA�+A�33A�33A�5?A�=qA�A�A�A�A�C�A�A�A�?}A�;dA�7LA��AǙ�Aƺ^Aƙ�A��A��yA��TA��HA��TA��mA��A�  A��A���A�JAƴ9A��mA�1'A���A���A�oA���A�1'A���A��!A�%A���A��FA�v�A���A�v�A�$�A��HA���A�~�A��A��A�Q�A���A��wA�bNA�ĜA���A�jA�`BA��jA��A�;dAw�wAuC�At�As�An��Akx�Ahz�Af�Ad-Aax�A`jA_�wA^�/A\��AXn�AU�;AR�RAP��AO��AMAL  AI
=AE��AD�/AChsAA�A=��A;A:^5A9�#A7oA4ffA1��A0ffA/�A.bA-C�A,=qA*jA(bNA%x�A#�#A!�A!\)A!/A;dA/A~�AƨA�hA�9A$�A�jA�wAn�A��A��A�wA��A�#A�jA/A��A	�wA��AbNAZA^5AXA��AA�A�/AA�A\)A�A�yA��Az�A-A|�A�A��A bN@��w@�\)@��+@�v�@�V@�M�@�{@��@��w@���@��#@�?}@�K�@���@��@��@�Ĝ@�u@�Q�@�K�@��H@@��@�S�@���@�5?@�z�@�l�@�h@�P@���@��D@� �@���@܃@۝�@ۅ@�t�@�n�@��#@�{@�5?@�v�@���@��@�&�@�dZ@�j@�(�@ߝ�@��y@�V@�t�@��@�-@�`B@�hs@�7L@���@�33@��@�Z@ӍP@�=q@�Z@��@·+@͙�@̋D@��@ˍP@��H@ʇ+@ʗ�@ʰ!@��H@���@ʏ\@ɲ-@�p�@���@�@Ł@��@ċD@�I�@�bN@�Q�@�I�@�Q�@ēu@�bN@���@�K�@�I�@�  @���@Õ�@���@�{@���@�bN@�A�@�  @�ƨ@�dZ@�Z@���@�5?@���@��P@��@�E�@��R@���@��;@��R@�v�@�{@��T@��#@���@���@�O�@�Ĝ@�9X@�(�@�I�@�Ĝ@�7L@��@��@�Z@�A�@�Q�@�I�@��@�l�@���@�M�@��#@��#@�`B@�1@��
@��;@��F@�+@�ff@�-@��@��^@��@�7L@��@�z�@�1@�l�@�"�@��@�@���@��R@��R@�{@��-@���@���@���@���@�hs@�V@���@���@�Ĝ@�r�@� �@��@�C�@�C�@�33@�+@��@�^5@�{@��@�{@���@��7@�V@�bN@��@�  @��;@��@�^5@�~�@�V@��^@��h@�x�@�p�@�`B@�G�@��@��`@��@�t�@�
=@��H@���@��@��R@�^5@���@�X@��@���@��9@�1'@�b@�1@�1@��w@�t�@�C�@��@�5?@��@�hs@�%@���@���@��@�Q�@�b@�  @�  @���@�\)@�o@�
=@�
=@���@��!@���@�E�@���@���@�?}@�V@�j@�(�@��;@���@�l�@�C�@��@��y@���@��@��7@�`B@�7L@�V@���@��D@�Q�@���@�|�@�l�@�C�@��@�M�@�-@�@��@���@��-@���@��h@��@�x�@�`B@�?}@�7L@��@�j@���@���@���@��P@�33@��@��R@�%�@{J#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�9B
�jB
B
ȴB
ɺB
��B
��B
��B
�B
�B
�
B
�B
�#B
�)B
�)B
�5B
�5B
�5B
�;B
�;B
�)B
�B�B1'BF�BR�BVBZBaHBjBw�B|�B�B�PB<BD�B@�B<jB33B'�B�BVBB��B�yB�`B�`B�TB�#B��BǮB�XB�{By�BhsBXB=qB�B
��B
��B
��B
R�B
=qB
-B
�B
B	�B	�!B	��B	��B	��B	�B	hsB	VB	K�B	<jB	49B	2-B	.B	'�B	�B	  B�B�NB�)B�#B�B��B��B��B�jB�LB�3B�!B�!B�B�B��B�B�B�B�B�B�!B�!B�!B�B�9B�FB�qB�wB�qB�}B��B�qB�LB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�bB�hB�bB�hB��B��B��B�B�XB�}B�wB�}BB��B��B�}B�qB�jB�jB�dB�^B�^B�dB�^B�^B�dB�jB�XB�FB�LB�FB�LB��BȴBȴBɺB��B��B��B��B��B�
B�B�B�B�
B�#B�5B�BB�BB�NB�B�B�B�B��B��B	B	JB	bB	{B	�B	�B	#�B	-B	=qB	F�B	G�B	M�B	R�B	P�B	N�B	N�B	Q�B	W
B	[#B	[#B	ZB	T�B	S�B	W
B	VB	S�B	T�B	Q�B	Q�B	Q�B	N�B	M�B	K�B	M�B	P�B	Q�B	VB	\)B	_;B	`BB	bNB	cTB	dZB	_;B	`BB	bNB	cTB	e`B	hsB	jB	m�B	q�B	t�B	u�B	v�B	x�B	�%B	�1B	�7B	�7B	�1B	�%B	�B	�%B	�PB	�\B	�\B	�hB	��B	��B	��B	��B	��B	�uB	�hB	��B	��B	�PB	�=B	�PB	�VB	�\B	�\B	�\B	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�9B	�?B	�FB	�RB	�XB	�dB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	�}B	�}B	��B	ÖB	ÖB	ĜB	ĜB	ĜB	ƨB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�;B	�;B	�BB	�BB	�NB	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
%B
%B
%B
%B
%B
%B
%B
%B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
VB
VB
VB
VB
\B
\B
uB
bB
#:222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�9B
�jB
B
ȴB
ɺB
��B
��B
��B
�B
�B
�
B
�B
�#B
�)B
�)B
�5B
�5B
�5B
�;B
�;B
�)B
�B�B1'BF�BR�BVBZBaHBjBw�B|�B�B�PB<BD�B@�B<jB33B'�B�BVBB��B�yB�`B�`B�TB�#B��BǮB�XB�{By�BhsBXB=qB�B
��B
��B
��B
R�B
=qB
-B
�B
B	�B	�!B	��B	��B	��B	�B	hsB	VB	K�B	<jB	49B	2-B	.B	'�B	�B	  B�B�NB�)B�#B�B��B��B��B�jB�LB�3B�!B�!B�B�B��B�B�B�B�B�B�!B�!B�!B�B�9B�FB�qB�wB�qB�}B��B�qB�LB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�bB�hB�bB�hB��B��B��B�B�XB�}B�wB�}BB��B��B�}B�qB�jB�jB�dB�^B�^B�dB�^B�^B�dB�jB�XB�FB�LB�FB�LB��BȴBȴBɺB��B��B��B��B��B�
B�B�B�B�
B�#B�5B�BB�BB�NB�B�B�B�B��B��B	B	JB	bB	{B	�B	�B	#�B	-B	=qB	F�B	G�B	M�B	R�B	P�B	N�B	N�B	Q�B	W
B	[#B	[#B	ZB	T�B	S�B	W
B	VB	S�B	T�B	Q�B	Q�B	Q�B	N�B	M�B	K�B	M�B	P�B	Q�B	VB	\)B	_;B	`BB	bNB	cTB	dZB	_;B	`BB	bNB	cTB	e`B	hsB	jB	m�B	q�B	t�B	u�B	v�B	x�B	�%B	�1B	�7B	�7B	�1B	�%B	�B	�%B	�PB	�\B	�\B	�hB	��B	��B	��B	��B	��B	�uB	�hB	��B	��B	�PB	�=B	�PB	�VB	�\B	�\B	�\B	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�9B	�?B	�FB	�RB	�XB	�dB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	�}B	�}B	��B	ÖB	ÖB	ĜB	ĜB	ĜB	ƨB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�;B	�;B	�BB	�BB	�NB	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
%B
%B
%B
%B
%B
%B
%B
%B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
VB
VB
VB
VB
\B
\B
uB
bB
#:222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190553                              AO  ARCAADJP                                                                    20181005190553    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190553  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190553  QCF$                G�O�G�O�G�O�8000            