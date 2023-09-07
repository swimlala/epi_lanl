CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:19Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191719  20181005191719  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$�o��1   @��%b���@5��$��duhr� �1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A^ffA~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBHffBP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B���C  C  C�C�C
�C�C  C  C  C  C�fC�fC�fC  C  C �C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCC�fCE�fCG�fCI�fCL  CN  CP  CR�CT�CV  CX  CZ  C\�C^�C`  Cb  Cd  Cf  Ch�Cj�Cl33Cn�Cp�Cr�Ct�Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C��C��C�  C��3C��3C��fC��3C��C�  C�  C��3C��3C��3C�  C��C��C��3C�  C�  C�  C�  C�  C�  C��3C��C��C�  C��C�  C�  C�  C��C�  C��C�  C��3C��3C�  C��3C�  C�  C�  C�  C��3C��3C�  C��C��3C��C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3D fD �fDfD� DfD� D��Dy�D��D� DfD�fD  D� D��Dy�D��D� D	  D	y�D
  D
� D
��D� D  D� D  D� D  D�fD  D� D  D� D  Dy�D  Dy�D��Dy�D��D� DfD�fDfD�fD  Dy�D  D� D��D� D��Dy�D  D� D  D��DfD� D  D�fD  Dy�D   D �fD!  D!s3D"  D"�fD#  D#y�D#��D$y�D$��D%y�D&  D&� D&��D'y�D(  D(y�D)  D)� D*  D*y�D*��D+y�D,  D,� D-  D-��D.fD.y�D/fD/� D/��D0y�D1  D1y�D2  D2� D2��D3y�D3��D4� D5  D5�fD6fD6�fD7  D7� D7��D8� D9fD9�fD:  D:� D;  D;�fD<fD<�fD=fD=�fD>fD>� D?  D?� D@  D@� D@��DAy�DB  DB�fDC  DCy�DD  DD� DE  DE�fDF  DF� DGfDG� DH  DH�fDI  DI� DJ  DJ�fDK  DK� DK��DL� DMfDM��DNfDN� DOfDO� DP  DP� DQ  DQy�DQ�3DRy�DSfDS�fDTfDT� DT��DU� DV  DVy�DW  DW� DXfDX�fDY  DYy�DZ  DZ� D[  D[� D\fD\� D]  D]y�D^  D^� D^��D_� D`  D`y�Da  Day�Db  Db� Dc  Dc�fDd  Dd� DefDe� Df  Df�fDg  Dgy�Dg��Dh� DifDi�fDj  Dj� Dj��Dk� DlfDl� Dm  Dm� Dn  Dn�fDo  Do� DpfDp� Dp��Dq� Dq��Drs3Dr��Dsy�Ds��Dt� Du  Duy�Du��Dv�fDwfDw� DwٚDy��D�/�D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�ffA33A#33AC33Aa��A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��BA33BI33BP��BX��B`��Bh��Bp��BxfgB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB왙B�ffB�ffB���B�ffC �C33C33CL�CL�C
L�CL�C33C33C33C33C�C�C�C33C33C L�C"L�C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB�CD�CF�CH�CJ�CL33CN33CP33CRL�CTL�CV33CX33CZ33C\L�C^L�C`33Cb33Cd33Cf33ChL�CjL�ClffCnL�CpL�CrL�CtL�Cv33Cx33Cz33C|33C~33C�&gC��C��C��C�&gC�&gC��C��C��C�  C��C�&gC��C��C��C��C��C��C�&gC�&gC��C��C��C��C��C��C��C��C�&gC�34C��C�&gC��C��C��C�&gC��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C�&gC�&gC�34C��C�&gC��C��C��C��C��C��C��C��C��C�&gC�&gC�&gC��C��C��C��C��C�34C�&gC��C��C��C��C�&gC��C��C�&gC�&gC��C��C��C��C��C��C��C��C��C��C��C�&gC�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C�&gC��C��C��C��C��C��D 3D �3D3D��D3D��DgD�gDgD��D3D�3D�D��DgD�gDgD��D	�D	�gD
�D
��DgD��D�D��D�D��D�D�3D�D��D�D��D�D�gD�D�gDgD�gDgD��D3D�3D3D�3D�D�gD�D��DgD��DgD�gD�D��D�D��D3D��D�D�3D�D�gD �D �3D!�D!� D"�D"�3D#�D#�gD$gD$�gD%gD%�gD&�D&��D'gD'�gD(�D(�gD)�D)��D*�D*�gD+gD+�gD,�D,��D-�D-��D.3D.�gD/3D/��D0gD0�gD1�D1�gD2�D2��D3gD3�gD4gD4��D5�D5�3D63D6�3D7�D7��D8gD8��D93D9�3D:�D:��D;�D;�3D<3D<�3D=3D=�3D>3D>��D?�D?��D@�D@��DAgDA�gDB�DB�3DC�DC�gDD�DD��DE�DE�3DF�DF��DG3DG��DH�DH�3DI�DI��DJ�DJ�3DK�DK��DLgDL��DM3DM��DN3DN��DO3DO��DP�DP��DQ�DQ�gDR  DR�gDS3DS�3DT3DT��DUgDU��DV�DV�gDW�DW��DX3DX�3DY�DY�gDZ�DZ��D[�D[��D\3D\��D]�D]�gD^�D^��D_gD_��D`�D`�gDa�Da�gDb�Db��Dc�Dc�3Dd�Dd��De3De��Df�Df�3Dg�Dg�gDhgDh��Di3Di�3Dj�Dj��DkgDk��Dl3Dl��Dm�Dm��Dn�Dn�3Do�Do��Dp3Dp��DqgDq��DrgDr� DsgDs�gDtgDt��Du�Du�gDvgDv�3Dw3Dw��Dw�gDy��D�6D��p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��`A��`A��`A��mA��yA��`A��mA��`A��mA��mA��yA��A��A��A��A��A��A��A��A��A��mA��A��Aأ�AׅA֏\AՉ7A�JA�jA�Q�A��A�l�A�v�AĲ-A�9XA�n�A��A���A��A�G�A�VA�%A��mA��A�I�A��hA�9XA��mA��\A�ȴA��uA�/A�?}A���A���A�ZA��DA�1A�&�A���A�|�A�jA�%A���A���A��\A�O�A��;A���A��\A��
A�hsA��/A��TA�O�A��uA�|�A��!A���A�ffA��-A�l�A���A�jA�E�A�ĜA�\)A�v�A�{A��FA��A��A~�A}ƨA|�HA{"�As�^Ap�An��An�DAn^5An(�Amx�AjQ�Afr�Ae�Ad�AcK�Ab�/Abz�Aa�TA^�A[�PAY��AYoAW�-AVbAUS�AT��AS�
AQ��APVAN-AK+AH�!AD�yAD{AC�A@VA=O�A<��A;x�A9�mA6��A2�`A0{A/A-7LA+?}A*n�A)&�A'|�A&�A%��A$�DA#��A"��A!�TA�A��A�A�+AE�A�A�/AQ�AS�A&�AȴA�
AQ�A/A�At�A�HA��A^5A��A��A�yAr�AC�A
�A�`A�mA�AO�A�jAx�A�A��A�RAv�A�A�A7L@��P@���@�7L@��9@��@��\@�%@��y@���@�1@�ƨ@�~�@�@@���@��T@�j@���@�K�@�p�@�t�@��H@�+@�$�@��@�%@�Q�@��@�
=@��#@���@�j@ޏ\@�`B@ۅ@�=q@؋D@�@��#@ԓu@Ӯ@�"�@ѡ�@��
@�;d@·+@�J@͙�@�&�@�`B@��m@�E�@���@�`B@��@Ĵ9@�b@�t�@���@�1@�A�@�Z@�I�@�1@���@���@���@�Ĝ@���@�"�@�$�@��T@�X@��@��^@��7@�p�@�/@���@�-@���@��!@�Ĝ@���@�33@�o@��H@�^5@���@�G�@�/@���@��@�  @�+@��R@���@�^5@��@�J@�@�/@�r�@��@��;@��
@��
@���@��;@��@�9X@��P@��P@���@�dZ@�\)@���@���@��P@���@��
@���@�"�@���@���@�n�@�=q@�-@��@�-@���@��/@��@�;d@���@��+@��+@���@���@�ȴ@��y@��@��H@��@���@��@��@��-@���@��7@�p�@�p�@�X@��u@�5?@��+@�M�@��!@��!@���@��H@��@���@��\@�ff@��@��#@�?}@���@��@��D@��u@��@��;@�S�@���@�E�@���@��-@�?}@��9@�r�@�  @��w@�|�@�dZ@�\)@�C�@�;d@�33@��@���@��@�n�@��h@���@���@�Ĝ@��9@��`@�%@��@� �@���@�hs@���@���@��-@���@��h@��@�x�@�hs@�G�@��h@���@���@�O�@�?}@�7L@���@�&�@�X@�x�@���@���@��@��D@�(�@�1'@�Q�@��@�S�@�o@�@��@��R@�~�@��@��7@��-@���@���@�x�@�Ĝ@�=q@�M�@�/@���@�9X@�(�@�9X@�r�@�I�@�(�@�Q�@��u@��@��P@��
@�/@���@���@���@���@�ȴ@���@�v�@�M�@��@���@�?}@��9@��@�Q�@� �@��m@�t�@�C�@�K�@�+@���@��^@��@�j@�@��@�P@|�@K�@+@~ȴ@~�+@~E�@}��@}p�@}�@|��@|1@{�
@{�F@{��@{�@{��@{�@z��@z=q@y��@yF@nd�@^!�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��TA��`A��`A��`A��mA��yA��`A��mA��`A��mA��mA��yA��A��A��A��A��A��A��A��A��A��mA��A��Aأ�AׅA֏\AՉ7A�JA�jA�Q�A��A�l�A�v�AĲ-A�9XA�n�A��A���A��A�G�A�VA�%A��mA��A�I�A��hA�9XA��mA��\A�ȴA��uA�/A�?}A���A���A�ZA��DA�1A�&�A���A�|�A�jA�%A���A���A��\A�O�A��;A���A��\A��
A�hsA��/A��TA�O�A��uA�|�A��!A���A�ffA��-A�l�A���A�jA�E�A�ĜA�\)A�v�A�{A��FA��A��A~�A}ƨA|�HA{"�As�^Ap�An��An�DAn^5An(�Amx�AjQ�Afr�Ae�Ad�AcK�Ab�/Abz�Aa�TA^�A[�PAY��AYoAW�-AVbAUS�AT��AS�
AQ��APVAN-AK+AH�!AD�yAD{AC�A@VA=O�A<��A;x�A9�mA6��A2�`A0{A/A-7LA+?}A*n�A)&�A'|�A&�A%��A$�DA#��A"��A!�TA�A��A�A�+AE�A�A�/AQ�AS�A&�AȴA�
AQ�A/A�At�A�HA��A^5A��A��A�yAr�AC�A
�A�`A�mA�AO�A�jAx�A�A��A�RAv�A�A�A7L@��P@���@�7L@��9@��@��\@�%@��y@���@�1@�ƨ@�~�@�@@���@��T@�j@���@�K�@�p�@�t�@��H@�+@�$�@��@�%@�Q�@��@�
=@��#@���@�j@ޏ\@�`B@ۅ@�=q@؋D@�@��#@ԓu@Ӯ@�"�@ѡ�@��
@�;d@·+@�J@͙�@�&�@�`B@��m@�E�@���@�`B@��@Ĵ9@�b@�t�@���@�1@�A�@�Z@�I�@�1@���@���@���@�Ĝ@���@�"�@�$�@��T@�X@��@��^@��7@�p�@�/@���@�-@���@��!@�Ĝ@���@�33@�o@��H@�^5@���@�G�@�/@���@��@�  @�+@��R@���@�^5@��@�J@�@�/@�r�@��@��;@��
@��
@���@��;@��@�9X@��P@��P@���@�dZ@�\)@���@���@��P@���@��
@���@�"�@���@���@�n�@�=q@�-@��@�-@���@��/@��@�;d@���@��+@��+@���@���@�ȴ@��y@��@��H@��@���@��@��@��-@���@��7@�p�@�p�@�X@��u@�5?@��+@�M�@��!@��!@���@��H@��@���@��\@�ff@��@��#@�?}@���@��@��D@��u@��@��;@�S�@���@�E�@���@��-@�?}@��9@�r�@�  @��w@�|�@�dZ@�\)@�C�@�;d@�33@��@���@��@�n�@��h@���@���@�Ĝ@��9@��`@�%@��@� �@���@�hs@���@���@��-@���@��h@��@�x�@�hs@�G�@��h@���@���@�O�@�?}@�7L@���@�&�@�X@�x�@���@���@��@��D@�(�@�1'@�Q�@��@�S�@�o@�@��@��R@�~�@��@��7@��-@���@���@�x�@�Ĝ@�=q@�M�@�/@���@�9X@�(�@�9X@�r�@�I�@�(�@�Q�@��u@��@��P@��
@�/@���@���@���@���@�ȴ@���@�v�@�M�@��@���@�?}@��9@��@�Q�@� �@��m@�t�@�C�@�K�@�+@���@��^@��@�j@�@��@�P@|�@K�@+@~ȴ@~�+@~E�@}��@}p�@}�@|��@|1@{�
@{�F@{��@{�@{��@{�@z��@z=q@y��@yF@nd�@^!�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=BDBDBDBDBDBDB
=BDBDB#�B49BB�BI�BP�BP�BF�BH�BP�BT�BiyBq�BjBo�Bu�B{�B~�B�%B�DB�JB�DB�VB�\B��B��B��B�bB�PB�B~�Bn�BhsBdZB\)BD�B,B!�BbB��B�B�)BĜB�}B�qB�XB�-B��B�{B�JB�B|�Bp�BgmB]/BZBN�B?}B�B
��B
�ZB
�B
ŢB
�9B
��B
�7B
y�B
^5B
K�B
F�B
@�B
8RB
/B
%�B
�B	�B	�B	��B	��B	ȴB	ŢB	�}B	�B	��B	�JB	�1B	�B	}�B	z�B	t�B	cTB	S�B	I�B	D�B	=qB	49B	/B	+B	%�B	�B	�B	JB	B�B�B�ZB�BB�B��BǮBĜB�jB�?B�B��B��B��B��B��B��B��B��B��B��B�uB�hB�VB�7B�%B�B�B�B� By�B|�B� B�B� B|�B{�Bw�Bp�Bl�BhsBffBe`BdZBbNBbNB_;BYBR�BN�BJ�BI�BH�BF�BC�BB�BB�BA�B@�B>wB;dB7LB6FB5?B5?B49B49B33B33B33B2-B1'B0!B/B/B.B-B-B,B,B+B+B,B,B,B,B,B,B,B,B+B+B+B)�B,B,B-B+B,B-B.B.B.B/B33B49B5?B6FB7LB8RB9XB?}BF�B?}BE�BK�BL�BM�BN�BO�BT�B_;Bk�Bn�Bo�Bq�Br�Bv�Bx�B{�Bw�Bp�Bn�Bn�Bm�Bq�Bs�Bt�Bv�B|�B|�B}�B}�B}�B�B�%B�1B�1B�7B�DB�\B�hB�oB�{B��B��B��B��B��B��B��B��B��B�B�9B�FB�XB�dB�jB��BȴB��B�B�B�TB�B�B�B��B��B��B	  B	1B	PB	bB	�B	�B	#�B	&�B	&�B	&�B	'�B	(�B	-B	2-B	33B	33B	49B	6FB	7LB	8RB	:^B	;dB	?}B	A�B	A�B	B�B	J�B	K�B	M�B	M�B	P�B	S�B	VB	ZB	[#B	YB	^5B	^5B	dZB	iyB	m�B	s�B	t�B	t�B	u�B	w�B	x�B	z�B	z�B	}�B	�B	�B	�+B	�7B	�7B	�1B	�%B	�B	�%B	�1B	�1B	�1B	�7B	�=B	�=B	�=B	�DB	�DB	�JB	�DB	�DB	�DB	�JB	�PB	�PB	�JB	�JB	�bB	�hB	�uB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�9B	�?B	�RB	�dB	�qB	��B	B	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	ȴB	ĜB	ÖB	B	ÖB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�#B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
#�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=BDBDBDBDBDBDB
=BDBDB#�B49BB�BI�BP�BP�BF�BH�BP�BT�BiyBq�BjBo�Bu�B{�B~�B�%B�DB�JB�DB�VB�\B��B��B��B�bB�PB�B~�Bn�BhsBdZB\)BD�B,B!�BbB��B�B�)BĜB�}B�qB�XB�-B��B�{B�JB�B|�Bp�BgmB]/BZBN�B?}B�B
��B
�ZB
�B
ŢB
�9B
��B
�7B
y�B
^5B
K�B
F�B
@�B
8RB
/B
%�B
�B	�B	�B	��B	��B	ȴB	ŢB	�}B	�B	��B	�JB	�1B	�B	}�B	z�B	t�B	cTB	S�B	I�B	D�B	=qB	49B	/B	+B	%�B	�B	�B	JB	B�B�B�ZB�BB�B��BǮBĜB�jB�?B�B��B��B��B��B��B��B��B��B��B��B�uB�hB�VB�7B�%B�B�B�B� By�B|�B� B�B� B|�B{�Bw�Bp�Bl�BhsBffBe`BdZBbNBbNB_;BYBR�BN�BJ�BI�BH�BF�BC�BB�BB�BA�B@�B>wB;dB7LB6FB5?B5?B49B49B33B33B33B2-B1'B0!B/B/B.B-B-B,B,B+B+B,B,B,B,B,B,B,B,B+B+B+B)�B,B,B-B+B,B-B.B.B.B/B33B49B5?B6FB7LB8RB9XB?}BF�B?}BE�BK�BL�BM�BN�BO�BT�B_;Bk�Bn�Bo�Bq�Br�Bv�Bx�B{�Bw�Bp�Bn�Bn�Bm�Bq�Bs�Bt�Bv�B|�B|�B}�B}�B}�B�B�%B�1B�1B�7B�DB�\B�hB�oB�{B��B��B��B��B��B��B��B��B��B�B�9B�FB�XB�dB�jB��BȴB��B�B�B�TB�B�B�B��B��B��B	  B	1B	PB	bB	�B	�B	#�B	&�B	&�B	&�B	'�B	(�B	-B	2-B	33B	33B	49B	6FB	7LB	8RB	:^B	;dB	?}B	A�B	A�B	B�B	J�B	K�B	M�B	M�B	P�B	S�B	VB	ZB	[#B	YB	^5B	^5B	dZB	iyB	m�B	s�B	t�B	t�B	u�B	w�B	x�B	z�B	z�B	}�B	�B	�B	�+B	�7B	�7B	�1B	�%B	�B	�%B	�1B	�1B	�1B	�7B	�=B	�=B	�=B	�DB	�DB	�JB	�DB	�DB	�DB	�JB	�PB	�PB	�JB	�JB	�bB	�hB	�uB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�9B	�?B	�RB	�dB	�qB	��B	B	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	ȴB	ĜB	ÖB	B	ÖB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�#B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
#�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191719                              AO  ARCAADJP                                                                    20181005191719    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191719  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191719  QCF$                G�O�G�O�G�O�8000            