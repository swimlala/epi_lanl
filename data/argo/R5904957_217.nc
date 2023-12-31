CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:45Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140845  20181024140845  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @���4}1   @���33E�@5�Q��d�C��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C�C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCw�fCz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C��3C��3C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C��C��C�  C�  D fD �fDfD� DfD� D  Dy�D��D� D  D� D  D� D��Dy�D  D� D	  D	�fD
fD
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0y�D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DLfDL�fDM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dk��Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dty�Du  Du� Dv  Dv� Dv��Dwy�Dw��Dy�\D� �D�_\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ffB��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C�C33C33C33C
33C33C33C33C33C33C33C33CL�C33C33C L�C"33C$33C&33C(33C*33C,33C.33C033C233C433C6�C833C:33C<33C>33C@33CB33CD33CF�CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^L�C`33Cb33Cd33Cf33ChL�Cj33Cl33Cn33Cp33Cr33Ct33Cv�Cx�Cz33C|33C~33C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C�&gC�&gC��C��C��C��C��C��C��C��C��C�&gC�&gC��C��C��C��C��C��C�&gC�&gC�&gC��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C�&gC�&gC�&gC�&gC��C��D 3D �3D3D��D3D��D�D�gDgD��D�D��D�D��DgD�gD�D��D	�D	�3D
3D
�3D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D3D��D�D��D�D��DgD��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,3D,��D-�D-��D.�D.��D/�D/��D0�D0�gD1�D1�3D2�D2��D3�D3��D4�D4��D5�D5�3D6�D6��D7�D7��D8�D8��D9gD9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI3DI��DJ�DJ��DK�DK��DL3DL�3DM�DM��DN�DN��DO�DO�gDP�DP��DQ�DQ��DR�DR�3DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX3DX�3DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]�3D^�D^�gD_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk�gDlgDl��Dm�Dm��DngDn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��DtgDt�gDu�Du��Dv�Dv��DwgDw�gDw��Dy�)D�'
D�e�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��/A��;A��HA��HA��HA��/A��TA��mA��mA��`A��mA��`A��`A��`A��mA��mA��A��A��A���A���A���A���A���A���A���A���A��A��
A�5?A�l�Aƛ�Ař�AöFA�jA�\)A��hA�=qA�A��#A�"�A��+A��9A��A�A�A��A�A���A���A�n�A�A�A��hA�n�A�{A���A�  A�G�A��\A���A��A�/A��TA���A��^A�VA�?}A�/A���A�oA�?}A��A�S�A�dZA��9A�=qA��wA���A���A���A��A�  A��uA�
=A��A�$�A��A�K�A��mA�p�A��A�+A�JA��A�M�A���A�A���A��A�O�A�{A�A��+A� �A���A�t�A��A�bNA��`A��A�A~��A|��Ax�DAw��Av��Au��At�jAs?}Aq�mAo�#AkƨAi�Ag`BAedZAd�AchsA`��A^�DAZA�AXA�AW`BAT~�AQ��APJAO��ANjAM\)AL��AL��AL=qAJ�\AG\)ADr�AC�mAC��AAG�A=�A<v�A933A9;dA8M�A6��A61'A4�\A3%A2jA1�A0(�A-C�A+�A*��A(M�A&r�A$n�A"~�A!t�A ~�A bA&�A��A�A �AS�A��A��AE�A��A��A�A�A�
Al�A&�A��A�DAA7LAv�AE�A|�AI�AdZAƨAA
�RA
5?A	�A	�AbNA�;AƨA��AVA�HAr�AA�PA�A�\AI�A��A�+A�
AG�@��@���@��@��@�(�@���@�~�@��
@��y@�ff@홚@�V@�Z@�F@���@�K�@�@�?}@�b@�t�@�l�@�dZ@�\)@�;d@�v�@�@ڧ�@ٺ^@��@ա�@��@ѡ�@�A�@�K�@�"�@θR@Χ�@�-@�$�@�n�@�  @��@ɑh@ɉ7@�@ʟ�@��@�@�M�@�z�@�;d@�z�@Å@��@°!@�+@�|�@�+@���@�M�@���@��D@�  @��;@���@��!@���@��m@�;d@�@�K�@�|�@���@�-@��-@���@�hs@��`@��@�ȴ@��\@�G�@���@��j@��u@��u@�r�@��@��T@�b@���@�7L@�X@�%@��@�7L@�/@���@��`@���@��@�z�@��;@�o@��R@���@�V@�5?@�{@��@���@�p�@�/@���@�Ĝ@��@�1'@��F@�t�@���@�V@�E�@�-@��@��@�z�@�9X@�  @��w@���@�K�@��@��R@�5?@��7@��u@�Z@���@���@��@�dZ@�33@��y@���@��+@�ff@�5?@�J@�@��@��j@�Z@� �@��F@��@���@�+@��H@�ȴ@���@��+@��@�p�@�X@�&�@���@��j@��u@�I�@���@�l�@�S�@�K�@��@���@���@��@�G�@�G�@�&�@��`@��D@�bN@�9X@� �@��@�S�@��H@�M�@��@��T@���@��@��@���@�z�@� �@��@�|�@�l�@�l�@�l�@��@��@�;d@�o@��@�
=@��@���@��\@�@��@�/@�x�@�E�@���@��@��h@�`B@�?}@�`B@��h@�X@���@��w@�l�@�C�@�;d@�S�@�l�@�|�@�l�@�;d@��@��@��H@��R@��\@�J@���@�O�@�G�@�%@���@���@�1'@��;@���@��
@��@�dZ@�S�@�33@��!@��+@�=q@�V@�=q@�{@���@��#@�x�@�G�@��@�V@���@���@��D@�b@���@�t�@�S�@�;d@���@��@���@���@��+@�^5@��@���@r�1@e��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��/A��;A��HA��HA��HA��/A��TA��mA��mA��`A��mA��`A��`A��`A��mA��mA��A��A��A���A���A���A���A���A���A���A���A��A��
A�5?A�l�Aƛ�Ař�AöFA�jA�\)A��hA�=qA�A��#A�"�A��+A��9A��A�A�A��A�A���A���A�n�A�A�A��hA�n�A�{A���A�  A�G�A��\A���A��A�/A��TA���A��^A�VA�?}A�/A���A�oA�?}A��A�S�A�dZA��9A�=qA��wA���A���A���A��A�  A��uA�
=A��A�$�A��A�K�A��mA�p�A��A�+A�JA��A�M�A���A�A���A��A�O�A�{A�A��+A� �A���A�t�A��A�bNA��`A��A�A~��A|��Ax�DAw��Av��Au��At�jAs?}Aq�mAo�#AkƨAi�Ag`BAedZAd�AchsA`��A^�DAZA�AXA�AW`BAT~�AQ��APJAO��ANjAM\)AL��AL��AL=qAJ�\AG\)ADr�AC�mAC��AAG�A=�A<v�A933A9;dA8M�A6��A61'A4�\A3%A2jA1�A0(�A-C�A+�A*��A(M�A&r�A$n�A"~�A!t�A ~�A bA&�A��A�A �AS�A��A��AE�A��A��A�A�A�
Al�A&�A��A�DAA7LAv�AE�A|�AI�AdZAƨAA
�RA
5?A	�A	�AbNA�;AƨA��AVA�HAr�AA�PA�A�\AI�A��A�+A�
AG�@��@���@��@��@�(�@���@�~�@��
@��y@�ff@홚@�V@�Z@�F@���@�K�@�@�?}@�b@�t�@�l�@�dZ@�\)@�;d@�v�@�@ڧ�@ٺ^@��@ա�@��@ѡ�@�A�@�K�@�"�@θR@Χ�@�-@�$�@�n�@�  @��@ɑh@ɉ7@�@ʟ�@��@�@�M�@�z�@�;d@�z�@Å@��@°!@�+@�|�@�+@���@�M�@���@��D@�  @��;@���@��!@���@��m@�;d@�@�K�@�|�@���@�-@��-@���@�hs@��`@��@�ȴ@��\@�G�@���@��j@��u@��u@�r�@��@��T@�b@���@�7L@�X@�%@��@�7L@�/@���@��`@���@��@�z�@��;@�o@��R@���@�V@�5?@�{@��@���@�p�@�/@���@�Ĝ@��@�1'@��F@�t�@���@�V@�E�@�-@��@��@�z�@�9X@�  @��w@���@�K�@��@��R@�5?@��7@��u@�Z@���@���@��@�dZ@�33@��y@���@��+@�ff@�5?@�J@�@��@��j@�Z@� �@��F@��@���@�+@��H@�ȴ@���@��+@��@�p�@�X@�&�@���@��j@��u@�I�@���@�l�@�S�@�K�@��@���@���@��@�G�@�G�@�&�@��`@��D@�bN@�9X@� �@��@�S�@��H@�M�@��@��T@���@��@��@���@�z�@� �@��@�|�@�l�@�l�@�l�@��@��@�;d@�o@��@�
=@��@���@��\@�@��@�/@�x�@�E�@���@��@��h@�`B@�?}@�`B@��h@�X@���@��w@�l�@�C�@�;d@�S�@�l�@�|�@�l�@�;d@��@��@��H@��R@��\@�J@���@�O�@�G�@�%@���@���@�1'@��;@���@��
@��@�dZ@�S�@�33@��!@��+@�=q@�V@�=q@�{@���@��#@�x�@�G�@��@�V@���@���@��D@�b@���@�t�@�S�@�;d@���@��@���@���@��+@�^5@��@���@r�1@e��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�bB�bB�bB�bB�bB�hB�hB�hB��BB�B�B+B@�BP�BQ�BP�BR�BW
Bm�Bm�BbNBbNBcTBiyBhsBgmBdZB]/BYBT�BS�BR�BR�BVBe`B�B�uB�PB�PB�%B�JB�{B�PB�Bz�Br�BffB`BBXBG�BB�B:^B2-B-B$�B�BJB��B�B�;B��BǮB�^B�'B�B��B��B�VBy�BXB2-B�BVB1BB
��B
�B
�yB
�mB
�TB
�;B
�B
��B
�wB
�?B
�B
��B
��B
�1B
z�B
jB
F�B
D�B
?}B
2-B
'�B
�B
hB
B	�B	�BB	��B	ǮB	ÖB	�XB	��B	��B	w�B	cTB	^5B	N�B	9XB	'�B	�B	hB	DB	VB	{B	DB��B�ZB�#B�)B�)B�
B�dB�9B��B�B�RB�RB�RB�LB�-B�!B�B��B��B��B��B��B��B��B�{B�uB�hB�\B�VB�JB�DB�1B�%B�B� Bx�Bs�Bs�Bs�Br�Br�Br�Br�Bq�Br�Br�Br�Bq�Bp�Bp�Bn�Bk�BiyBiyBiyBgmBffBe`BgmBo�Bo�Bp�Bu�Bu�Bw�By�Bx�B{�B~�B� B�B�B~�B|�Bz�Bw�Bw�Bq�BhsBe`Be`BffBffBgmBhsBhsBiyBiyBm�Bt�B{�B~�B�1B�\B�\B�\B�\B�\B�hB�hB�uB��B��B��B�{B�PB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�3B�9B�3B�LB�dB�wBĜB��B��B��B��B��B�B�B�B�B�B�)B�5B�TB�fB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	B��B�B�B��B	�B	-B	49B	6FB	7LB	8RB	9XB	:^B	:^B	;dB	<jB	@�B	B�B	C�B	F�B	G�B	G�B	H�B	J�B	K�B	M�B	M�B	N�B	O�B	P�B	R�B	R�B	T�B	W
B	XB	XB	W
B	ZB	[#B	]/B	^5B	^5B	_;B	aHB	dZB	dZB	e`B	iyB	o�B	q�B	t�B	u�B	u�B	w�B	x�B	z�B	{�B	|�B	� B	�B	�+B	�=B	�JB	�JB	�\B	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�?B	�?B	�?B	�?B	�FB	�RB	�XB	�^B	�dB	�wB	��B	��B	��B	��B	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�;B	�5B	�HB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
  B
B
B
B
%B
+B
+B
	7B
	7B
DB
JB
PB
PB
PB
PB
PB
VB
VB
VB
PB
PB
PB
VB
\B
\B
bB
hB
oB
oB
oB
uB
{B
�B
2B
 vB
+�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�bB�bB�bB�bB�bB�hB�hB�hB��BB�B�B+B@�BP�BQ�BP�BR�BW
Bm�Bm�BbNBbNBcTBiyBhsBgmBdZB]/BYBT�BS�BR�BR�BVBe`B�B�uB�PB�PB�%B�JB�{B�PB�Bz�Br�BffB`BBXBG�BB�B:^B2-B-B$�B�BJB��B�B�;B��BǮB�^B�'B�B��B��B�VBy�BXB2-B�BVB1BB
��B
�B
�yB
�mB
�TB
�;B
�B
��B
�wB
�?B
�B
��B
��B
�1B
z�B
jB
F�B
D�B
?}B
2-B
'�B
�B
hB
B	�B	�BB	��B	ǮB	ÖB	�XB	��B	��B	w�B	cTB	^5B	N�B	9XB	'�B	�B	hB	DB	VB	{B	DB��B�ZB�#B�)B�)B�
B�dB�9B��B�B�RB�RB�RB�LB�-B�!B�B��B��B��B��B��B��B��B�{B�uB�hB�\B�VB�JB�DB�1B�%B�B� Bx�Bs�Bs�Bs�Br�Br�Br�Br�Bq�Br�Br�Br�Bq�Bp�Bp�Bn�Bk�BiyBiyBiyBgmBffBe`BgmBo�Bo�Bp�Bu�Bu�Bw�By�Bx�B{�B~�B� B�B�B~�B|�Bz�Bw�Bw�Bq�BhsBe`Be`BffBffBgmBhsBhsBiyBiyBm�Bt�B{�B~�B�1B�\B�\B�\B�\B�\B�hB�hB�uB��B��B��B�{B�PB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�3B�9B�3B�LB�dB�wBĜB��B��B��B��B��B�B�B�B�B�B�)B�5B�TB�fB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	B��B�B�B��B	�B	-B	49B	6FB	7LB	8RB	9XB	:^B	:^B	;dB	<jB	@�B	B�B	C�B	F�B	G�B	G�B	H�B	J�B	K�B	M�B	M�B	N�B	O�B	P�B	R�B	R�B	T�B	W
B	XB	XB	W
B	ZB	[#B	]/B	^5B	^5B	_;B	aHB	dZB	dZB	e`B	iyB	o�B	q�B	t�B	u�B	u�B	w�B	x�B	z�B	{�B	|�B	� B	�B	�+B	�=B	�JB	�JB	�\B	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�?B	�?B	�?B	�?B	�FB	�RB	�XB	�^B	�dB	�wB	��B	��B	��B	��B	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�;B	�5B	�HB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
  B
B
B
B
%B
+B
+B
	7B
	7B
DB
JB
PB
PB
PB
PB
PB
VB
VB
VB
PB
PB
PB
VB
\B
\B
bB
hB
oB
oB
oB
uB
{B
�B
2B
 vB
+�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140845                              AO  ARCAADJP                                                                    20181024140845    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140845  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140845  QCF$                G�O�G�O�G�O�0               