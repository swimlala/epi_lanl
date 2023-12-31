CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:06Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190506  20181005190506  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @ס*��q�1   @ס+��,@3`A�7K��c��O�;d1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @9��@y��@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�33A�33B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B_��Bh  BpffBx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C33C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C[�fC^  C`�Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct�Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��3C�  C��3C��3C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C��3C��3C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C��3C��3C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��3D � D  Dy�D��Dy�D��Dy�D��D� DfD� D  D�fD  D� D  D� D��D	� D
  D
� D  D� D  Dy�D��D� DfD� D  Dy�D  D�fDfD� D  D� D  D� DfD�fD  Dy�D  D� D��D� D��Dy�D  D� D��Dy�D��Dy�D  D� D  Dy�D��Dy�D  D�fD fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&y�D'  D'� D(fD(� D)fD)�fD*fD*�fD+fD+�fD,fD,� D-  D-�fD.fD.� D/  D/�fD0  D0� D1fD1� D1��D2� D3  D3� D3��D4y�D5  D5� D5��D6� D7fD7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=�fD>  D>�fD?fD?� D@  D@� DAfDA� DA��DBy�DB��DC� DD  DD� DE  DE� DF  DFy�DG  DG� DG��DHy�DH��DI� DJ  DJ�fDK  DK� DL  DLy�DL��DMy�DN  DN� DO  DO� DO��DPy�DP��DQy�DR  DRy�DR��DS� DT  DT� DUfDU�fDV  DV� DV��DWy�DX  DX�fDY  DYy�DY��DZy�D[  D[�fD\  D\y�D\��D]� D^fD^� D^��D_�fD`fD`� Da  Da�fDa��Dby�Dc  Dc� Dc��Dd�fDefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Di��Dj� Dk  Dk�fDk��Dl� Dm  Dm� Dn  Dn� Do  Doy�DpfDpy�Dq  Dqy�DrfDr� Ds  Ds� Dt  Dt� Du  Du� DvfDvy�DwfDw� DwٚDy� D�H�D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Ffg@�33@�ffA33A!��AC33Ac33A���A���A���A���A���Aљ�AᙚA���B ffB��B��B��B ��B(��B0��B8��B@fgBH��BP��BX��B`fgBh��Bq33Bx��B���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C�C33C33C33C
33C33C33C33C33C33CffC�C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>L�C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV�CX33CZ33C\�C^33C`L�Cb33Cd33Cf�Ch33Cj33Cl33Cn33Cp33Cr33CtL�CvL�Cx33Cz33C|33C~33C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC�&gC�&gC��C��C��C��C��C��C��C��C�&gC��C��C�&gC��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C�&gC�&gC��C��C��C��C��C��D fD ��D�D�gDgD�gDgD�gDgD��D3D��D�D�3D�D��D�D��D	gD	��D
�D
��D�D��D�D�gDgD��D3D��D�D�gD�D�3D3D��D�D��D�D��D3D�3D�D�gD�D��DgD��DgD�gD�D��DgD�gDgD�gD�D��D�D�gDgD�gD�D�3D 3D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&�gD'�D'��D(3D(��D)3D)�3D*3D*�3D+3D+�3D,3D,��D-�D-�3D.3D.��D/�D/�3D0�D0��D13D1��D2gD2��D3�D3��D4gD4�gD5�D5��D6gD6��D73D7��D8�D8��D9�D9�3D:�D:��D;�D;��D<�D<��D=�D=�3D>�D>�3D?3D?��D@�D@��DA3DA��DBgDB�gDCgDC��DD�DD��DE�DE��DF�DF�gDG�DG��DHgDH�gDIgDI��DJ�DJ�3DK�DK��DL�DL�gDMgDM�gDN�DN��DO�DO��DPgDP�gDQgDQ�gDR�DR�gDSgDS��DT�DT��DU3DU�3DV�DV��DWgDW�gDX�DX�3DY�DY�gDZgDZ�gD[�D[�3D\�D\�gD]gD]��D^3D^��D_gD_�3D`3D`��Da�Da�3DbgDb�gDc�Dc��DdgDd�3De3De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��DjgDj��Dk�Dk�3DlgDl��Dm�Dm��Dn�Dn��Do�Do�gDp3Dp�gDq�Dq�gDr3Dr��Ds�Ds��Dt�Dt��Du�Du��Dv3Dv�gDw3Dw��Dw�gDy��D�O\D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7LA�5?A�5?A�33A�7LA�5?A�33A��
A̺^A�hsA�bA��A���A�A˾wAˬAˍPA�r�A�33A�1A���A��A��A��A��yA��TA��A��
A��
A��
A���A���A���A���AʼjAʙ�AʋDA�v�A�M�A�&�A��A��A��A��
Aɛ�AɓuA��A���AɮA�l�AȾwA�z�A��A��
A�ffAƸRA�ffA���Aŗ�A�`BA�7LA��A���A�;dA��A�A��hA�(�A�t�A�x�A��A��
A��A���A��A�1A���A��yA��9A�33A��uA�"�A�9XA��/A�r�A�~�A�VA��A��yA�(�A�z�A��yA�JA�5?A�bA��A�l�A�-A�VA�5?A��`A�+A��A�dZA���A�{A�
=A�{A�ĜA��`A�r�A��;A�bNAG�Ay`BAu�hAs|�Ap{AnAl��Aj��AiVAg
=Af��Ac33A_t�AYl�AW�PASVAP^5AN �AMO�AL�AK�
AK?}AI�AH�`AG�AF�AE��AEAD��AD �ABA�A>VA=�A<Q�A;��A:�A8�`A7��A6��A5��A4^5A3��A1�
A1�A0�A0~�A/�
A.ȴA-dZA,v�A,JA+�PA*��A*~�A)XA(�A(1A$ĜA"{A!K�A M�A��A~�AG�A  A��Ax�A�A�A%A�A
=A�HA�PAt�A�AI�A-A�-A��A
�A1'A�FAG�A�TA��A~�AE�AAG�AĜAr�AVA9XA�A�PA 1@�V@���@���@�%@�I�@�1@��
@�
=@��/@�P@�+@�+@�X@�Ĝ@�(�@�ƨ@��-@�1@�C�@�J@�\)@�x�@�D@�z�@�r�@㕁@��@�^@ߕ�@��H@�X@�9X@�ƨ@ڗ�@�`B@��@�b@�M�@�x�@���@��;@�+@�@��/@�C�@�@͡�@���@�hs@���@�@ə�@�G�@���@ȋD@� �@Ǖ�@�@��@�z�@ÍP@���@��w@��@�p�@�&�@���@�9X@���@�@�v�@���@�v�@���@���@��/@��@�Z@�1'@��F@��@���@���@���@��R@�ȴ@���@���@�5?@���@�x�@�7L@�V@���@�Ĝ@��9@���@��u@��@��;@�S�@��y@���@���@�p�@�-@�K�@�n�@��R@�o@�V@���@��
@��@��;@�1'@�9X@�(�@�b@��m@�t�@�C�@�S�@�C�@�"�@�@��y@��@��!@���@���@�v�@�5?@�{@��@���@�x�@�/@��@��@��@�V@�%@��j@�Z@��;@��@���@���@��\@���@�V@���@�z�@�b@�b@��w@�\)@�33@�33@��H@�M�@��@��@�@�x�@�G�@�/@���@��@���@���@��@��@��@���@���@��@�r�@�Z@�Q�@�A�@�(�@�1@���@���@��@��@���@��F@�|�@�K�@��@�@��@���@�n�@�M�@�=q@�$�@�{@��@��-@�p�@�7L@���@���@��@�  @��F@��@��@���@��@�K�@�o@�ȴ@�v�@�$�@���@���@���@�hs@�7L@�&�@��@�Ĝ@���@�r�@�1@�|�@�"�@���@�n�@�V@��@��7@��/@�Z@�  @���@���@�dZ@��H@���@�~�@�ff@�V@�E�@���@�7L@�V@���@�A�@�dZ@���@�ȴ@�ȴ@���@�n�@��\@�^5@���@�hs@�?}@��@���@� �@��;@���@���@���@�\)@�K�@�o@�
=@�o@��@�V@�{@��@�x�@��@�%@��`@��9@�1�@���@nں1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�7LA�5?A�5?A�33A�7LA�5?A�33A��
A̺^A�hsA�bA��A���A�A˾wAˬAˍPA�r�A�33A�1A���A��A��A��A��yA��TA��A��
A��
A��
A���A���A���A���AʼjAʙ�AʋDA�v�A�M�A�&�A��A��A��A��
Aɛ�AɓuA��A���AɮA�l�AȾwA�z�A��A��
A�ffAƸRA�ffA���Aŗ�A�`BA�7LA��A���A�;dA��A�A��hA�(�A�t�A�x�A��A��
A��A���A��A�1A���A��yA��9A�33A��uA�"�A�9XA��/A�r�A�~�A�VA��A��yA�(�A�z�A��yA�JA�5?A�bA��A�l�A�-A�VA�5?A��`A�+A��A�dZA���A�{A�
=A�{A�ĜA��`A�r�A��;A�bNAG�Ay`BAu�hAs|�Ap{AnAl��Aj��AiVAg
=Af��Ac33A_t�AYl�AW�PASVAP^5AN �AMO�AL�AK�
AK?}AI�AH�`AG�AF�AE��AEAD��AD �ABA�A>VA=�A<Q�A;��A:�A8�`A7��A6��A5��A4^5A3��A1�
A1�A0�A0~�A/�
A.ȴA-dZA,v�A,JA+�PA*��A*~�A)XA(�A(1A$ĜA"{A!K�A M�A��A~�AG�A  A��Ax�A�A�A%A�A
=A�HA�PAt�A�AI�A-A�-A��A
�A1'A�FAG�A�TA��A~�AE�AAG�AĜAr�AVA9XA�A�PA 1@�V@���@���@�%@�I�@�1@��
@�
=@��/@�P@�+@�+@�X@�Ĝ@�(�@�ƨ@��-@�1@�C�@�J@�\)@�x�@�D@�z�@�r�@㕁@��@�^@ߕ�@��H@�X@�9X@�ƨ@ڗ�@�`B@��@�b@�M�@�x�@���@��;@�+@�@��/@�C�@�@͡�@���@�hs@���@�@ə�@�G�@���@ȋD@� �@Ǖ�@�@��@�z�@ÍP@���@��w@��@�p�@�&�@���@�9X@���@�@�v�@���@�v�@���@���@��/@��@�Z@�1'@��F@��@���@���@���@��R@�ȴ@���@���@�5?@���@�x�@�7L@�V@���@�Ĝ@��9@���@��u@��@��;@�S�@��y@���@���@�p�@�-@�K�@�n�@��R@�o@�V@���@��
@��@��;@�1'@�9X@�(�@�b@��m@�t�@�C�@�S�@�C�@�"�@�@��y@��@��!@���@���@�v�@�5?@�{@��@���@�x�@�/@��@��@��@�V@�%@��j@�Z@��;@��@���@���@��\@���@�V@���@�z�@�b@�b@��w@�\)@�33@�33@��H@�M�@��@��@�@�x�@�G�@�/@���@��@���@���@��@��@��@���@���@��@�r�@�Z@�Q�@�A�@�(�@�1@���@���@��@��@���@��F@�|�@�K�@��@�@��@���@�n�@�M�@�=q@�$�@�{@��@��-@�p�@�7L@���@���@��@�  @��F@��@��@���@��@�K�@�o@�ȴ@�v�@�$�@���@���@���@�hs@�7L@�&�@��@�Ĝ@���@�r�@�1@�|�@�"�@���@�n�@�V@��@��7@��/@�Z@�  @���@���@�dZ@��H@���@�~�@�ff@�V@�E�@���@�7L@�V@���@�A�@�dZ@���@�ȴ@�ȴ@���@�n�@��\@�^5@���@�hs@�?}@��@���@� �@��;@���@���@���@�\)@�K�@�o@�
=@�o@��@�V@�{@��@�x�@��@�%@��`@��9@�1�@���@nں1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�\B
�\B
�\B
�\B
�\B
�VB
�VB
�DB
�VB
��B
�B
��B
�B
�B
�B
�!B
�!B
�'B
�9B
�9B
�9B
�3B
�9B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�LB
�LB
�LB
�XB
�^B
�dB
�dB
�RB
�3B
�B
�B
�-B
�LB
�LB
�dB
��B
��B1B�B-BI�BjB�oB�B�XB��BŢB��B�)B�5B�B��B��B��B��BBB%B
=BVB{B+B�B�`B�BB�;B�B�)B�B�B�B�BuB%�B�B�B�B'�B(�B'�B(�B �BB�B��B�B��B��BdZBM�BF�B8RB�B�B �B
��B
�fB
��B
��B
�!B
t�B
W
B
K�B
.B	��B	�5B	��B	�3B	��B	��B	�JB	�B	v�B	p�B	^5B	I�B	.B	"�B	oB	1B	B��B��B��B��B�B�B�B�mB�`B�TB�HB�;B�B��B��B��B��BȴBƨBÖB��B�qB�dB�RB�dB�wB�wB�}BBƨB��B�B�/B�mB�B��B��B��B��B��B�B�B��B��B�fB��B��B��B��B��B��BŢB�B��B��B�-B��B�B�B��B��BB�!B��B��B��B�uB�PB�DB�=B�7B�7B�+B�+B�%B�%B�B�%B�B�JB�VB�PB�uB�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�B�XB�qB�}BBÖBÖBĜBĜBĜBƨB��B��B��B��B��B��B��B��B�
B��B��B��B�B�`B�fB�mB�fB�fB�sB�B�B�B�B�sB�fB�B�B��B��B��B��B��B��B	B	\B	�B	�B	 �B	 �B	 �B	�B	 �B	$�B	$�B	$�B	$�B	$�B	(�B	-B	1'B	49B	7LB	7LB	9XB	;dB	=qB	>wB	A�B	C�B	C�B	C�B	F�B	H�B	K�B	K�B	K�B	N�B	T�B	\)B	jB	jB	l�B	q�B	s�B	r�B	t�B	x�B	|�B	�B	�%B	�%B	�1B	�JB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�!B	�!B	�!B	�'B	�3B	�9B	�3B	�9B	�?B	�?B	�FB	�^B	�^B	�dB	�jB	�jB	�dB	�dB	�dB	�jB	�jB	�qB	�qB	�wB	�wB	�}B	��B	��B	ÖB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�#B	�)B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�ZB	�fB	�mB	�mB	�mB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
%B
B
B
%B
%B
%B
+B
�B
�B
*2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
�\B
�\B
�\B
�\B
�\B
�VB
�VB
�DB
�VB
��B
�B
��B
�B
�B
�B
�!B
�!B
�'B
�9B
�9B
�9B
�3B
�9B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�LB
�LB
�LB
�XB
�^B
�dB
�dB
�RB
�3B
�B
�B
�-B
�LB
�LB
�dB
��B
��B1B�B-BI�BjB�oB�B�XB��BŢB��B�)B�5B�B��B��B��B��BBB%B
=BVB{B+B�B�`B�BB�;B�B�)B�B�B�B�BuB%�B�B�B�B'�B(�B'�B(�B �BB�B��B�B��B��BdZBM�BF�B8RB�B�B �B
��B
�fB
��B
��B
�!B
t�B
W
B
K�B
.B	��B	�5B	��B	�3B	��B	��B	�JB	�B	v�B	p�B	^5B	I�B	.B	"�B	oB	1B	B��B��B��B��B�B�B�B�mB�`B�TB�HB�;B�B��B��B��B��BȴBƨBÖB��B�qB�dB�RB�dB�wB�wB�}BBƨB��B�B�/B�mB�B��B��B��B��B��B�B�B��B��B�fB��B��B��B��B��B��BŢB�B��B��B�-B��B�B�B��B��BB�!B��B��B��B�uB�PB�DB�=B�7B�7B�+B�+B�%B�%B�B�%B�B�JB�VB�PB�uB�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�B�XB�qB�}BBÖBÖBĜBĜBĜBƨB��B��B��B��B��B��B��B��B�
B��B��B��B�B�`B�fB�mB�fB�fB�sB�B�B�B�B�sB�fB�B�B��B��B��B��B��B��B	B	\B	�B	�B	 �B	 �B	 �B	�B	 �B	$�B	$�B	$�B	$�B	$�B	(�B	-B	1'B	49B	7LB	7LB	9XB	;dB	=qB	>wB	A�B	C�B	C�B	C�B	F�B	H�B	K�B	K�B	K�B	N�B	T�B	\)B	jB	jB	l�B	q�B	s�B	r�B	t�B	x�B	|�B	�B	�%B	�%B	�1B	�JB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�!B	�!B	�!B	�'B	�3B	�9B	�3B	�9B	�?B	�?B	�FB	�^B	�^B	�dB	�jB	�jB	�dB	�dB	�dB	�jB	�jB	�qB	�qB	�wB	�wB	�}B	��B	��B	ÖB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�#B	�)B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�ZB	�fB	�mB	�mB	�mB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
%B
B
B
%B
%B
%B
+B
�B
�B
*2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190506                              AO  ARCAADJP                                                                    20181005190506    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190506  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190506  QCF$                G�O�G�O�G�O�8000            