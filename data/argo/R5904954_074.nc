CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:05Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191705  20181005191705  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               JA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�����c 1   @���hK��@50 ě���d!�E��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      JA   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A���A���A�33B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C�C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl�Cn  Co�fCq�fCs�fCv  Cx�Cz�C|  C~  C�fC�  C�  C�  C�  C�  C��3C��3C��3C�  C��3C��3C�  C��C��C��C��C��C�  C��3C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C��C�  C�  C�  C��C��C��C�  C�  C��C��3C��3C�  C��C�  C�  C�  C�  C�  C��C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C��C��C�  C��C��3C��C��3C��3C��3C��3C��3C�  C�  C�  C��C�  C�  C��C�  C��3C�  C�  C��3C��3C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C��C�  C��3C�  C��C��3C�  C��C�  C��C��C��C�  C�  C��C�  C��3C�  C��3C�  C��3D   D �fD  Dy�D��Dy�DfD� D  D�fD  D� D��D� DfD�fDfD� D��D	� D
fD
� D
��Ds3D��Dy�D�3Dy�D  D� DfDy�D��Dy�D  Dy�D��D� D��D� D��Dy�D��Dy�D��D� D  D� D  Dy�D��D�fD  Dy�D  D� D��D� D  D� DfD� D��D�fD fD �fD ��D!y�D!��D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'fD'y�D(  D(�fD)fD)�fD*fD*� D+  D+�fD,  D,y�D-  D-� D-��D.� D/  D/� D/��D0� D1  D1� D1��D2s3D2��D3� D4  D4�fD5  D5�fD6  D6y�D6��D7y�D7��D8y�D9fD9� D9��D:y�D;  D;� D<  D<y�D=  D=� D=��D>� D?fD?� D@  D@� DA  DAy�DB  DB� DB��DC� DD  DD�fDEfDE� DE�3DFy�DGfDG�fDG��DHy�DH��DI� DJfDJ� DJ��DK� DLfDL�fDM  DM� DN  DNy�DO  DO� DO��DP�fDQfDQy�DQ��DRy�DS  DSy�DS��DT� DUfDU�fDU��DVy�DW  DW� DXfDX� DX��DY� DZfDZ�fD[  D[� D\  D\� D]  D]y�D]��D^y�D_  D_y�D`  D`�fDafDay�Db  Db�fDcfDc�fDdfDd�fDefDe� Df  Df� Df��Dg� Dh  Dh� Di  Diy�Di��Djy�DkfDk�fDl  Dl� Dm  Dm� Dn  Dny�Do  Do�fDpfDp�fDq  Dqy�Dr  Dr�fDs  Dsy�Dt  Dt�fDu  Du� Dv  Dvy�Dw  Dw�fDw��Dy��D�8RD��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @@  @�ff@�ffA33A#33AC33Ac33A���A���A���A���A���A���A�fgA�fgB ffB��B��B��B ��B(��B0��B8��BA33BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB���B�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB虙B�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33CL�C33C33C33CL�C33C33C L�C"33C$33C&33C(33C*33C,33C.33C033C2L�C433C633C833C:33C<L�C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33CjL�ClL�Cn33Cp�Cr�Ct�Cv33CxL�CzL�C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC�&gC�&gC�&gC�&gC��C��C��C��C��C��C��C�&gC��C��C��C�&gC��C��C��C�&gC��C��C��C�&gC�&gC�&gC��C��C�&gC��C��C��C�&gC��C��C��C��C��C�&gC��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC�&gC��C�&gC��C�&gC��C��C��C��C��C��C��C��C�&gC��C��C�&gC��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C�&gC��C��C��C�&gC��C��C�&gC��C�&gC�&gC�&gC��C��C�&gC��C��C��C��C��C��D �D �3D�D�gDgD�gD3D��D�D�3D�D��DgD��D3D�3D3D��D	gD	��D
3D
��DgD� DgD�gD  D�gD�D��D3D�gDgD�gD�D�gDgD��DgD��DgD�gDgD�gDgD��D�D��D�D�gDgD�3D�D�gD�D��DgD��D�D��D3D��DgD�3D 3D �3D!gD!�gD"gD"��D#�D#��D$�D$��D%�D%��D&�D&�3D'3D'�gD(�D(�3D)3D)�3D*3D*��D+�D+�3D,�D,�gD-�D-��D.gD.��D/�D/��D0gD0��D1�D1��D2gD2� D3gD3��D4�D4�3D5�D5�3D6�D6�gD7gD7�gD8gD8�gD93D9��D:gD:�gD;�D;��D<�D<�gD=�D=��D>gD>��D?3D?��D@�D@��DA�DA�gDB�DB��DCgDC��DD�DD�3DE3DE��DF  DF�gDG3DG�3DHgDH�gDIgDI��DJ3DJ��DKgDK��DL3DL�3DM�DM��DN�DN�gDO�DO��DPgDP�3DQ3DQ�gDRgDR�gDS�DS�gDTgDT��DU3DU�3DVgDV�gDW�DW��DX3DX��DYgDY��DZ3DZ�3D[�D[��D\�D\��D]�D]�gD^gD^�gD_�D_�gD`�D`�3Da3Da�gDb�Db�3Dc3Dc�3Dd3Dd�3De3De��Df�Df��DggDg��Dh�Dh��Di�Di�gDjgDj�gDk3Dk�3Dl�Dl��Dm�Dm��Dn�Dn�gDo�Do�3Dp3Dp�3Dq�Dq�gDr�Dr�3Ds�Ds�gDt�Dt�3Du�Du��Dv�Dv�gDw�Dw�3DwٚDy�\D�>�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�=qA�1'A�(�A��A�ĜAۣ�Aۉ7A�r�A�ffA�`BA�^5A�VA�
=A�  A��A�^5A��A�
=A���AͲ-A���A�ȴA�?}Aɥ�A�
=A��mA�t�A�&�AƩ�A���AĸRAăA�K�A��A�n�A�33A��A�$�A�bA�oA��A�O�A��mA���A�=qA�1'A���A���A�XA�
=A��yA��A��;A���A�O�A���A��A��mA��A�|�A��#A���A�{A�?}A��DA�A�oA���A��9A��!A��`A��FA��`A��A���A�A�33A���A�7LA���A�hsA���A��DA��;A�7LA�M�A��yA�ĜA��A�n�A�A���A�ZA���A�bNA�;dA���A�M�A�l�A��
A��A��A���A}�FA|�+AzQ�Ax�AxI�Aw��Av(�As�;Aq��AnI�Aj^5AhA�Ag&�AfffAd�/Ac�Aap�A`��A^��AY�FAW��AW"�AVjAV�AUC�AS
=AQ�7APz�AN�AH^5AE;dAC�;AA+A>  A=&�A<�A<r�A;��A;\)A9��A9XA9"�A6��A6(�A4ffA2��A2��A2��A1�A1C�A0��A0A,��A,9XA+��A+;dA*�/A*M�A)l�A'�mA';dA&��A&=qA%`BA#G�A"n�A!�
A!
=A ĜA 1'AA�A�A A JA�uA��A(�A  A�PA�A��AXA5?A`BA�!A�`A?}A�A��AjAp�AG�A�mAhsA��AA�+A�A
r�A	33AC�A��AjAƨA��A�A (�@�O�@�t�@�~�@�7L@��D@���@��@�1'@��-@�l�@��/@�o@��/@�9@��@�7@�D@�7L@�\@�5?@�\)@� �@�C�@�+@��@��@�t�@ݡ�@��@�;d@֟�@�(�@��
@�Z@ҸR@Χ�@��@�ff@Ѳ-@с@���@���@θR@͉7@�ƨ@��/@�A�@�\)@θR@�?}@̼j@�A�@�dZ@�{@�G�@� �@�l�@�-@�O�@�^5@�J@�p�@�Ĝ@ļj@���@�O�@�(�@�V@�V@�&�@�`B@���@�A�@��!@��\@�v�@�v�@�$�@��#@�G�@�j@��@�V@�/@���@��9@�bN@� �@��w@�|�@�;d@��R@�~�@�{@�x�@���@�I�@��@��y@�=q@�7L@��u@�S�@�C�@�33@�o@��@��@���@�=q@��#@��#@��#@��h@�G�@���@�z�@�/@��@���@���@��u@�9X@�I�@�I�@� �@��m@�ƨ@��P@�t�@�dZ@�"�@���@���@�ff@���@��h@�/@���@�Q�@�t�@���@���@��+@�/@���@�t�@�K�@��@���@�@�;d@�S�@��@���@�|�@�|�@�l�@�\)@�K�@�C�@��@��R@���@���@���@��\@�^5@��@���@���@�%@� �@�K�@�+@��R@�-@��@�J@�@��#@���@�x�@�?}@��@��`@��D@��@���@�K�@�33@���@���@�n�@�5?@�5?@�J@��^@���@��D@�Z@���@��u@�(�@�1@��m@�ƨ@�
=@�~�@�v�@��\@�~�@�5?@��@�{@�{@��#@���@�p�@�X@�/@��@��/@��@��u@�z�@��@���@�S�@�o@��R@�ȴ@���@�ȴ@���@���@�@��-@���@�x�@�X@�%@���@�Ĝ@��9@�z�@�I�@�b@�ƨ@���@�33@�;d@��y@��!@�~�@�M�@��@��-@�p�@�&�@�Ĝ@��j@��9@��D@�r�@�I�@��;@���@��@��@�@��H@�v�@��@���@���@��7@�hs@��@���@��j@��@��@���@���@~ �@l�?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1'A�=qA�1'A�(�A��A�ĜAۣ�Aۉ7A�r�A�ffA�`BA�^5A�VA�
=A�  A��A�^5A��A�
=A���AͲ-A���A�ȴA�?}Aɥ�A�
=A��mA�t�A�&�AƩ�A���AĸRAăA�K�A��A�n�A�33A��A�$�A�bA�oA��A�O�A��mA���A�=qA�1'A���A���A�XA�
=A��yA��A��;A���A�O�A���A��A��mA��A�|�A��#A���A�{A�?}A��DA�A�oA���A��9A��!A��`A��FA��`A��A���A�A�33A���A�7LA���A�hsA���A��DA��;A�7LA�M�A��yA�ĜA��A�n�A�A���A�ZA���A�bNA�;dA���A�M�A�l�A��
A��A��A���A}�FA|�+AzQ�Ax�AxI�Aw��Av(�As�;Aq��AnI�Aj^5AhA�Ag&�AfffAd�/Ac�Aap�A`��A^��AY�FAW��AW"�AVjAV�AUC�AS
=AQ�7APz�AN�AH^5AE;dAC�;AA+A>  A=&�A<�A<r�A;��A;\)A9��A9XA9"�A6��A6(�A4ffA2��A2��A2��A1�A1C�A0��A0A,��A,9XA+��A+;dA*�/A*M�A)l�A'�mA';dA&��A&=qA%`BA#G�A"n�A!�
A!
=A ĜA 1'AA�A�A A JA�uA��A(�A  A�PA�A��AXA5?A`BA�!A�`A?}A�A��AjAp�AG�A�mAhsA��AA�+A�A
r�A	33AC�A��AjAƨA��A�A (�@�O�@�t�@�~�@�7L@��D@���@��@�1'@��-@�l�@��/@�o@��/@�9@��@�7@�D@�7L@�\@�5?@�\)@� �@�C�@�+@��@��@�t�@ݡ�@��@�;d@֟�@�(�@��
@�Z@ҸR@Χ�@��@�ff@Ѳ-@с@���@���@θR@͉7@�ƨ@��/@�A�@�\)@θR@�?}@̼j@�A�@�dZ@�{@�G�@� �@�l�@�-@�O�@�^5@�J@�p�@�Ĝ@ļj@���@�O�@�(�@�V@�V@�&�@�`B@���@�A�@��!@��\@�v�@�v�@�$�@��#@�G�@�j@��@�V@�/@���@��9@�bN@� �@��w@�|�@�;d@��R@�~�@�{@�x�@���@�I�@��@��y@�=q@�7L@��u@�S�@�C�@�33@�o@��@��@���@�=q@��#@��#@��#@��h@�G�@���@�z�@�/@��@���@���@��u@�9X@�I�@�I�@� �@��m@�ƨ@��P@�t�@�dZ@�"�@���@���@�ff@���@��h@�/@���@�Q�@�t�@���@���@��+@�/@���@�t�@�K�@��@���@�@�;d@�S�@��@���@�|�@�|�@�l�@�\)@�K�@�C�@��@��R@���@���@���@��\@�^5@��@���@���@�%@� �@�K�@�+@��R@�-@��@�J@�@��#@���@�x�@�?}@��@��`@��D@��@���@�K�@�33@���@���@�n�@�5?@�5?@�J@��^@���@��D@�Z@���@��u@�(�@�1@��m@�ƨ@�
=@�~�@�v�@��\@�~�@�5?@��@�{@�{@��#@���@�p�@�X@�/@��@��/@��@��u@�z�@��@���@�S�@�o@��R@�ȴ@���@�ȴ@���@���@�@��-@���@�x�@�X@�%@���@�Ĝ@��9@�z�@�I�@�b@�ƨ@���@�33@�;d@��y@��!@�~�@�M�@��@��-@�p�@�&�@�Ĝ@��j@��9@��D@�r�@�I�@��;@���@��@��@�@��H@�v�@��@���@���@��7@�hs@��@���@��j@��@��@���@���@~ �@l�?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�LB�LB�?B�9B�-B��B��B��B��B�B�B�B�B��B��B��B��B�B�?BĜB��B�B�NB��B��BB{B�B�B!�B-B=qB?}BA�BE�B[#BgmBhsBo�By�B~�B�=B�PB�\B�\B�\B�oB��B��B��B��B�RB�dB�RB�'B��B��B��B��B��B�oB|�Bn�BffBR�BJ�BC�B9XB&�BbB��B�B�#B��B�FB�B��B��B�{B�PB�B�Bz�Bs�BjB`BBQ�B5?B2-B0!B,B%�BuBB
��B
��B
�B
�fB
�}B
��B
�\B
�%B
}�B
n�B
G�B
<jB
.B
!�B
�B
�B
DB	��B	�B	��B	�RB	��B	��B	��B	�hB	�B	x�B	q�B	bNB	G�B	;dB	6FB	2-B	/B	(�B	�B	�B	{B	%B�B�TB�/B��BǮBĜBÖB��B�}B�qB�dB�RB�FB�-B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B�B�B��B��B�!B�qB�jB�9B�'B�B�B�LB�^B��B��B��B��BȴBĜB�^B�B��B��B��B�oB�7B�B|�Bz�Bw�By�B�hB��B��B��B��B��B��B�{B��B�bB~�B�+B�oB��B��B��B�{B�{B��B��B��B��B�{B��B�\B�B{�Bz�B}�B�Bx�B�B��B��B��B��B��B��B��B�-BBŢBǮB��B��B��B��B��B��BȴBǮBɺB��B��B�/B�TB�`B�ZB�`B�mB�B�B�B��B��B��B��B��B��B��B	  B	B	B	%B	1B	DB	PB	uB	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	#�B	$�B	'�B	,B	5?B	8RB	9XB	8RB	8RB	9XB	;dB	=qB	?}B	@�B	D�B	J�B	K�B	K�B	M�B	T�B	[#B	_;B	`BB	`BB	bNB	iyB	l�B	n�B	o�B	p�B	p�B	q�B	r�B	t�B	u�B	v�B	x�B	y�B	y�B	z�B	{�B	~�B	� B	� B	� B	�B	�B	�B	� B	~�B	~�B	�B	�B	� B	�B	�B	�B	�+B	�PB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�-B	�3B	�3B	�3B	�3B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�dB	�dB	�jB	��B	ÖB	B	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�/B	�;B	�;B	�BB	�NB	�NB	�NB	�ZB	�`B	�`B	�`B	�fB	�sB	�yB	�yB	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
	7B
	7B
	7B

=B

=B
JB
�B
�B
&�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�LB�LB�?B�9B�-B��B��B��B��B�B�B�B�B��B��B��B��B�B�?BĜB��B�B�NB��B��BB{B�B�B!�B-B=qB?}BA�BE�B[#BgmBhsBo�By�B~�B�=B�PB�\B�\B�\B�oB��B��B��B��B�RB�dB�RB�'B��B��B��B��B��B�oB|�Bn�BffBR�BJ�BC�B9XB&�BbB��B�B�#B��B�FB�B��B��B�{B�PB�B�Bz�Bs�BjB`BBQ�B5?B2-B0!B,B%�BuBB
��B
��B
�B
�fB
�}B
��B
�\B
�%B
}�B
n�B
G�B
<jB
.B
!�B
�B
�B
DB	��B	�B	��B	�RB	��B	��B	��B	�hB	�B	x�B	q�B	bNB	G�B	;dB	6FB	2-B	/B	(�B	�B	�B	{B	%B�B�TB�/B��BǮBĜBÖB��B�}B�qB�dB�RB�FB�-B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B�B�B��B��B�!B�qB�jB�9B�'B�B�B�LB�^B��B��B��B��BȴBĜB�^B�B��B��B��B�oB�7B�B|�Bz�Bw�By�B�hB��B��B��B��B��B��B�{B��B�bB~�B�+B�oB��B��B��B�{B�{B��B��B��B��B�{B��B�\B�B{�Bz�B}�B�Bx�B�B��B��B��B��B��B��B��B�-BBŢBǮB��B��B��B��B��B��BȴBǮBɺB��B��B�/B�TB�`B�ZB�`B�mB�B�B�B��B��B��B��B��B��B��B	  B	B	B	%B	1B	DB	PB	uB	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	#�B	$�B	'�B	,B	5?B	8RB	9XB	8RB	8RB	9XB	;dB	=qB	?}B	@�B	D�B	J�B	K�B	K�B	M�B	T�B	[#B	_;B	`BB	`BB	bNB	iyB	l�B	n�B	o�B	p�B	p�B	q�B	r�B	t�B	u�B	v�B	x�B	y�B	y�B	z�B	{�B	~�B	� B	� B	� B	�B	�B	�B	� B	~�B	~�B	�B	�B	� B	�B	�B	�B	�+B	�PB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�-B	�3B	�3B	�3B	�3B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�dB	�dB	�jB	��B	ÖB	B	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�/B	�;B	�;B	�BB	�NB	�NB	�NB	�ZB	�`B	�`B	�`B	�fB	�sB	�yB	�yB	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
	7B
	7B
	7B

=B

=B
JB
�B
�B
&�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191705                              AO  ARCAADJP                                                                    20181005191705    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191705  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191705  QCF$                G�O�G�O�G�O�8000            