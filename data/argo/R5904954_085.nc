CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:08Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005191708  20181005191708  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               UA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�ĤͿ��1   @�ĥW:۬@4�~��"��d3dZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      UA   A   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B
  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC�fC�fC  C�C  C  C  C  C�fC  C   C"  C$  C&  C(  C)�fC+�fC.  C0�C2  C4  C6�C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CQ�fCS�fCU�fCX  CZ  C[�fC^  C`  Ca�fCc�fCe�fCg�fCj  Cl  Cm�fCp  Cr  Cs�fCv  Cw�fCy�fC{�fC~  C��C�  C�  C�  C��C�  C��3C�  C��C��C��3C��fC�  C��C�  C��C��C�  C�  C�  C��fC�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C��C��3C�  C�  C�  C��C�  C�  C�  C��C��C��C�  C��3C�  C��3C�  C�  C��3C�  C�  C�  C�  C��C��3C�  C�  C�  C��C�  C��3C��C�  C��3C��3C�  C��C��C��C��C��C�  C��C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C��3C��3C��C��C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��C�  C��3C��3C�  C��C��C��C�  C��3C��C��C�  C��C�  C��C�  C��3C��3C�  C��C�  C�  C�  C��3C��3C�  D   D � DfD� D  D�fD  Dy�D��Dy�D�3Dy�D��Dy�D  D� D  D� D	  D	� D	��D
� D  Dy�D��D� D  D� D  Dy�D��D� D  D� D��D�fDfD� D��Dy�D��D� DfD�fD  D� DfD� D��Dy�D  D� D��D� D  D� D��D� DfD� D��D� D  Dy�D fD �fD!  D!y�D!��D"y�D.� D/fD/� D0fD0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6fD6y�D6��D7y�D8fD8�fD9  D9� D:  D:�fD;  D;y�D;��D<y�D<��D=y�D>  D>� D?fD?�fD@fD@�fDAfDA� DB  DB�fDC  DCy�DDfDD� DEfDE� DF  DFy�DG  DG� DH  DH� DH��DIs3DI��DJy�DK  DK� DL  DLy�DL��DM� DN  DN� DOfDO� DP  DP� DQ  DQ�fDR  DR� DS  DS�fDT  DTy�DU  DU� DU��DVy�DV��DWy�DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D^��D_y�D`  D`�fDa  Da�fDbfDb�fDc  Dc� Dc��Dd� De  De� Df  Dfy�DgfDg�fDhfDh� Di  Di� Dj  Dj� Dk  Dky�DlfDl�fDm  Dm� Dn  Dn�fDo  Doy�Do��Dpy�Dp�3Dq� DrfDr� Dr��Ds� Dt  Dt� DufDu�fDvfDv� Dv��Dw� Dw� Dy��D�@�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@<��@���@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B
33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B�L�B��B��B��B��B��gB��B��B��B��B��B�L�B��B��gB��B��B�L�B��B��B��B��B��B�L�B��B��B��B��B��B��B��C �C�C�C�C�C	�3C�3C�3C�C&gC�C�C�C�C�3C�C �C"�C$�C&�C(�C)�3C+�3C.�C0&gC2�C4�C6&gC8�C9�3C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN&gCP�CQ�3CS�3CU�3CX�CZ�C[�3C^�C`�Ca�3Cc�3Ce�3Cg�3Cj�Cl�Cm�3Cp�Cr�Cs�3Cv�Cw�3Cy�3C{�3C~�C�  C�fC�fC�fC�3C�fC���C�fC�3C�3C���C���C�fC�3C�fC�3C�3C�fC�fC�fC���C�fC�fC�fC�3C�fC���C���C�fC�fC�fC�fC�3C���C�fC�fC�fC�3C�fC�fC�fC�3C�3C�3C�fC���C�fC���C�fC�fC���C�fC�fC�fC�fC�3C���C�fC�fC�fC�3C�fC���C�3C�fC���C���C�fC�3C�3C�3C�3C�3C�fC�3C�fC�fC�3C�fC�fC�fC���C���C�fC�fC���C���C�3C�  C�3C�3C�fC���C�fC�fC�fC�fC�fC�fC�fC���C�3C�fC���C���C�fC�3C�3C�3C�fC���C�3C�3C�fC�3C�fC�3C�fC���C���C�fC�3C�fC�fC�fC���C���C�fD 3D �3D	�D�3D3D��D3D|�D��D|�D�fD|�D��D|�D3D�3D3D�3D	3D	�3D	��D
�3D3D|�D��D�3D3D�3D3D|�D��D�3D3D�3D��D��D	�D�3D��D|�D��D�3D	�D��D3D�3D	�D�3D��D|�D3D�3D��D�3D3D�3D��D�3D	�D�3D��D�3D3D|�D 	�D ��D!3D!|�D!��D"|�D.�3D/	�D/�3D0	�D0�3D13D1�3D2	�D2�3D33D3�3D43D4�3D53D5�3D6	�D6|�D6��D7|�D8	�D8��D93D9�3D:3D:��D;3D;|�D;��D<|�D<��D=|�D>3D>�3D?	�D?��D@	�D@��DA	�DA�3DB3DB��DC3DC|�DD	�DD�3DE	�DE�3DF3DF|�DG3DG�3DH3DH�3DH��DIvfDI��DJ|�DK3DK�3DL3DL|�DL��DM�3DN3DN�3DO	�DO�3DP3DP�3DQ3DQ��DR3DR�3DS3DS��DT3DT|�DU3DU�3DU��DV|�DV��DW|�DX3DX��DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]	�D]�3D^3D^�3D^��D_|�D`3D`��Da3Da��Db	�Db��Dc3Dc�3Dc��Dd�3De3De�3Df3Df|�Dg	�Dg��Dh	�Dh�3Di3Di�3Dj3Dj�3Dk3Dk|�Dl	�Dl��Dm3Dm�3Dn3Dn��Do3Do|�Do��Dp|�Dp�fDq�3Dr	�Dr�3Dr��Ds�3Dt3Dt�3Du	�Du��Dv	�Dv�3Dv��Dw�3Dw�3Dy��D�B>D��]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aܲ-A�A�A�ĜA�ƨA�ƨA�ƨA�ƨA�ĜA�ƨA�ƨA���A���A���A�ȴA���A���A�jAѥ�A��A��AЃAΑhẠ�AˬA��Aɴ9A�
=A���A�;dAď\A�(�A�jA�33A��A��wA�A�A���A���A��
A��FA�VA�E�A��A��`A�VA��/A��A�dZA�{A���A�ZA�A��7A�{A�l�A�{A��A�1A��HA�$�A���A��A���A��!A�l�A�&�A��#A��A���A��A���A��A�|�A��jA���A�A�A��/A�ffA�9XA�1A�oA�C�A�n�A� �A���A��mA�v�A���A��\A��7A���A���A�~�A�?}A��TA���A�A|=qAx�Av�uAvbAtz�As;dArI�AmS�Ai��Ag��AgK�Ag"�Afz�AeC�AaC�A^��A]t�A\�A\{AZZAW��AS�APjAO33AMƨAL$�AJ1AF�HAEoAC�wAB��AB-AAt�A@r�A=��A;/A9`BA8�9A77LA5t�A2A0�DA/�A/|�A.�A.jA,�A*��A)hsA($�A%/A#�^A#;dA"VA!/AhsA~�A�AO�AVAjAS�A�jA��A�AƨA�A�HA��A�!A�A��AS�A&�A�uA=qA(�A�An�A{A�PAK�A"�A
=AffA�A	�A��A�A��A�HA{A�A�hA"�A(�A �@��F@�^5@���@��/@��m@�5?@���@���@�@��7@��m@��#@�  @�P@�n�@���@��@�V@���@�P@�M�@�7@�b@�ff@�^@�+@ݑh@� �@۾w@�t�@ڗ�@�$�@ٲ-@�/@��@�r�@�dZ@���@֟�@֏\@�J@ա�@��/@�C�@�{@�7L@Гu@�C�@�M�@��@͡�@�x�@�%@���@��@��y@��#@�j@���@Õ�@�o@�b@�1@��
@�5?@�$�@�r�@�dZ@�S�@��\@�v�@�ff@��\@�\)@�M�@���@�p�@��/@��u@�r�@�1@���@�r�@��H@�-@��@��@��j@��/@���@��9@�9X@��@�+@�@��@�K�@�;d@�o@��\@�V@�E�@�E�@�@�@�v�@���@�Ĝ@�j@�dZ@�33@��^@�S�@�J@��F@��!@�-@�hs@�^5@�+@���@���@�  @��@�?}@��@���@�^5@���@��^@���@�|�@�l�@�t�@�t�@�t�@�t�@�t�@�t�@�l�@�K�@�C�@�;d@�"�@��R@��+@�ff@��#@��h@�hs@�?}@��@�z�@��@���@�\)@�+@�
=@��+@�=q@���@��T@��T@���@�O�@��/@��@�1@���@�C�@��@��y@�ȴ@���@�ff@�-@�-@�-@��@��`@���@�Z@�9X@�  @��
@�|�@�+@��@���@�^5@�E�@�@��7@�V@���@�(�@�1@���@��@�1@��@�1@��;@��F@���@��@�K�@�@���@�n�@�@��#@��T@��@��@��T@���@��@�@���@��7@�p�@�`B@��`@�z�@�9X@�(�@�(�@� �@��@�l�@�;d@�"�@�
=@���@�v�@�ff@�E�@�{@��@��T@��^@��-@���@��7@�p�@�?}@���@���@��@��D@�r�@�9X@� �@��@���@�|�@�S�@�K�@��@���@��H@���@�~�@�^5@�$�@�@�`B@�G�@�/@��@��`@��D@�bN@�b@���@���@��@��f@w{J@b�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aܲ-A�A�A�ĜA�ƨA�ƨA�ƨA�ƨA�ĜA�ƨA�ƨA���A���A���A�ȴA���A���A�jAѥ�A��A��AЃAΑhẠ�AˬA��Aɴ9A�
=A���A�;dAď\A�(�A�jA�33A��A��wA�A�A���A���A��
A��FA�VA�E�A��A��`A�VA��/A��A�dZA�{A���A�ZA�A��7A�{A�l�A�{A��A�1A��HA�$�A���A��A���A��!A�l�A�&�A��#A��A���A��A���A��A�|�A��jA���A�A�A��/A�ffA�9XA�1A�oA�C�A�n�A� �A���A��mA�v�A���A��\A��7A���A���A�~�A�?}A��TA���A�A|=qAx�Av�uAvbAtz�As;dArI�AmS�Ai��Ag��AgK�Ag"�Afz�AeC�AaC�A^��A]t�A\�A\{AZZAW��AS�APjAO33AMƨAL$�AJ1AF�HAEoAC�wAB��AB-AAt�A@r�A=��A;/A9`BA8�9A77LA5t�A2A0�DA/�A/|�A.�A.jA,�A*��A)hsA($�A%/A#�^A#;dA"VA!/AhsA~�A�AO�AVAjAS�A�jA��A�AƨA�A�HA��A�!A�A��AS�A&�A�uA=qA(�A�An�A{A�PAK�A"�A
=AffA�A	�A��A�A��A�HA{A�A�hA"�A(�A �@��F@�^5@���@��/@��m@�5?@���@���@�@��7@��m@��#@�  @�P@�n�@���@��@�V@���@�P@�M�@�7@�b@�ff@�^@�+@ݑh@� �@۾w@�t�@ڗ�@�$�@ٲ-@�/@��@�r�@�dZ@���@֟�@֏\@�J@ա�@��/@�C�@�{@�7L@Гu@�C�@�M�@��@͡�@�x�@�%@���@��@��y@��#@�j@���@Õ�@�o@�b@�1@��
@�5?@�$�@�r�@�dZ@�S�@��\@�v�@�ff@��\@�\)@�M�@���@�p�@��/@��u@�r�@�1@���@�r�@��H@�-@��@��@��j@��/@���@��9@�9X@��@�+@�@��@�K�@�;d@�o@��\@�V@�E�@�E�@�@�@�v�@���@�Ĝ@�j@�dZ@�33@��^@�S�@�J@��F@��!@�-@�hs@�^5@�+@���@���@�  @��@�?}@��@���@�^5@���@��^@���@�|�@�l�@�t�@�t�@�t�@�t�@�t�@�t�@�l�@�K�@�C�@�;d@�"�@��R@��+@�ff@��#@��h@�hs@�?}@��@�z�@��@���@�\)@�+@�
=@��+@�=q@���@��T@��T@���@�O�@��/@��@�1@���@�C�@��@��y@�ȴ@���@�ff@�-@�-@�-@��@��`@���@�Z@�9X@�  @��
@�|�@�+@��@���@�^5@�E�@�@��7@�V@���@�(�@�1@���@��@�1@��@�1@��;@��F@���@��@�K�@�@���@�n�@�@��#@��T@��@��@��T@���@��@�@���@��7@�p�@�`B@��`@�z�@�9X@�(�@�(�@� �@��@�l�@�;d@�"�@�
=@���@�v�@�ff@�E�@�{@��@��T@��^@��-@���@��7@�p�@�?}@���@���@��@��D@�r�@�9X@� �@��@���@�|�@�S�@�K�@��@���@��H@���@�~�@�^5@�$�@�@�`B@�G�@�/@��@��`@��D@�bN@�b@���@���@��@��f@w{J@b�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B+B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B&�B#�B+B%B+B	7BhB�B"�B&�B/B>wBH�BW
B[#B]/BaHBjBq�Br�Bt�Bz�B�B�7B�\B�uB��B�{B��B��B��B��B�oB�\B�PB�1B�Bz�Bq�Bm�BbNBS�BF�B=qB=qB@�B8RB+B�BPB�B��BɺBǮBŢB��B�RB�'B��B�JBu�B_;BH�B6FB#�BuBB
��B
�fB
��B
ȴB
��B
�^B
��B
�{B
�B
x�B
s�B
o�B
gmB
YB
H�B
33B
�B
+B
B	��B	�B	�;B	��B	��B	��B	��B	��B	�bB	�1B	q�B	dZB	[#B	W
B	O�B	F�B	7LB	$�B	�B	PB	%B��B�B�B�TB�NB�BB�/B�B��BɺBÖB�qB�XB�?B�B��B��B��B��B��B��B��B��B�uB�hB�hB�hB�bB�bB�\B�bB�bB�bB�oB�hB�hB�hB�hB�oB�oB�uB�uB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�\B�PB�DB�1B�B� B}�B{�B~�B�B�B� B�B�B�1B�\B�hB�bB�{B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�!B�'B�-B�9B�RB�^B�jB�wB�wB��BBŢBƨBǮBǮB��B��B�qB�XB�dB�qB�jBÖBȴB��B�
B��B��B��B��B��B��B��B��B�B�;B�`B�TB�NB�BB�;B�)B��BȴBĜBB��B��BĜBǮBǮBɺB��B��B��B��B�NB�B�B�B��B		7B	�B	2-B	:^B	>wB	@�B	?}B	@�B	D�B	D�B	F�B	F�B	=qB	8RB	0!B	0!B	2-B	J�B	R�B	ZB	\)B	ZB	^5B	YB	T�B	S�B	Q�B	R�B	VB�B	�%B	�%B	�+B	�+B	�1B	�7B	�DB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�3B	�FB	�LB	�XB	�XB	�XB	�^B	�jB	�wB	��B	ÖB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�)B	�5B	�;B	�HB	�TB	�ZB	�`B	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
+B
+B
1B
	7B

=B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
bB
hB
hB
{B
NB
IB
$Z22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B+B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B&�B#�B+B%B+B	7BhB�B"�B&�B/B>wBH�BW
B[#B]/BaHBjBq�Br�Bt�Bz�B�B�7B�\B�uB��B�{B��B��B��B��B�oB�\B�PB�1B�Bz�Bq�Bm�BbNBS�BF�B=qB=qB@�B8RB+B�BPB�B��BɺBǮBŢB��B�RB�'B��B�JBu�B_;BH�B6FB#�BuBB
��B
�fB
��B
ȴB
��B
�^B
��B
�{B
�B
x�B
s�B
o�B
gmB
YB
H�B
33B
�B
+B
B	��B	�B	�;B	��B	��B	��B	��B	��B	�bB	�1B	q�B	dZB	[#B	W
B	O�B	F�B	7LB	$�B	�B	PB	%B��B�B�B�TB�NB�BB�/B�B��BɺBÖB�qB�XB�?B�B��B��B��B��B��B��B��B��B�uB�hB�hB�hB�bB�bB�\B�bB�bB�bB�oB�hB�hB�hB�hB�oB�oB�uB�uB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�\B�PB�DB�1B�B� B}�B{�B~�B�B�B� B�B�B�1B�\B�hB�bB�{B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�!B�'B�-B�9B�RB�^B�jB�wB�wB��BBŢBƨBǮBǮB��B��B�qB�XB�dB�qB�jBÖBȴB��B�
B��B��B��B��B��B��B��B��B�B�;B�`B�TB�NB�BB�;B�)B��BȴBĜBB��B��BĜBǮBǮBɺB��B��B��B��B�NB�B�B�B��B		7B	�B	2-B	:^B	>wB	@�B	?}B	@�B	D�B	D�B	F�B	F�B	=qB	8RB	0!B	0!B	2-B	J�B	R�B	ZB	\)B	ZB	^5B	YB	T�B	S�B	Q�B	R�B	VB�B	�%B	�%B	�+B	�+B	�1B	�7B	�DB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�3B	�FB	�LB	�XB	�XB	�XB	�^B	�jB	�wB	��B	ÖB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�)B	�5B	�;B	�HB	�TB	�ZB	�`B	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
+B
+B
1B
	7B

=B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
bB
hB
hB
{B
NB
IB
$Z22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191708                              AO  ARCAADJP                                                                    20181005191708    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191708  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191708  QCF$                G�O�G�O�G�O�8000            