CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:07Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005191707  20181005191707  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               OA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$܊1   @��%ffy@5��$��d*��O�;1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      OA   A   B   @�ff@�  A   AffA>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB133B6��B?��BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B���B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC�fC  C  C  C  C�fC  C�C�C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCC�fCE�fCG�fCJ  CL  CM�fCO�fCQ�fCS�fCV�CX�CZ  C[�fC]��C`  Cb  Cc�fCf  Ch�Cj�Cl�Cn�Cp�Cr  Cs�fCv  Cx  Cy�fC{�fC~  C�  C�  C�  C��3C�  C��C��C��3C�  C�  C�  C�  C��C�  C��C��3C�  C�  C�  C�  C��C��3C��C��C��3C��3C�  C�  C��3C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��3C�  C�  C��3C�  C�  C�  C��3C��3C��3C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  C��C�  C��3C��3C�  C��C�  C��C�  C��3C�  C�  C��C��C�  C�  C�  C�  C��3C��3C�  C�  C��C��3C��3C�  C��3C�  C��C�  C�  C�  C�  C��3C��3C��3C�  C��C�  C�  C��C��C�  C�  C��C��3C�  C�  C�  C��3C��fC��3D fD � DfD� DfD�fD�D�fD��Dy�DfD� D��D�fDfD�fD�D�fD	fD	�fD
  D
� DfD�fDfDy�D��Dy�D��D� D��Dy�D  D�fD��D� DfDy�D  D�fDfD��D  Dy�D  D� D  D�fD  Dy�D��D� D  D�fDfD� D  Dy�D  Dy�D��D� D  D� D   D � D!fD!�fD"fD"� D#  D#� D$  D$�fD%fD%� D&  D&�fD'fD'� D(fD(�fD)fD)�fD*fD*��D+fD+y�D,  D,� D-fD-�fD.  D.� D/fD/�fD0fD0� D0��D1y�D1��D2� D3fD3� D4  D4�fD5  D5� D6  D6� D6��D7s3D7�3D8� D9  D9� D:  D:�fD;fD;� D;��D<� D=  D=y�D=��D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DCs3DC��DD� DP� DQ  DQ�fDR  DRy�DS  DS�fDTfDT� DU  DU� DU��DVy�DW  DW�fDXfDX�fDYfDY� DZ  DZ� D[  D[�fD\  D\� D]fD]�fD^  D^� D_  D_� D_��D`� Da  Day�Db  Dby�Db��Dc� Dd  Dd� Dd��De� DffDf�fDg  Dgy�Dg��Dh� Di  Di� DjfDj�fDkfDky�Dk��Dl� Dm  Dmy�Dn  Dn� Do  Doy�Dp  Dp�fDp��Dq� DrfDr�fDsfDs�fDtfDt�fDu�Du� Dv  Dv�fDw�Dw�fDw�3Dy�)D�@ D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���A ��A33A?33A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(��B1ffB7  B?��BH33BP33BX33B`33Bh��Bp33Bx33B��B��B��B��B��gB��B��B��gB��gB��B��B��B��B��B��B��B�L�B��B��B��B�L�B�L�B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�3C�3C�C�C�C�C�3C�C&gC&gC &gC"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CA�3CC�3CE�3CG�3CJ�CL�CM�3CO�3CQ�3CS�3CV&gCX&gCZ�C[�3C]ٚC`�Cb�Cc�3Cf�Ch&gCj&gCl&gCn&gCp&gCr�Cs�3Cv�Cx�Cy�3C{�3C~�C�fC�fC�fC���C�fC�3C�3C���C�fC�fC�fC�fC�3C�fC�3C���C�fC�fC�fC�fC�3C���C�3C�3C���C���C�fC�fC���C�fC�fC�fC�fC���C�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�3C���C�fC�fC���C�fC�fC�fC���C���C���C�3C�fC�fC�fC�fC�3C�fC�fC�fC�fC�3C�3C�fC�3C�fC�fC�fC�3C�fC���C���C�fC�3C�fC�3C�fC���C�fC�fC�3C�3C�fC�fC�fC�fC���C���C�fC�fC�3C���C���C�fC���C�fC�3C�fC�fC�fC�fC���C���C���C�fC�3C�fC�fC�3C�  C�fC�fC�3C���C�fC�fC�fC���C���C���D 	�D �3D	�D�3D	�D��D D��D��D|�D	�D�3D��D��D	�D��D D��D		�D	��D
3D
�3D	�D��D	�D|�D��D|�D��D�3D��D|�D3D��D��D�3D	�D|�D3D��D	�D� D3D|�D3D�3D3D��D3D|�D��D�3D3D��D	�D�3D3D|�D3D|�D��D�3D3D�3D 3D �3D!	�D!��D"	�D"�3D#3D#�3D$3D$��D%	�D%�3D&3D&��D'	�D'�3D(	�D(��D)	�D)��D*	�D*� D+	�D+|�D,3D,�3D-	�D-��D.3D.�3D/	�D/��D0	�D0�3D0��D1|�D1��D2�3D3	�D3�3D43D4��D53D5�3D63D6�3D6��D7vfD7�fD8�3D93D9�3D:3D:��D;	�D;�3D;��D<�3D=3D=|�D=��D>�3D?3D?�3D@3D@|�DA3DA�3DB3DB�3DC3DCvfDC��DD�3DP�3DQ3DQ��DR3DR|�DS3DS��DT	�DT�3DU3DU�3DU��DV|�DW3DW��DX	�DX��DY	�DY�3DZ3DZ�3D[3D[��D\3D\�3D]	�D]��D^3D^�3D_3D_�3D_��D`�3Da3Da|�Db3Db|�Db��Dc�3Dd3Dd�3Dd��De�3Df	�Df��Dg3Dg|�Dg��Dh�3Di3Di�3Dj	�Dj��Dk	�Dk|�Dk��Dl�3Dm3Dm|�Dn3Dn�3Do3Do|�Dp3Dp��Dp��Dq�3Dr	�Dr��Ds	�Ds��Dt	�Dt��Du Du�3Dv3Dv��Dw Dw��Dw�fDy�\D�A�D��]1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A� �A� �A� �A� �A��A��A�$�A�(�A�(�A�+A�-A�-A�+A�+A�(�A�+A�-A�ȴAش9A�(�Aװ!A��HA��A�hsA���A�VA�bNA�E�A��A�$�AŁAËDA�A�&�A��A�9XA�33A���A�JA��PA�"�A�A�A���A�r�A���A�"�A��-A�\)A��A��A�JA��wA�t�A�(�A�  A���A�5?A�ffA���A�Q�A��A��;A��A�x�A���A���A�G�A���A�  A�ffA���A�|�A��A�9XA��jA�M�A��A�$�A���A��
A�O�A��hA��^A��A�G�A���A�
=A��A�z�A��A�bA�G�A�ffA}��A|  Ax~�AvjAuK�As\)Aq
=Ao
=Am+Al��Al1AjQ�Ah��Af{AdVAb�A`��A^1AZ��AY�AXI�AVI�AUl�AU%AT�9AT5?AS�PAR��ARVAQ?}AOt�AM�-AJ�RAHjAF$�AA�A=��A<-A:�A:jA:A7�^A4Q�A2��A0��A-�hA,n�A+�7A)�-A'�A'
=A&�+A%�#A#�
A"��A"{A!��A!C�A ��At�Ar�A�PA �A7LA�HA�+AbA��A=qA�;Ap�A%AA�AA�uA(�A��A��AVA�A�A;dA��A�An�A;dAM�A��A
��A	��A	�7A	C�A�HA��A`BA��A�A �A��A7LA�A��A��AdZA �j@���@���@�C�@���@�M�@��@���@�I�@���@� �@�R@�Z@�x�@��@�1@�I�@��
@���@◍@�E�@�^@�hs@��@��D@���@�
=@�O�@��@���@�Q�@��@ՙ�@�Z@�1'@�dZ@�-@Ѳ-@д9@��H@�E�@̼j@��@�&�@�%@��@�C�@�(�@��@���@���@�7L@��j@�9X@��@��@�+@�@�~�@��@�G�@��9@�(�@���@�;d@�
=@��R@�E�@�=q@���@�&�@�z�@�Z@��@�ƨ@���@���@�7L@��@��@���@��u@�j@�  @�t�@���@���@��
@�E�@��@�A�@���@��@���@��@���@�G�@�`B@�z�@�t�@��+@�"�@��@��u@�r�@�1'@�(�@�Q�@�r�@�A�@���@�|�@�\)@��h@�{@�n�@��@��j@�/@�I�@�S�@�l�@�S�@��P@�ƨ@�A�@�Q�@�j@��@��!@��@��@��R@���@�@��y@���@�dZ@�ƨ@���@�  @���@��w@��P@�l�@��@��\@��\@���@���@�V@�@��@�Z@���@��@�"�@��+@��@���@�$�@�=q@���@�ff@�J@��T@�hs@��@�Ĝ@��D@�r�@�I�@�b@��;@���@��w@��@��H@�5?@�@��@���@�hs@�/@���@��@��@��/@�Ĝ@��u@�bN@�ƨ@�"�@��@�z�@���@�O�@�&�@��`@���@�j@�b@��w@�|�@�;d@���@�V@�-@��@���@���@��u@�A�@�l�@�@�ff@�$�@�@�G�@���@�I�@�9X@� �@��@��w@���@���@�l�@�+@���@�n�@�@��#@���@��h@�X@��@�&�@�/@��@��/@���@�r�@�Z@�9X@��;@��w@��@���@��P@�S�@�K�@�+@���@��R@�V@��@���@�hs@�G�@�&�@��@��j@��D@�z�@�r�@�j@� �@��m@�|�@�C�@��@��@��@��R@��@r�!@a|1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A� �A� �A� �A� �A��A��A�$�A�(�A�(�A�+A�-A�-A�+A�+A�(�A�+A�-A�ȴAش9A�(�Aװ!A��HA��A�hsA���A�VA�bNA�E�A��A�$�AŁAËDA�A�&�A��A�9XA�33A���A�JA��PA�"�A�A�A���A�r�A���A�"�A��-A�\)A��A��A�JA��wA�t�A�(�A�  A���A�5?A�ffA���A�Q�A��A��;A��A�x�A���A���A�G�A���A�  A�ffA���A�|�A��A�9XA��jA�M�A��A�$�A���A��
A�O�A��hA��^A��A�G�A���A�
=A��A�z�A��A�bA�G�A�ffA}��A|  Ax~�AvjAuK�As\)Aq
=Ao
=Am+Al��Al1AjQ�Ah��Af{AdVAb�A`��A^1AZ��AY�AXI�AVI�AUl�AU%AT�9AT5?AS�PAR��ARVAQ?}AOt�AM�-AJ�RAHjAF$�AA�A=��A<-A:�A:jA:A7�^A4Q�A2��A0��A-�hA,n�A+�7A)�-A'�A'
=A&�+A%�#A#�
A"��A"{A!��A!C�A ��At�Ar�A�PA �A7LA�HA�+AbA��A=qA�;Ap�A%AA�AA�uA(�A��A��AVA�A�A;dA��A�An�A;dAM�A��A
��A	��A	�7A	C�A�HA��A`BA��A�A �A��A7LA�A��A��AdZA �j@���@���@�C�@���@�M�@��@���@�I�@���@� �@�R@�Z@�x�@��@�1@�I�@��
@���@◍@�E�@�^@�hs@��@��D@���@�
=@�O�@��@���@�Q�@��@ՙ�@�Z@�1'@�dZ@�-@Ѳ-@д9@��H@�E�@̼j@��@�&�@�%@��@�C�@�(�@��@���@���@�7L@��j@�9X@��@��@�+@�@�~�@��@�G�@��9@�(�@���@�;d@�
=@��R@�E�@�=q@���@�&�@�z�@�Z@��@�ƨ@���@���@�7L@��@��@���@��u@�j@�  @�t�@���@���@��
@�E�@��@�A�@���@��@���@��@���@�G�@�`B@�z�@�t�@��+@�"�@��@��u@�r�@�1'@�(�@�Q�@�r�@�A�@���@�|�@�\)@��h@�{@�n�@��@��j@�/@�I�@�S�@�l�@�S�@��P@�ƨ@�A�@�Q�@�j@��@��!@��@��@��R@���@�@��y@���@�dZ@�ƨ@���@�  @���@��w@��P@�l�@��@��\@��\@���@���@�V@�@��@�Z@���@��@�"�@��+@��@���@�$�@�=q@���@�ff@�J@��T@�hs@��@�Ĝ@��D@�r�@�I�@�b@��;@���@��w@��@��H@�5?@�@��@���@�hs@�/@���@��@��@��/@�Ĝ@��u@�bN@�ƨ@�"�@��@�z�@���@�O�@�&�@��`@���@�j@�b@��w@�|�@�;d@���@�V@�-@��@���@���@��u@�A�@�l�@�@�ff@�$�@�@�G�@���@�I�@�9X@� �@��@��w@���@���@�l�@�+@���@�n�@�@��#@���@��h@�X@��@�&�@�/@��@��/@���@�r�@�Z@�9X@��;@��w@��@���@��P@�S�@�K�@�+@���@��R@�V@��@���@�hs@�G�@�&�@��@��j@��D@�z�@�r�@�j@� �@��m@�|�@�C�@��@��@��@��R@��@r�!@a|1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BuBuBuBuBuBuBuBuBuBuBuBuBuBuBuBoBoBDB�B�B�B�fB�B�NB�B�B��BVB�B�B2-BI�BW
B^5BffBq�Bx�Bx�B� B�B�%B�bB�oB��B��B��B�B�B�B�!B�'B�B�B�B��B��B��B��B��B�hB�By�Bs�Bs�Bo�BaHBI�B6FB#�BhBB�B�B��B�dB�9B��B�1BhsBXB33B%�B�B
��B
�B
�B
��B
B
�RB
�'B
��B
y�B
XB
1'B
"�B
VB
  B	��B	�B	�TB	�)B	��B	��B	ĜB	�XB	�B	��B	��B	�oB	�%B	z�B	dZB	\)B	O�B	H�B	J�B	K�B	I�B	F�B	@�B	;dB	6FB	-B	!�B	�B	B��B�B�B��BĜB��B�wB�dB�?B�B�B��B��B��B��B��B��B��B��B��B�{B�{B�{B�{B�uB�uB�{B�uB�uB�{B�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�\B�DB�7B�1B�+B�+B�B�B�B~�By�Br�Bn�BjBn�Bw�Bz�B}�B�B�B�B�B�B�B� B� B{�By�B{�Bz�By�Bx�Bx�Bw�Bv�Bw�Bw�Bt�Br�Bq�Bm�Bm�Bk�BjBjBffBe`BhsBjBm�Bm�Bn�Bn�Bo�Bo�Bo�Bp�Bp�Bs�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bu�Bu�Bu�Bw�By�Bz�B}�B� B�B�7B�PB�uB��B��B��B��B��B�B�?B�3B�-B�-B�3B�FB�RB�jB�wB�}B��BĜBɺB��B��B�B�
B�5B�ZB�B�B��B��B��B��B	B	B	B	
=B	hB	�B	"�B	%�B	(�B	'�B	-B	0!B	33B	5?B	9XB	<jB	=qB	A�B	>wB	A�B	C�B	C�B	H�B	K�B	N�B	P�B	[#B	aHB	cTB	e`B	hsB	l�B	l�B	n�B	o�B	p�B	p�B	p�B	q�B	r�B	u�B	s�B	q�B	r�B	u�B	u�B	s�B	t�B	u�B	x�B	{�B	� B	�B	� B	�%B	�1B	�=B	�DB	�VB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�9B	�FB	�LB	�LB$�B	�B	�B	�B	�B	�)B	�/B	�;B	�HB	�NB	�NB	�ZB	�fB	�fB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
+B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
bB
bB
bB
hB
hB
oB
�B
!-B
/�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222 BuBuBuBuBuBuBuBuBuBuBuBuBuBuBuBoBoBDB�B�B�B�fB�B�NB�B�B��BVB�B�B2-BI�BW
B^5BffBq�Bx�Bx�B� B�B�%B�bB�oB��B��B��B�B�B�B�!B�'B�B�B�B��B��B��B��B��B�hB�By�Bs�Bs�Bo�BaHBI�B6FB#�BhBB�B�B��B�dB�9B��B�1BhsBXB33B%�B�B
��B
�B
�B
��B
B
�RB
�'B
��B
y�B
XB
1'B
"�B
VB
  B	��B	�B	�TB	�)B	��B	��B	ĜB	�XB	�B	��B	��B	�oB	�%B	z�B	dZB	\)B	O�B	H�B	J�B	K�B	I�B	F�B	@�B	;dB	6FB	-B	!�B	�B	B��B�B�B��BĜB��B�wB�dB�?B�B�B��B��B��B��B��B��B��B��B��B�{B�{B�{B�{B�uB�uB�{B�uB�uB�{B�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�\B�DB�7B�1B�+B�+B�B�B�B~�By�Br�Bn�BjBn�Bw�Bz�B}�B�B�B�B�B�B�B� B� B{�By�B{�Bz�By�Bx�Bx�Bw�Bv�Bw�Bw�Bt�Br�Bq�Bm�Bm�Bk�BjBjBffBe`BhsBjBm�Bm�Bn�Bn�Bo�Bo�Bo�Bp�Bp�Bs�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bu�Bu�Bu�Bw�By�Bz�B}�B� B�B�7B�PB�uB��B��B��B��B��B�B�?B�3B�-B�-B�3B�FB�RB�jB�wB�}B��BĜBɺB��B��B�B�
B�5B�ZB�B�B��B��B��B��B	B	B	B	
=B	hB	�B	"�B	%�B	(�B	'�B	-B	0!B	33B	5?B	9XB	<jB	=qB	A�B	>wB	A�B	C�B	C�B	H�B	K�B	N�B	P�B	[#B	aHB	cTB	e`B	hsB	l�B	l�B	n�B	o�B	p�B	p�B	p�B	q�B	r�B	u�B	s�B	q�B	r�B	u�B	u�B	s�B	t�B	u�B	x�B	{�B	� B	�B	� B	�%B	�1B	�=B	�DB	�VB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�9B	�FB	�LB	�LB$�B	�B	�B	�B	�B	�)B	�/B	�;B	�HB	�NB	�NB	�ZB	�fB	�fB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
+B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
bB
bB
bB
hB
hB
oB
�B
!-B
/�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191707                              AO  ARCAADJP                                                                    20181005191707    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191707  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191707  QCF$                G�O�G�O�G�O�8000            