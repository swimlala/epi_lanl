CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:13Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190613  20181005190613  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              (A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�i���R1   @�j33H@0�C��%�c333331   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     (A   A   A   @�  @�  A   A   A@  A`  A�  A���A���A�  A�  A�  A�  A�  B   BffBffBffB ffB(  B0  B8  B@  BG��BO��BW��B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C��3C��3C��3C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D�fD	fD	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� DfD� D��D� D  D� D  D� D��Dy�D  Dy�D  D�fD  Dy�D  D� D  D� D  D� D  D�fDfD� D��Dy�D  D� D   D � D ��D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(fD(�fD)  D)� D*fD*� D*��D+� D,  D,y�D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5� D6  D6� D7fD7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>y�D>��D?� D@  D@� D@��DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DK��DLy�DM  DMy�DN  DN� DO  DO�fDPfDP� DQ  DQy�DR  DR� DS  DS� DTfDT�fDU  DUy�DU��DV� DWfDW� DX  DX�fDY  DYy�DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^�fD_  D_y�D_��D`� DafDa� Db  Db� Dc  Dcy�Dd  Dd� De  De�fDf  Df� Dg  Dgy�Dh  Dh� Dh��Di� DjfDj� Dj��Dk� DlfDl� Dm  Dm� DnfDn�fDo  Do� Dp  Dpy�Dp��Dqy�Dr  Dry�Dr��Dsy�Ds��Dty�Dt��Du� Du��Dvy�Dw  Dwy�Dx  DxL�D�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�\)A�A#�AC�Ac�A��
A���A���A��
A��
A��
A��
A��
B �B	Q�BQ�BQ�B!Q�B(�B0�B8�B@�BH�BP�BX�B`�BiQ�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�B�B�u�B���B�u�B�u�B�u�B�u�B�u�B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C :�C:�C:�C:�C!GC
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJT{CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb!GCd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�*>C�qC�qC��C��C��C�qC�qC��C�qC�qC�qC�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�*>C�qC��C�qC�*>C�qC�qC�qC�qC�qC�qC�qC��C��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�*>C�*>C�*>C�qC�qC�qC�qC�qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D�RD�D�D	D	��D
�D
��DRD��D�D��D�D��D�D��D�D��D�D��DD��DRD��D�D��D�D��DRD�RD�D�RD�D�D�D�RD�D��D�D��D�D��D�D�DD��DRD�RD�D��D �D ��D!RD!��D"�D"��D#�D#��D$D$��D%�D%��D&�D&��D'�D'��D(D(�D)�D)��D*D*��D+RD+��D,�D,�RD-�D-��D.RD.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5RD5��D6�D6��D7D7��D8�D8��D9�D9�D:�D:��D;�D;��D<�D<��D=�D=��D>�D>�RD?RD?��D@�D@��DARDA��DBDB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DLRDL�RDM�DM�RDN�DN��DO�DO�DPDP��DQ�DQ�RDR�DR��DS�DS��DTDT�DU�DU�RDVRDV��DWDW��DX�DX�DY�DY�RDZ�DZ��D[RD[��D\�D\��D]�D]��D^�D^�D_�D_�RD`RD`��DaDa��Db�Db��Dc�Dc�RDd�Dd��De�De�Df�Df��Dg�Dg�RDh�Dh��DiRDi��DjDj��DkRDk��DlDl��Dm�Dm��DnDn�Do�Do��Dp�Dp�RDqRDq�RDr�Dr�RDsRDs�RDtRDt�RDuRDu��DvRDv�RDw�Dw�RDx�Dx[�D�H 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yA��A��A��A��A���A��A��A��A��A���A��yA���A̴9A̮A̟�A̟�A̛�A̕�Ȁ\Ả7A�l�A���A�ffAʾwA�p�AȲ-A���A��A�|�A�5?A�dZA���Aŕ�A�r�A�(�A�hsA���A�dZA�I�A�$�A�K�A���A��A���A�ZA�9XA��A��wA��A�n�A�9XA��uA�JA�r�A��hA�|�A�O�A��A�t�A�/A��;A�E�A��A�
=A�;dA�5?A���A�^5A�;dA��A��jA���A�A�A��RA���A��yA���A�S�A��A�A� �A���A��A�|�A��A���A�ȴA�r�A�\)A�A��A���A�l�A�A�bNA�-A�C�A�~�A���A�|�A��A�1A�{A��9A�v�A��!A}p�A{��Ay��Av�AsK�Ap^5An�AlVAhbNAe�hA_�AX��AS"�AO�AMAH��AEC�ACK�AA��A@��A?7LA=�TA;;dA9��A9�A9�A8�`A6�\A4��A3��A2ȴA1��A/�A.1A,��A+oA(�A(9XA&��A%��A$�HA#��A"�A!�A�A�+A�uA/A�AƨA$�A7LA1'A�A��A�FAl�A&�A��A�#A�7A�PA�PA`BA�AffA�Ax�AK�A/A��A�AVA
��A
-A	�;A	��A�/A�A��A��A1'A��A?}A��AĜA~�A��A��A��A�mAoA�+AZAVA1'A{A��A I�@���@��y@��@�V@�?}@�^5A A�A M�@��;@�-@�V@��u@�33@�Q�@�A�@��@@�9X@��@�  @��H@�ff@�x�@ߝ�@���@�?}@��@ڸR@���@���@�$�@�ȴ@�@ۍP@ۮ@�@�o@��@�r�@�R@��@�o@��@���@㝲@�K�@�
=@�-@��u@�o@��/@�|�@��@�p�@ٲ-@ڧ�@���@� �@�5?@��@�p�@�J@Ձ@Ձ@��@ڇ+@�r�@�Z@�|�@�t�@�  @�  @�  @�ƨ@�|�@�\)@�V@�`B@���@�I�@��@���@׍P@֟�@ԓu@�9X@�l�@ҏ\@�v�@�E�@�$�@�x�@Ь@�1'@϶F@ύP@ύP@���@��;@�o@Ώ\@���@͡�@��@���@̼j@�1'@�n�@��@�(�@�1'@��
@�dZ@��@��@�o@Ɨ�@�5?@�-@Ł@���@���@Ĵ9@�1@�ƨ@�C�@��@��@��#@�@��7@���@��
@���@�C�@�
=@���@��y@�ȴ@�{@��@���@��j@�bN@�I�@�(�@�  @��w@�K�@�@��@���@�-@���@��7@�`B@�G�@�&�@�Ĝ@���@��^@�G�@�G�@�G�@��9@�j@�A�@��@��@��@���@��H@�G�@�r�@�(�@���@���@�t�@�"�@���@�$�@��7@���@��@���@�|�@���@���@���@�ff@�^5@�M�@�S�@�ƨ@���@�S�@�
=@��\@�~�@�=q@��@�hs@���@��`@��u@� �@��F@���@��P@�+@���@���@���@��@�x�@�7L@�V@��j@��u@�Q�@�I�@�1'@�|�@�J@�V@��u@�Q�@��
@��w@��@�l�@��y@��!@�5?@�@�x�@�X@�/@��@�%@��/@��9@�Q�@�(�@���@��
@���@�ƨ@��@��P@�;d@�+@�S�@��P@���@�S�@�@��@�\)@�|�@�S�@�"�@��\@�V@�=q@��@��@��@��-@��7@�/@���@��@��@�  @��
@���@�;d@�
=@���@�G�@���@���@�Q�@�b@�|�@�+@��@���@�J@�;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��yA��A��A��A��A���A��A��A��A��A���A��yA���A̴9A̮A̟�A̟�A̛�A̕�Ȁ\Ả7A�l�A���A�ffAʾwA�p�AȲ-A���A��A�|�A�5?A�dZA���Aŕ�A�r�A�(�A�hsA���A�dZA�I�A�$�A�K�A���A��A���A�ZA�9XA��A��wA��A�n�A�9XA��uA�JA�r�A��hA�|�A�O�A��A�t�A�/A��;A�E�A��A�
=A�;dA�5?A���A�^5A�;dA��A��jA���A�A�A��RA���A��yA���A�S�A��A�A� �A���A��A�|�A��A���A�ȴA�r�A�\)A�A��A���A�l�A�A�bNA�-A�C�A�~�A���A�|�A��A�1A�{A��9A�v�A��!A}p�A{��Ay��Av�AsK�Ap^5An�AlVAhbNAe�hA_�AX��AS"�AO�AMAH��AEC�ACK�AA��A@��A?7LA=�TA;;dA9��A9�A9�A8�`A6�\A4��A3��A2ȴA1��A/�A.1A,��A+oA(�A(9XA&��A%��A$�HA#��A"�A!�A�A�+A�uA/A�AƨA$�A7LA1'A�A��A�FAl�A&�A��A�#A�7A�PA�PA`BA�AffA�Ax�AK�A/A��A�AVA
��A
-A	�;A	��A�/A�A��A��A1'A��A?}A��AĜA~�A��A��A��A�mAoA�+AZAVA1'A{A��A I�@���@��y@��@�V@�?}@�^5A A�A M�@��;@�-@�V@��u@�33@�Q�@�A�@��@@�9X@��@�  @��H@�ff@�x�@ߝ�@���@�?}@��@ڸR@���@���@�$�@�ȴ@�@ۍP@ۮ@�@�o@��@�r�@�R@��@�o@��@���@㝲@�K�@�
=@�-@��u@�o@��/@�|�@��@�p�@ٲ-@ڧ�@���@� �@�5?@��@�p�@�J@Ձ@Ձ@��@ڇ+@�r�@�Z@�|�@�t�@�  @�  @�  @�ƨ@�|�@�\)@�V@�`B@���@�I�@��@���@׍P@֟�@ԓu@�9X@�l�@ҏ\@�v�@�E�@�$�@�x�@Ь@�1'@϶F@ύP@ύP@���@��;@�o@Ώ\@���@͡�@��@���@̼j@�1'@�n�@��@�(�@�1'@��
@�dZ@��@��@�o@Ɨ�@�5?@�-@Ł@���@���@Ĵ9@�1@�ƨ@�C�@��@��@��#@�@��7@���@��
@���@�C�@�
=@���@��y@�ȴ@�{@��@���@��j@�bN@�I�@�(�@�  @��w@�K�@�@��@���@�-@���@��7@�`B@�G�@�&�@�Ĝ@���@��^@�G�@�G�@�G�@��9@�j@�A�@��@��@��@���@��H@�G�@�r�@�(�@���@���@�t�@�"�@���@�$�@��7@���@��@���@�|�@���@���@���@�ff@�^5@�M�@�S�@�ƨ@���@�S�@�
=@��\@�~�@�=q@��@�hs@���@��`@��u@� �@��F@���@��P@�+@���@���@���@��@�x�@�7L@�V@��j@��u@�Q�@�I�@�1'@�|�@�J@�V@��u@�Q�@��
@��w@��@�l�@��y@��!@�5?@�@�x�@�X@�/@��@�%@��/@��9@�Q�@�(�@���@��
@���@�ƨ@��@��P@�;d@�+@�S�@��P@���@�S�@�@��@�\)@�|�@�S�@�"�@��\@�V@�=q@��@��@��@��-@��7@�/@���@��@��@�  @��
@���@�;d@�
=@���@�G�@���@���@�Q�@�b@�|�@�+@��@���@�J@�;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
-B
33B
=qB
p�B
�dB
�#B
��B+B#�B/B=qBD�BG�BE�BQ�B[#BW
Bm�B�B~�B� B�hB��B��B��B�B�-B�jBǮB��B�BB�B��B+B1BhBhB�B�B�B�B"�B$�B(�B0!B6FB6FB+B2-B<jB33B%�B1'B:^B,B�B�B$�BoB��B�BB��BB�LB�B��B�Bt�B`BBD�B5?B&�B#�B!�BoB	7BB
�B
ĜB
�\B
s�B
^5B
/B
�B
1B	�TB	�#B	�B	�}B	��B	�%B	t�B	iyB	R�B	=qB	�B�B�5B��B��B�qB�9B�'B�B�B��B��B��B��B��B��B�-B�B��B�B�B�B�3B�?B�FB�LB�dB�jB��BŢBȴB��B��BɺBȴBƨBȴBǮBǮBĜBB��B��B��B��BĜBƨBȴB��B��B��B��B��B�B�B�#B�#B�)B�5B�NB�ZB�sB�fB�fB�sB�sB�sB�B��B��B��B��B��B��B	B	B	B	B	+B	DB		7B	+B	B	1B	VB	�B	�B	�B	bB	PB	DB	DB	hB	hB	!�B	7LB	@�B	G�B	E�B	C�B	A�B	<jB	33B	(�B	�B	�B	JB	B��B�B�B�B�mB�fB�fB�sB�B�B�B��B��B��B	+B	DB	1B	hB	@�B	\)B	k�B	k�B	l�B	jB	gmB	l�B	l�B	k�B	jB	e`B	`BB	YB	S�B	M�B	L�B	T�B	bNB	`BB	[#B	XB	XB	[#B	bNB	cTB	gmB	t�B	�VB	��B	��B	��B	��B	��B	�B	�'B	�-B	�?B	�LB	�qB	�wB	�}B	��B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	�}B	��B	�}B	�}B	��B	B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�#B	�)B	�#B	�B	�#B	�;B	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
	7B
PB
PB
\B
hB
oB
oB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
-B
33B
=qB
p�B
�dB
�#B
��B+B#�B/B=qBD�BG�BE�BQ�B[#BW
Bm�B�B~�B� B�hB��B��B��B�B�-B�jBǮB��B�BB�B��B+B1BhBhB�B�B�B�B"�B$�B(�B0!B6FB6FB+B2-B<jB33B%�B1'B:^B,B�B�B$�BoB��B�BB��BB�LB�B��B�Bt�B`BBD�B5?B&�B#�B!�BoB	7BB
�B
ĜB
�\B
s�B
^5B
/B
�B
1B	�TB	�#B	�B	�}B	��B	�%B	t�B	iyB	R�B	=qB	�B�B�5B��B��B�qB�9B�'B�B�B��B��B��B��B��B��B�-B�B��B�B�B�B�3B�?B�FB�LB�dB�jB��BŢBȴB��B��BɺBȴBƨBȴBǮBǮBĜBB��B��B��B��BĜBƨBȴB��B��B��B��B��B�B�B�#B�#B�)B�5B�NB�ZB�sB�fB�fB�sB�sB�sB�B��B��B��B��B��B��B	B	B	B	B	+B	DB		7B	+B	B	1B	VB	�B	�B	�B	bB	PB	DB	DB	hB	hB	!�B	7LB	@�B	G�B	E�B	C�B	A�B	<jB	33B	(�B	�B	�B	JB	B��B�B�B�B�mB�fB�fB�sB�B�B�B��B��B��B	+B	DB	1B	hB	@�B	\)B	k�B	k�B	l�B	jB	gmB	l�B	l�B	k�B	jB	e`B	`BB	YB	S�B	M�B	L�B	T�B	bNB	`BB	[#B	XB	XB	[#B	bNB	cTB	gmB	t�B	�VB	��B	��B	��B	��B	��B	�B	�'B	�-B	�?B	�LB	�qB	�wB	�}B	��B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	�}B	��B	�}B	�}B	��B	B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�#B	�)B	�#B	�B	�#B	�;B	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
	7B
PB
PB
\B
hB
oB
oB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190613                              AO  ARCAADJP                                                                    20181005190613    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190613  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190613  QCF$                G�O�G�O�G�O�8000            