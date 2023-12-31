CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:50Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191750  20181005191750  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d����1   @��eI���@5E�����d{-1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC9�fC;�fC>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT�CV�CX  CZ  C[�fC^  C`  Cb  Cd  Cf�Ch  Cj  Cl�Cn�Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C��3C�  C��C��C��C��C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C��C�  C��3C��C��C�  C��3C��3C��3C��3C��3C��3C��C��C��C��C��C�  C��3C�  C��3C��3C�  C��C�  C��3C��3C��3C��3C��3C�  C��C��C��C��C�  C�  C��C��C�  C��3C��C��C�  C�  C��C��C��C��C�  C�  C�  C��C�  C��C��C��3C�  C��D   D � D  Dy�D��D� D  Dy�D��D� D��Dy�D��D� D  D� DfD� D	  D	�fD
  D
��DfD� D  D� D��D� D  Dy�D  Dy�D  D�fDfD�fD��Dy�D  Dy�D��Dy�D��D� D  D� DfDy�D  D� D��D� D��Dy�D  D� D  Dy�D  Dy�D  Ds3D��D� D��D � D!fD!� D"  D"� D#  D#s3D$  D$� D%  D%� D&  D&�fD'  D'y�D(  D(�fD(��D)� D*  D*�fD+  D+� D,  D,�fD-  D-�fD.  D.� D/  D/� D0  D0� D1  D1y�D1�3D2� D3  D3�fD4  D4y�D4��D5� D6fD6� D7  D7� D7��D8� D8��D9y�D:  D:� D;  D;y�D<  D<� D=fD=y�D>fD>y�D?fD?y�D@  D@y�DAfDA�fDB  DB� DB��DC� DDfDD�fDE  DE�fDE��DF� DG  DG�fDH�DH� DI  DI� DJ  DJ� DKfDK�fDK��DL� DMfDM�fDN  DNy�DN��DO�fDP  DPy�DP��DQ�fDRfDR�fDSfDS� DS��DTy�DT��DU� DVfDV�fDW  DW�fDX  DX� DX��DY� DZfDZ� DZ��D[y�D\fD\� D]fD]y�D^fD^�fD_  D_y�D`fD`�fDa  Da� DbfDb�fDb��Dc� Dd  Dd� Dd��De�fDf  Df�fDgfDgy�Dg��Dh� Dh��Di� Dj  Dj� DkfDky�Dl  Dly�DmfDm� Dm��Dn�fDo  Doy�Do��Dpy�Dq  Dq� DrfDr� DsfDs� Ds��Dt� DufDuy�Dv  Dv�fDwfDwy�Dyj�D�;�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=p@��
A�A!�AA�Aa�A���A���A���A���A���A���A���A���B z�Bz�Bz�Bz�B z�B(z�B0z�B8z�B@{BHz�BPz�BXz�B`z�Bh�GBpz�Bxz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qC �C�C�C8RC�C
�C8RC�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8C:C<C>�C@�CB�CD�CF�CH�CJCL�CN�CP�CR�CT8RCV8RCX�CZ�C\C^�C`�Cb�Cd�Cf8RCh�Cj�Cl8RCn8RCp�Cr�Ct8RCv�Cx�Cz�C|�C~�C�\C�\C�\C�\C�\C��C�\C�\C�\C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C��C�\C�)C�\C��C�\C�)C�\C��C�\C�\C��C�\C�\C��C�\C�\C�\C�\C�\C��C�\C�\C�\C�\C��C�\C��C�\C�)C�)C�)C�)C�\C�\C��C�\C�)C�)C�)C�\C�\C�\C�\C�\C�)C�\C��C�)C�)C�\C��C��C��C��C��C��C�)C�)C�)C�)C�)C�\C��C�\C��C��C�\C�)C�\C��C��C��C��C��C�\C�)C�)C�)C�)C�\C�\C�)C�)C�\C��C�)C�)C�\C�\C�)C�)C�)C�)C�\C�\C�\C�)C�\C�)C�)C��C�\C�)D �D ��D�D�HDHD��D�D�HDHD��DHD�HDHD��D�D��DD��D	�D	�D
�D
�{DD��D�D��DHD��D�D�HD�D�HD�D�DD�DHD�HD�D�HDHD�HDHD��D�D��DD�HD�D��DHD��DHD�HD�D��D�D�HD�D�HD�Dz�DHD��D HD ��D!D!��D"�D"��D#�D#z�D$�D$��D%�D%��D&�D&�D'�D'�HD(�D(�D)HD)��D*�D*�D+�D+��D,�D,�D-�D-�D.�D.��D/�D/��D0�D0��D1�D1�HD1��D2��D3�D3�D4�D4�HD5HD5��D6D6��D7�D7��D8HD8��D9HD9�HD:�D:��D;�D;�HD<�D<��D=D=�HD>D>�HD?D?�HD@�D@�HDADA�DB�DB��DCHDC��DDDD�DE�DE�DFHDF��DG�DG�DH{DH��DI�DI��DJ�DJ��DKDK�DLHDL��DMDM�DN�DN�HDOHDO�DP�DP�HDQHDQ�DRDR�DSDS��DTHDT�HDUHDU��DVDV�DW�DW�DX�DX��DYHDY��DZDZ��D[HD[�HD\D\��D]D]�HD^D^�D_�D_�HD`D`�Da�Da��DbDb�DcHDc��Dd�Dd��DeHDe�Df�Df�DgDg�HDhHDh��DiHDi��Dj�Dj��DkDk�HDl�Dl�HDmDm��DnHDn�Do�Do�HDpHDp�HDq�Dq��DrDr��DsDs��DtHDt��DuDu�HDv�Dv�DwDw�HDyr�D�?�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AÏ\AÑhAÑhAÓuAÓuAÕ�AÕ�A×�AÓuA×�AÙ�AÓuAÑhAÙ�AÛ�Aß�Aá�Aã�Aá�Aá�A�v�A�7LA�A��#A�APA�p�A�dZA�ffA�l�A�n�A�C�A���A���A��A��A�=qA�A�p�A�1'A�oA��#A�"�A���A�1'A�z�A�?}A�bA�ƨA��A��A�ZA���A�=qA��DA���A�jA�
=A�E�A��#A�v�A���A�33A��;A���A��A�ȴA�+A��A�C�A��A�E�A��A�JA�VA�I�A���A���A�E�A�oA�5?A��TA��mA�x�A�ƨA�9XA��
A��A���A�Q�A��A��A���A�?}A�7LA�ZA��hA�JA�S�A��A�r�A��wA��mA���A�O�A��A��A� �A��A�^5A�wA~��A~=qA{�AzE�Ax�`Aw?}Av�!Au��At�Aq&�An5?Am?}Al�yAl1'AkdZAj�jAh �Ag�Afv�AfVAf$�Ae��Ad��Ad  A`�RA[33AYoAW�
AVJAT�AQ�7APffAN��AMhsAMoALA�AK|�AJ�AIx�AH{AGC�AFr�AD�HAC�AC%AB9XA@�A?��A>��A>ZA>bA=�A=\)A<A�A:�`A:M�A9`BA8�A6�\A4�A21A/��A.r�A,��A+G�A*ZA)"�A( �A'�;A'O�A&ffA$z�A#?}A"�`A"��A!�TA!XA ��A ��A A�A�#A33A�uA��A�A�RAl�A��A�A
=A��A��A�AffA�+AK�A9XA�A
��A
(�A	��A	��A	�-A	��A	�A�^AoA�9A�+A��AI�A�A=qA�A;d@�;d@�?}@��@�5?@���@�V@�bN@��
@�C�@�@���@�^5@�@�ƨ@��@�?}@�j@�|�@��@�+@���@ꟾ@��@��/@� �@�33@�7L@�(�@�o@��@�j@�=q@܃@ۥ�@�-@٩�@���@�Q�@�o@�5?@թ�@�G�@Ԭ@���@��;@Ӯ@җ�@�=q@Ѻ^@�1@�V@��/@�ƨ@ʸR@�9X@Ɵ�@���@���@�j@�  @���@ÍP@���@��@��h@�Ĝ@��@��y@��@�hs@���@�t�@�n�@��@��u@�9X@��j@�"�@�O�@�|�@���@��H@���@�l�@��m@�j@�j@��@��R@��T@��D@�bN@�Z@��H@�n�@���@�~�@�&�@���@�V@�V@��@��@�E�@�@�J@���@�9X@� �@��@�b@�  @���@��m@��
@��@�C�@�o@���@���@�^5@�J@��^@�G�@���@���@���@���@��@�I�@���@�ƨ@�|�@�@�v�@��-@�&�@�V@��/@��@�r�@��@��;@��F@�t�@���@�1@�(�@�I�@�r�@���@��P@��P@�o@���@��^@��^@��@��@���@�S�@�dZ@�o@��@��R@���@��@�$�@��@�x�@��@�b@�ƨ@�+@�v�@�=q@�J@���@��-@��h@�O�@�7L@���@��D@�j@�I�@�A�@�A�@�A�@�(�@��@��
@���@�dZ@�K�@��y@��R@���@���@��\@��+@�~�@�M�@�$�@��T@��^@��h@��/@�j@�9X@�9X@�1@��m@��w@��+@��@�J@��-@��h@�p�@�`B@�`B@�X@�7L@��@���@��@���@��@���@��9@��u@�1'@��m@��@��@���@�t�@�;d@��@�o@�
=@�@��@���@���@��+@���@���@�^5@�5?@���@��7@�p�@�G�@��@���@�Ĝ@��D@�j@�9X@�1@��@���@��P@�S�@��@��@��@zz@i��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AÏ\AÑhAÑhAÓuAÓuAÕ�AÕ�A×�AÓuA×�AÙ�AÓuAÑhAÙ�AÛ�Aß�Aá�Aã�Aá�Aá�A�v�A�7LA�A��#A�APA�p�A�dZA�ffA�l�A�n�A�C�A���A���A��A��A�=qA�A�p�A�1'A�oA��#A�"�A���A�1'A�z�A�?}A�bA�ƨA��A��A�ZA���A�=qA��DA���A�jA�
=A�E�A��#A�v�A���A�33A��;A���A��A�ȴA�+A��A�C�A��A�E�A��A�JA�VA�I�A���A���A�E�A�oA�5?A��TA��mA�x�A�ƨA�9XA��
A��A���A�Q�A��A��A���A�?}A�7LA�ZA��hA�JA�S�A��A�r�A��wA��mA���A�O�A��A��A� �A��A�^5A�wA~��A~=qA{�AzE�Ax�`Aw?}Av�!Au��At�Aq&�An5?Am?}Al�yAl1'AkdZAj�jAh �Ag�Afv�AfVAf$�Ae��Ad��Ad  A`�RA[33AYoAW�
AVJAT�AQ�7APffAN��AMhsAMoALA�AK|�AJ�AIx�AH{AGC�AFr�AD�HAC�AC%AB9XA@�A?��A>��A>ZA>bA=�A=\)A<A�A:�`A:M�A9`BA8�A6�\A4�A21A/��A.r�A,��A+G�A*ZA)"�A( �A'�;A'O�A&ffA$z�A#?}A"�`A"��A!�TA!XA ��A ��A A�A�#A33A�uA��A�A�RAl�A��A�A
=A��A��A�AffA�+AK�A9XA�A
��A
(�A	��A	��A	�-A	��A	�A�^AoA�9A�+A��AI�A�A=qA�A;d@�;d@�?}@��@�5?@���@�V@�bN@��
@�C�@�@���@�^5@�@�ƨ@��@�?}@�j@�|�@��@�+@���@ꟾ@��@��/@� �@�33@�7L@�(�@�o@��@�j@�=q@܃@ۥ�@�-@٩�@���@�Q�@�o@�5?@թ�@�G�@Ԭ@���@��;@Ӯ@җ�@�=q@Ѻ^@�1@�V@��/@�ƨ@ʸR@�9X@Ɵ�@���@���@�j@�  @���@ÍP@���@��@��h@�Ĝ@��@��y@��@�hs@���@�t�@�n�@��@��u@�9X@��j@�"�@�O�@�|�@���@��H@���@�l�@��m@�j@�j@��@��R@��T@��D@�bN@�Z@��H@�n�@���@�~�@�&�@���@�V@�V@��@��@�E�@�@�J@���@�9X@� �@��@�b@�  @���@��m@��
@��@�C�@�o@���@���@�^5@�J@��^@�G�@���@���@���@���@��@�I�@���@�ƨ@�|�@�@�v�@��-@�&�@�V@��/@��@�r�@��@��;@��F@�t�@���@�1@�(�@�I�@�r�@���@��P@��P@�o@���@��^@��^@��@��@���@�S�@�dZ@�o@��@��R@���@��@�$�@��@�x�@��@�b@�ƨ@�+@�v�@�=q@�J@���@��-@��h@�O�@�7L@���@��D@�j@�I�@�A�@�A�@�A�@�(�@��@��
@���@�dZ@�K�@��y@��R@���@���@��\@��+@�~�@�M�@�$�@��T@��^@��h@��/@�j@�9X@�9X@�1@��m@��w@��+@��@�J@��-@��h@�p�@�`B@�`B@�X@�7L@��@���@��@���@��@���@��9@��u@�1'@��m@��@��@���@�t�@�;d@��@�o@�
=@�@��@���@���@��+@���@���@�^5@�5?@���@��7@�p�@�G�@��@���@�Ĝ@��D@�j@�9X@�1@��@���@��P@�S�@��@��@��@zz@i��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�yB�yB�yB�yB�yB�yB�yB�B�yB�B�B�mB�fB�B�B�B�B�B��B�B8RBO�BS�BR�BT�BW
BXBYBZB]/B_;BbNBgmBk�BjBiyBhsBe`BcTBiyBz�B�PB��B�9B�wBǮB��B��B�
B�B�/B�5B�)B��B��BƨBĜBÖB��B�}B�dB�'B��B�VB�1B�B� Bx�Bn�BdZB^5BYBT�BM�BB�B5?B �B�BoBVB��B�TB�B��B��BǮBÖB�wB�3B��B�oB�1B� Bx�Bo�BdZBO�B9XB/B)�B�BhB
��B
��B
�RB
�B
��B
��B
�=B
� B
x�B
q�B
l�B
`BB
R�B
H�B
G�B
G�B
B�B
6FB
 �B
hB
JB

=B
1B
B
B	�B	�B	�`B	�ZB	�NB	�/B	��B	��B	�'B	�%B	q�B	ffB	[#B	O�B	<jB	33B	'�B	!�B	�B	�B	�B	,B	+B	#�B	�B	�B	JB	\B	
=B	B��B��B��B��B�B�B�B�B�fB�ZB�BB�)B��BƨB�FB�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�oB�oB�hB�\B�VB�JB�DB�7B�1B�%B�B�B� B|�B{�Bw�Bs�Bq�Bp�Bo�Bo�Bo�Bo�Bn�Bn�Bm�Bl�Bk�Bk�BjBiyBhsBgmBgmBgmBgmBdZBcTBcTBe`BffBgmBhsBhsBgmBhsBhsBhsBgmBgmBjBm�Bm�Bn�Bo�Bo�Bo�Bq�Bs�Bs�Bt�Bs�Bt�Bw�Bw�Bw�Bx�Bw�Bv�Bu�Bw�Bz�B|�B~�B�B�B�B�B�B�B�+B�+B�+B�JB�PB�PB�\B�{B��B��B��B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�3B�9B�9B�FBB�B�B�B�
B��B�B�HB�B�B��B��B��B��B��B��B	B��B	B	+B	JB	bB	hB	oB	uB	{B	�B	�B	�B	"�B	-B	5?B	7LB	8RB	9XB	:^B	<jB	=qB	?}B	D�B	H�B	J�B	K�B	N�B	P�B	S�B	VB	XB	^5B	`BB	aHB	cTB	ffB	k�B	l�B	l�B	o�B	p�B	o�B	n�B	o�B	q�B	q�B	r�B	q�B	q�B	s�B	t�B	y�B	�B	�%B	�+B	�+B	�7B	�hB	�hB	�hB	�hB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�9B	�^B	�jB	�qB	�wB	��B	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�5B	�HB	�ZB	�fB	�mB	�sB	�yB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
�B
�B
.I22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�yB�yB�yB�yB�yB�yB�yB�B�yB�B�B�mB�fB�B�B�B�B�B��B�B8RBO�BS�BR�BT�BW
BXBYBZB]/B_;BbNBgmBk�BjBiyBhsBe`BcTBiyBz�B�PB��B�9B�wBǮB��B��B�
B�B�/B�5B�)B��B��BƨBĜBÖB��B�}B�dB�'B��B�VB�1B�B� Bx�Bn�BdZB^5BYBT�BM�BB�B5?B �B�BoBVB��B�TB�B��B��BǮBÖB�wB�3B��B�oB�1B� Bx�Bo�BdZBO�B9XB/B)�B�BhB
��B
��B
�RB
�B
��B
��B
�=B
� B
x�B
q�B
l�B
`BB
R�B
H�B
G�B
G�B
B�B
6FB
 �B
hB
JB

=B
1B
B
B	�B	�B	�`B	�ZB	�NB	�/B	��B	��B	�'B	�%B	q�B	ffB	[#B	O�B	<jB	33B	'�B	!�B	�B	�B	�B	,B	+B	#�B	�B	�B	JB	\B	
=B	B��B��B��B��B�B�B�B�B�fB�ZB�BB�)B��BƨB�FB�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�oB�oB�hB�\B�VB�JB�DB�7B�1B�%B�B�B� B|�B{�Bw�Bs�Bq�Bp�Bo�Bo�Bo�Bo�Bn�Bn�Bm�Bl�Bk�Bk�BjBiyBhsBgmBgmBgmBgmBdZBcTBcTBe`BffBgmBhsBhsBgmBhsBhsBhsBgmBgmBjBm�Bm�Bn�Bo�Bo�Bo�Bq�Bs�Bs�Bt�Bs�Bt�Bw�Bw�Bw�Bx�Bw�Bv�Bu�Bw�Bz�B|�B~�B�B�B�B�B�B�B�+B�+B�+B�JB�PB�PB�\B�{B��B��B��B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�3B�9B�9B�FBB�B�B�B�
B��B�B�HB�B�B��B��B��B��B��B��B	B��B	B	+B	JB	bB	hB	oB	uB	{B	�B	�B	�B	"�B	-B	5?B	7LB	8RB	9XB	:^B	<jB	=qB	?}B	D�B	H�B	J�B	K�B	N�B	P�B	S�B	VB	XB	^5B	`BB	aHB	cTB	ffB	k�B	l�B	l�B	o�B	p�B	o�B	n�B	o�B	q�B	q�B	r�B	q�B	q�B	s�B	t�B	y�B	�B	�%B	�+B	�+B	�7B	�hB	�hB	�hB	�hB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�9B	�^B	�jB	�qB	�wB	��B	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�5B	�HB	�ZB	�fB	�mB	�sB	�yB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
�B
�B
.I22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.12 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191750                              AO  ARCAADJP                                                                    20181005191750    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191750  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191750  QCF$                G�O�G�O�G�O�8000            