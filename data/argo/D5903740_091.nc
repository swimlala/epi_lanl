CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-09-14T07:01:18Z AOML 3.0 creation; 2016-06-01T00:08:20Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140914070118  20160531170820  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               [A   AO  4055_7112_091                   2C  D   APEX                            5374                            041511                          846 @����1   @��}� @9��`A�7�d#��S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    [A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B  B  B  B   B(  B0  B8  B@  BHffBN��BX  B`  Bh  Bp  By33B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dy�fD���D�c3D�y�D���D� D�C3D�y�D��3D�  D�C3D���D��fD���D�@ D�vfD��fD�fD�6fD�vfD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@���AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�  A�  B��B	��B��B��B!��B)��B1��B9��BA��BJ  BPfgBY��Ba��Bi��Bq��Bz��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C ffCffCffCffCffC
ffCffCffCffCffCffCffCffCffCffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3� D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtl�Dy� D���D�p D��gD��D��D�P D��gD�� D��D�P D��gD��3D�gD�L�Dڃ3D��3D�3D�C3D�3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��#A��#A��A�DAߝ�A�dZA���A޼jAޙ�A�`BA�1Aݩ�A�dZA�bAܙ�A�/A���A۬AڮA�%A�A�r�A�1'AփA���A�^5Aϟ�A��A�A���A��A��A��+A�Q�A��A��/A�bA�^5A�hsA��A���A��!A�&�A��!A�XA�C�A�ƨA�S�A���A�ZA��TA���A�`BA�ZA�I�A���A�?}A���A�x�A��A��wA�ȴA���A���A�Q�A��-A�7LA��`A��A�JA�v�A��^A�=qA�~�A��A��mA���A�Q�A�1'A��A���A�`BA���A��A�-A�ƨA�p�A��A�O�A���A��A��7A�C�A�|�A�+A���A��RA�dZA��A�O�A��A�ffA�JA�E�A33A~z�A~5?A~�A}l�A{�Ayx�AxffAv  At�Aq�AoAm��AmhsAmC�Akp�Ag�TAeK�AdjAc��Ac��Ac��Ab��Ab1'A`�A^�+A\��A[�#A[AY�AX$�AW"�AV�AV�jAV�AUp�AT  AR��AQ|�AP�HAOANr�AMt�AK�TAIt�AH�AG33AFjAE��AC|�AA+A??}A>n�A<�A;��A;+A:�yA:bA8ZA7��A7VA6��A6�A6�+A5C�A4~�A3�TA3��A2�A2�A0��A01'A/��A.~�A-
=A+��A+"�A*��A*�A)
=A(�9A(�uA(n�A(JA'oA&�+A&  A$�RA#��A#33A"��A!|�A {A�A33A�Ap�A�A=qA�A  A�^AVA�AVA�AAZA+A^5A�A��AVA�TA��A�PA�A��A
bNA	dZA��A��Ap�A�Ar�A��A�A��A?}A�uA ��@��@�J@�O�@��@��R@���@��@���@�"�@��#@��@��@�/@���@��
@�K�@���@�@�r�@��@��@�@���@�K�@ޏ\@�1@ۅ@ڏ\@�{@١�@�7L@�Ĝ@�dZ@�S�@��T@ЋD@� �@��;@�n�@͡�@̴9@�b@˥�@�l�@�C�@�33@�o@��y@ʏ\@ə�@�/@ȓu@Ǿw@���@�J@�@��`@��@�v�@��@���@�Ĝ@��+@��7@�7L@���@� �@�|�@���@�-@�?}@��/@�bN@��F@�\)@��@�ff@���@���@�O�@��/@�z�@�1@��w@�;d@���@��y@�ȴ@���@�=q@���@�x�@��m@�^5@��#@��^@�x�@���@��@��@�Ĝ@�Q�@�t�@��@���@��-@�?}@�&�@�S�@��y@��!@��@�7L@���@�Z@��;@�K�@��!@�~�@�$�@���@��@��@�Q�@��@���@�t�@�E�@��`@��@�bN@���@�|�@�o@���@�ff@�E�@��@��T@�X@���@��/@� �@��
@���@�l�@�dZ@�dZ@�\)@�K�@�"�@���@�ff@��#@���@�x�@�O�@�V@���@���@�j@�Q�@�A�@�(�@� �@�b@�  @��m@��w@�\)@���@�v�@�@�x�@�p�@�O�@�V@���@��/@�z�@�  @��m@��w@���@��@�S�@�o@���@��\@�^5@�-@���@��7@��7@��@�`B@���@��/@��/@�Ĝ@��D@�r�@�Q�@�  @�ƨ@�K�@�
=@���@�v�@�n�@�M�@�=q@�5?@�J@��T@��-@��@�G�@��@�%@���@��9@�Z@�@~ff@}�T@}/@|��@|j@|9X@|I�@{�m@{�@{o@zn�@y�^@yx�@x��@x�@xr�@xA�@xb@w+@v��@vȴ@vv�@vE�@u�h@tz�@tj@tj@t1@qG�@g\)@`�9@Wl�@P1'@JM�@E�@A��@=�h@9��@3�@-@)�@%O�@�@��@Z@%@O�@
M�@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A��#A��#A��A�DAߝ�A�dZA���A޼jAޙ�A�`BA�1Aݩ�A�dZA�bAܙ�A�/A���A۬AڮA�%A�A�r�A�1'AփA���A�^5Aϟ�A��A�A���A��A��A��+A�Q�A��A��/A�bA�^5A�hsA��A���A��!A�&�A��!A�XA�C�A�ƨA�S�A���A�ZA��TA���A�`BA�ZA�I�A���A�?}A���A�x�A��A��wA�ȴA���A���A�Q�A��-A�7LA��`A��A�JA�v�A��^A�=qA�~�A��A��mA���A�Q�A�1'A��A���A�`BA���A��A�-A�ƨA�p�A��A�O�A���A��A��7A�C�A�|�A�+A���A��RA�dZA��A�O�A��A�ffA�JA�E�A33A~z�A~5?A~�A}l�A{�Ayx�AxffAv  At�Aq�AoAm��AmhsAmC�Akp�Ag�TAeK�AdjAc��Ac��Ac��Ab��Ab1'A`�A^�+A\��A[�#A[AY�AX$�AW"�AV�AV�jAV�AUp�AT  AR��AQ|�AP�HAOANr�AMt�AK�TAIt�AH�AG33AFjAE��AC|�AA+A??}A>n�A<�A;��A;+A:�yA:bA8ZA7��A7VA6��A6�A6�+A5C�A4~�A3�TA3��A2�A2�A0��A01'A/��A.~�A-
=A+��A+"�A*��A*�A)
=A(�9A(�uA(n�A(JA'oA&�+A&  A$�RA#��A#33A"��A!|�A {A�A33A�Ap�A�A=qA�A  A�^AVA�AVA�AAZA+A^5A�A��AVA�TA��A�PA�A��A
bNA	dZA��A��Ap�A�Ar�A��A�A��A?}A�uA ��@��@�J@�O�@��@��R@���@��@���@�"�@��#@��@��@�/@���@��
@�K�@���@�@�r�@��@��@�@���@�K�@ޏ\@�1@ۅ@ڏ\@�{@١�@�7L@�Ĝ@�dZ@�S�@��T@ЋD@� �@��;@�n�@͡�@̴9@�b@˥�@�l�@�C�@�33@�o@��y@ʏ\@ə�@�/@ȓu@Ǿw@���@�J@�@��`@��@�v�@��@���@�Ĝ@��+@��7@�7L@���@� �@�|�@���@�-@�?}@��/@�bN@��F@�\)@��@�ff@���@���@�O�@��/@�z�@�1@��w@�;d@���@��y@�ȴ@���@�=q@���@�x�@��m@�^5@��#@��^@�x�@���@��@��@�Ĝ@�Q�@�t�@��@���@��-@�?}@�&�@�S�@��y@��!@��@�7L@���@�Z@��;@�K�@��!@�~�@�$�@���@��@��@�Q�@��@���@�t�@�E�@��`@��@�bN@���@�|�@�o@���@�ff@�E�@��@��T@�X@���@��/@� �@��
@���@�l�@�dZ@�dZ@�\)@�K�@�"�@���@�ff@��#@���@�x�@�O�@�V@���@���@�j@�Q�@�A�@�(�@� �@�b@�  @��m@��w@�\)@���@�v�@�@�x�@�p�@�O�@�V@���@��/@�z�@�  @��m@��w@���@��@�S�@�o@���@��\@�^5@�-@���@��7@��7@��@�`B@���@��/@��/@�Ĝ@��D@�r�@�Q�@�  @�ƨ@�K�@�
=@���@�v�@�n�@�M�@�=q@�5?@�J@��T@��-@��@�G�@��@�%@���@��9@�Z@�@~ff@}�T@}/@|��@|j@|9X@|I�@{�m@{�@{o@zn�@y�^@yx�@x��@x�@xr�@xA�@xb@w+@v��@vȴ@vv�@vE�@u�h@tz�@tj@tj@t1@qG�@g\)@`�9@Wl�@P1'@JM�@E�@A��@=�h@9��@3�@-@)�@%O�@�@��@Z@%@O�@
M�@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBB��B��BɺB��BɺBȴB��B�B�)B�/B�HB�B�B�B�B�B�mB�`B�HB�BB�B  B��B��B��B�mB�^B��B�JBz�BiyB]/BK�B>wB7LB-B�B�B�BbB	7B��B�B�B�fB�5B�B��B��B��B��B��BŢB�qB�FB�3B��B��B�7Bx�Bm�BbNBXBN�BG�B7LB&�B�BVBB��B�yB�BǮB��B�wB�^B�FB�'B��B��B�VB�+B~�Br�BdZBK�B2-BJB
��B
�mB
�;B
��B
ÖB
�wB
�RB
��B
��B
��B
�uB
�B
y�B
r�B
p�B
n�B
gmB
ZB
F�B
;dB
&�B
�B
B	�B	�TB	�BB	�)B	��B	�XB	�B	��B	��B	��B	��B	��B	��B	�\B	�B	x�B	r�B	m�B	gmB	_;B	[#B	ZB	XB	T�B	P�B	I�B	D�B	?}B	;dB	6FB	/B	)�B	!�B	�B	uB	\B	JB	+B	  B��B��B�B�B�B�B�yB�fB�HB�;B�/B�#B�#B�B��B��B��B��B��BƨBÖB��B�}B�^B�?B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B�uB�hB�PB�1B�B~�B{�By�Bw�Bv�Bu�Bt�Bs�Bq�Bn�BjBgmBdZBaHB_;B]/B[#BYBW
BS�BO�BM�BL�BI�BF�BE�BB�B>wB<jB;dB7LB33B1'B0!B/B,B)�B)�B(�B'�B&�B%�B$�B#�B"�B!�B!�B �B!�B!�B!�B!�B!�B!�B �B�B�B�B �B �B!�B!�B%�B%�B&�B&�B&�B&�B%�B%�B+B-B/B/B.B0!B0!B1'B1'B2-B2-B2-B2-B1'B1'B1'B2-B2-B2-B33B33B1'B49B6FB6FB6FB6FB5?B6FB9XB;dB;dB;dB<jB=qB=qB>wB@�BA�BA�BC�BD�BD�BF�BI�BI�BJ�BL�BM�BN�BO�BQ�BR�BR�BS�BS�BT�BVBW
B]/BdZBffBgmBhsBjBjBjBjBl�Bo�Bx�By�By�B|�B{�B�%B�1B�1B�JB�hB�oB��B��B��B��B��B��B��B��B�B�B�3B�3B�3B�^BŢBȴBɺB��B��B��B�
B�#B�#B�/B�5B�TB�mB�mB�B�B�B�B��B��B��B��B��B��B��B	  B	B	B	%B	1B	
=B	PB	\B	bB	hB	hB	oB	oB	uB	uB	�B	�B	�B	 �B	'�B	)�B	)�B	+B	.B	.B	/B	2-B	7LB	8RB	9XB	:^B	;dB	<jB	>wB	@�B	C�B	D�B	F�B	H�B	K�B	K�B	K�B	L�B	O�B	P�B	P�B	Q�B	R�B	S�B	T�B	XB	YB	[#B	^5B	`BB	bNB	cTB	dZB	e`B	e`B	ffB	gmB	jB	k�B	m�B	n�B	n�B	n�B	p�B	q�B	s�B	x�B	y�B	{�B	{�B	}�B	}�B	~�B	� B	�B	�B	�B	�%B	�DB	�JB	�PB	�PB	�PB	�PB	�bB	�hB	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	B	�BB	��B
B
hB
�B
!�B
(�B
/B
8RB
@�B
E�B
K�B
S�B
ZB
bNB
e`B
jB
o�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BBB B�B��B̶BɢB˰BɠBȜB��B��B�B�B�1B�pB�B�B�B�B�UB�KB�1B�-B�sB��B��B��B��B�UB�CB��B�)Bz�BiZB]BK�B>UB7.B,�B�BxBbB?B	B��B�B�hB�BB�B��B��B��BηBͮB̨B�}B�MB�"B�B��B�kB�Bx�BmkBb&BW�BN�BG�B7$B&�B�B,B�B��B�SB��BǉB�dB�QB�8B�B�B��B�~B�0B�B~�Br�Bd3BK�B2	B$B
��B
�IB
�B
��B
�rB
�SB
�.B
��B
��B
��B
�PB
��B
y�B
r�B
p~B
ntB
gIB
Y�B
F�B
;AB
&�B
kB
�B	�B	�4B	�"B	�B	ͲB	�9B	��B	��B	��B	��B	��B	��B	�sB	�>B	��B	x�B	r�B	mrB	gOB	_B	[B	ZB	W�B	T�B	P�B	I�B	D�B	?^B	;FB	6)B	.�B	)�B	!�B	B	ZB	?B	,B	B��B��B��B�B�{B�pB�lB�]B�LB�-B�B�B�
B�	B��B��B��B��BͷBʧBƍB�}B�oB�cB�DB�&B�B� B��B��B��B��B��B��B��B��B��B��B�{B�nB�[B�PB�8B�B�B~�B{�By�Bw�Bv�Bu�Bt�Bs�Bq�Bn�BjhBgWBdBBa3B_"B]B[BX�BV�BS�BO�BM�BL�BI�BF�BE�BBxB>bB<TB;MB76B3B1B0B/B+�B)�B)�B(�B'�B&�B%�B$�B#�B"�B!�B!�B �B!�B!�B!�B!�B!�B!�B �B�B�B�B �B �B!�B!�B%�B%�B&�B&�B&�B&�B%�B%�B*�B,�B/B/B-�B0B0B1B1B2B2B2B2B1B1B1B2B2B2B3B3B1B4B6,B6+B6*B6(B5%B6*B9=B;IB;HB;HB<OB=VB=XB>[B@fBAkBAmBCxBD�BD�BF�BI�BI�BJ�BL�BM�BN�BO�BQ�BR�BR�BS�BS�BT�BU�BV�B]Bd8BfHBgOBhUBjaBj_Bj`BjaBljBoBx�By�By�B|�B{�B�B�B�B�*B�IB�PB�`B�lB��B��B��B��B��B��B��B��B�B�B�B�:B�}BȐBɗBͰB��B��B��B� B��B�B�B�.B�FB�JB�lB�~B�B�B��B��B��B��B��B��B��B��B	�B	�B	�B	B	
B	*B	5B	9B	CB	AB	HB	IB	PB	PB	[B	tB	�B	 �B	'�B	)�B	)�B	*�B	-�B	-�B	.�B	2B	7#B	8)B	9/B	:7B	;<B	<@B	>MB	@[B	CnB	DuB	F�B	H�B	K�B	K�B	K�B	L�B	O�B	P�B	P�B	Q�B	R�B	S�B	T�B	W�B	X�B	Z�B	^
B	`B	b%B	c)B	d2B	e6B	e5B	f:B	gBB	jVB	kZB	mhB	nnB	nnB	noB	pwB	q�B	s�B	x�B	y�B	{�B	{�B	}�B	}�B	~�B	�B	��B	��B	��B	��B	�B	�B	�'B	�'B	�$B	�$B	�7B	�:B	�:B	�JB	�JB	�[B	�zB	�wB	�yB	��B	��B	�bB	�B	�B
�B
9B
pB
!�B
(�B
.�B
8 B
@UB
EsB
K�B
S�B
Y�B
bB
e2B
jNB
olB
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.4 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708202016053117082020160531170820  AO  ARCAADJP                                                                    20140914070118    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140914070118  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140914070118  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170820  IP                  G�O�G�O�G�O�                