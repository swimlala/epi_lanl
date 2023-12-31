CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:00Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               !A   AO  20111130135813  20190522121825  1727_5046_033                   2C  D   APEX                            2143                            040306                          846 @�@X��@1   @�@Y}'�@7v�t��c���v�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B'��B/��B7��B@  BHffBP  BX  B`  Bh  Bo��Bw��B�  B�  B�  B�33B�  B���B�  B�  B�  B���B���B�  B�  B���B�  B�33B�33B�33B�  B�33B�  B���B�  B�  B�  B���B�  B�  B�  B���B���B�  C   C�C  C  C�C
  C  C  C�fC  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4�C5�fC8  C:  C<  C>  C@  CB  CD�CE�fCH  CJ  CL  CN  CP  CR�CS�fCV  CX  CZ  C\�C^  C_�fCb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cq�fCs�fCu�fCx  Cz�C|  C}�fC�  C�  C�  C��C��C��C�  C��3C�  C��C��C��C��C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C��C�  C��3C��3C�  C��C��C��C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C��3C�  C�  C��3C��C�  C�  C�  C��3C�  C��C�  C��3C��3C��3C��3C��3C�  C�  C�  C��C��C�  C��3C�  C�  C�  C��C�  C��3C�  C��C��C�  C�  C��C��C��C��C�  C�  C�  C��3C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��D fD �fD ��D� D  D� D  D� DfDy�D  D�fD  D� D  D�fD  Dy�D	  D	� D
fD
� D
��D� D  D�fDfD� D��D� D  D�fD  Dy�D  D� D  D�fDfD�fDfD� D  D� D  D� DfD� D  D� DfD� D  D� D  Dy�D  D�fD  D� D  Dy�D  D� D��D � D!  D!y�D"  D"�fD#  D#y�D#��D$� D%fD%� D&  D&� D'  D'�fD(  D(� D)fD)� D)��D*y�D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0fD0�fD1fD1�fD2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9fD9� D9��D:� D;  D;� D<  D<y�D<��D=y�D>  D>�fD?  D?� D@  D@� DA  DA� DBfDB� DB��DCy�DD  DD� DEfDE� DE��DF� DG  DG� DHfDH� DH��DI� DJ  DJ� DJ��DK� DL  DLy�DM  DM� DNfDN� DN��DO� DP  DP� DQ  DQy�DR  DR�fDS  DS� DT  DT� DU  DU� DV  DVy�DW  DW� DW��DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`fD`� Da  Da� Db  Db�fDc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyٚD�,�D�l�D���D��D�,�D�l�D��3D��fD�0 D�\�D���D��fD��D�` Dړ3D���D��D�i�D� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @,��@y��@���@���AffA>ffA`  A~ffA�33A�33A�33A�33A�33A�33A�33A�33B  B��B��B��B'33B/33B733B?��BH  BO��BW��B_��Bg��Bo33Bw33B��B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B�  B�  B�  B���B�  B���Bә�B���B���B���B㙚B���B���B���B�B���B���B���C  C�fC�fC  C	�fC�fC�fC��C�fC�fC�fC�fC��C�fC�fC�fC!�fC#�fC%�fC'�fC*  C+�fC-�fC/�fC1�fC4  C5��C7�fC9�fC;�fC=�fC?�fCA�fCD  CE��CG�fCI�fCK�fCM�fCO�fCR  CS��CU�fCW�fCY�fC\  C]�fC_��Ca�fCc�fCe�fCg��Ci�fCk�fCm�fCo�fCq��Cs��Cu��Cw�fCz  C{�fC}��C�fC��3C��3C�  C�  C�  C��3C��fC��3C�  C�  C�  C�  C��3C��3C��3C��fC��3C�  C��3C��3C�  C��3C��3C��3C�  C��3C��fC��fC��3C�  C�  C�  C�  C��3C��3C��fC��fC��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��fC��fC��3C��3C��fC��3C��3C��3C�  C��3C��3C��fC��fC��3C��3C��fC��3C��3C��fC�  C��3C��3C��3C��fC��3C�  C��3C��fC��fC��fC��fC��fC��3C��3C��3C�  C�  C��3C��fC��3C��3C��3C�  C��3C��fC��3C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C��3C��fC�  C��3C��3C��fC��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C�  D   D � D �3Dy�D��Dy�D��Dy�D  Ds3D��D� D��Dy�D��D� D��Ds3D��D	y�D
  D
y�D
�3Dy�D��D� D  Dy�D�3Dy�D��D� D��Ds3D��Dy�D��D� D  D� D  Dy�D��Dy�D��Dy�D  Dy�D��Dy�D  Dy�D��Dy�D��Ds3D��D� D��Dy�D��Ds3D��Dy�D�3D y�D ��D!s3D!��D"� D"��D#s3D#�3D$y�D%  D%y�D%��D&y�D&��D'� D'��D(y�D)  D)y�D)�3D*s3D*��D+� D+��D,y�D,��D-y�D-��D.y�D.��D/y�D0  D0� D1  D1� D1��D2� D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7�3D8y�D9  D9y�D9�3D:y�D:��D;y�D;��D<s3D<�3D=s3D=��D>� D>��D?y�D?��D@y�D@��DAy�DB  DBy�DB�3DCs3DC��DDy�DE  DEy�DE�3DFy�DF��DGy�DH  DHy�DH�3DIy�DI��DJy�DJ�3DKy�DK��DLs3DL��DMy�DN  DNy�DN�3DOy�DO��DPy�DP��DQs3DQ��DR� DR��DSy�DS��DTy�DT��DUy�DU��DVs3DV��DWy�DW�3DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]s3D]��D^y�D^��D_y�D`  D`y�D`��Day�Da��Db� Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dt` Dy�3D�)�D�i�D��fD��fD�)�D�i�D�� D��3D�,�D�Y�D��fD��3D�fD�\�Dڐ D�ɚD��D�ffD��D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aԥ�Aԣ�Aԡ�Aԩ�Aԧ�Aԛ�A�p�A�;dA�oA���A�ƨA���AӲ-AӓuA�v�A�&�A���AґhA�I�A�(�A��A�VA���A��;AѶFA�dZA�$�Aд9A�1'A�S�A��Aɗ�A�33A�JA�n�A�ȴA�ƨA�VA��PA��hA� �A���A�ƨA���A�VA�"�A��yA�
=A��A�|�A��A�|�A�A�A���A��A��RA�r�A�A�n�A�1A��#A��A�VA���A���A�O�A���A���A�ZA��A��RA�9XA�jA�(�A�$�A��mA��RA���A�z�A���A��A�%A���A��FA�A�M�A�G�A�ffA�K�A�|�A���A���A���A�r�A��A��A�1A�XA�v�A�|�A��A�"�A�ȴA�{A���A��A���A�7LA� �A��DA���A��A�G�A�bNA�XA�&�A�ȴA�bNA��TA��FA~�yA{�PAz�yAy�PAv�uAuS�Atr�At1'AtJAs�FAsp�AsK�As
=AqO�Ao�Am��Al{Ajn�AiƨAh��Af�HAdI�Ab^5A`jA_ƨA_;dA^��A]�AZĜAWK�AV$�AT��ASVAP9XAN~�AL9XAJ��AJ��AJv�AJJAI&�AHȴAG��AF��AF�AE�AEoAD�DAC�ABn�AA��A@��A>�A<�`A<-A;��A:�`A8�!A7�A7G�A6�9A5hsA4n�A3S�A2�A2M�A1&�A/p�A-�FA-G�A+�
A*��A)ƨA)/A(�A(jA({A'+A&$�A%A$JA"��A"A!��A ��A��A�\A;dA�A��A��Al�A�A�AQ�A%AE�A��AXAr�A��A��A+AVAȴA33A�jAbAz�A�^A
��A	��A	�7A	+A�!A�A&�A��AoA�yA1'A��Al�AA�RA�A�jAx�AVA�#At�A ĜA j@���@��-@�z�@�"�@�hs@�(�@�5?@�V@��`@���@�O�@��-@�p�@�x�@��-@���@�ff@���@�I�@�@柾@噚@�j@��;@�
=@�n�@ᙚ@�G�@�?}@���@ߝ�@ޟ�@�-@ݲ-@�X@�/@�V@���@�bN@�"�@�C�@�\)@�o@���@�b@��@�I�@���@Ӯ@��@���@���@��y@�Ĝ@�(�@��y@ɩ�@��@�l�@��@�?}@ģ�@�1'@öF@þw@î@�  @�;d@�^5@�X@���@�C�@���@�?}@�9X@�V@�X@�r�@�1'@��@�l�@�33@�"�@��!@�&�@���@��
@��@�=q@��@��@�;d@���@�/@�7L@��#@���@�V@�%@�Q�@�|�@�$�@�/@��;@�+@�ȴ@�M�@��/@��@��@�l�@�+@�v�@�M�@��@�hs@�A�@�Q�@���@�%@��@��@���@�;d@��!@�+@�=q@�hs@���@��9@�Ĝ@�  @��P@�S�@�"�@�o@�
=@�
=@��H@��R@�~�@�M�@��@�@�p�@��j@��;@��P@�dZ@���@���@�p�@�z�@�Z@�1@���@�K�@�
=@���@�M�@��T@�`B@�G�@�/@��@�V@��/@��@��@�l�@��y@�+@�C�@�
=@��@�ȴ@���@��!@�V@��T@�?}@�Ĝ@��9@��9@��@���@� �@���@�dZ@�K�@�33@�
=@���@�~�@��@���@��-@�x�@�X@�&�@�V@��/@��9@���@�I�@��@�1@��@���@�dZ@�K�@�+@���@�^5@�M�@�E�@�-@�$�@�{@�J@���@��@��@��T@��#@���@��^@���@��@�x�@��@��`@�Ĝ@��@��@�A�@���@|��@r~�@i%@a�@Xr�@Qx�@J�!@B��@;�
@4�D@-?}@'��@!�#@ff@�9@33@�h@	G�@�h@dZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aԥ�Aԣ�Aԡ�Aԩ�Aԧ�Aԛ�A�p�A�;dA�oA���A�ƨA���AӲ-AӓuA�v�A�&�A���AґhA�I�A�(�A��A�VA���A��;AѶFA�dZA�$�Aд9A�1'A�S�A��Aɗ�A�33A�JA�n�A�ȴA�ƨA�VA��PA��hA� �A���A�ƨA���A�VA�"�A��yA�
=A��A�|�A��A�|�A�A�A���A��A��RA�r�A�A�n�A�1A��#A��A�VA���A���A�O�A���A���A�ZA��A��RA�9XA�jA�(�A�$�A��mA��RA���A�z�A���A��A�%A���A��FA�A�M�A�G�A�ffA�K�A�|�A���A���A���A�r�A��A��A�1A�XA�v�A�|�A��A�"�A�ȴA�{A���A��A���A�7LA� �A��DA���A��A�G�A�bNA�XA�&�A�ȴA�bNA��TA��FA~�yA{�PAz�yAy�PAv�uAuS�Atr�At1'AtJAs�FAsp�AsK�As
=AqO�Ao�Am��Al{Ajn�AiƨAh��Af�HAdI�Ab^5A`jA_ƨA_;dA^��A]�AZĜAWK�AV$�AT��ASVAP9XAN~�AL9XAJ��AJ��AJv�AJJAI&�AHȴAG��AF��AF�AE�AEoAD�DAC�ABn�AA��A@��A>�A<�`A<-A;��A:�`A8�!A7�A7G�A6�9A5hsA4n�A3S�A2�A2M�A1&�A/p�A-�FA-G�A+�
A*��A)ƨA)/A(�A(jA({A'+A&$�A%A$JA"��A"A!��A ��A��A�\A;dA�A��A��Al�A�A�AQ�A%AE�A��AXAr�A��A��A+AVAȴA33A�jAbAz�A�^A
��A	��A	�7A	+A�!A�A&�A��AoA�yA1'A��Al�AA�RA�A�jAx�AVA�#At�A ĜA j@���@��-@�z�@�"�@�hs@�(�@�5?@�V@��`@���@�O�@��-@�p�@�x�@��-@���@�ff@���@�I�@�@柾@噚@�j@��;@�
=@�n�@ᙚ@�G�@�?}@���@ߝ�@ޟ�@�-@ݲ-@�X@�/@�V@���@�bN@�"�@�C�@�\)@�o@���@�b@��@�I�@���@Ӯ@��@���@���@��y@�Ĝ@�(�@��y@ɩ�@��@�l�@��@�?}@ģ�@�1'@öF@þw@î@�  @�;d@�^5@�X@���@�C�@���@�?}@�9X@�V@�X@�r�@�1'@��@�l�@�33@�"�@��!@�&�@���@��
@��@�=q@��@��@�;d@���@�/@�7L@��#@���@�V@�%@�Q�@�|�@�$�@�/@��;@�+@�ȴ@�M�@��/@��@��@�l�@�+@�v�@�M�@��@�hs@�A�@�Q�@���@�%@��@��@���@�;d@��!@�+@�=q@�hs@���@��9@�Ĝ@�  @��P@�S�@�"�@�o@�
=@�
=@��H@��R@�~�@�M�@��@�@�p�@��j@��;@��P@�dZ@���@���@�p�@�z�@�Z@�1@���@�K�@�
=@���@�M�@��T@�`B@�G�@�/@��@�V@��/@��@��@�l�@��y@�+@�C�@�
=@��@�ȴ@���@��!@�V@��T@�?}@�Ĝ@��9@��9@��@���@� �@���@�dZ@�K�@�33@�
=@���@�~�@��@���@��-@�x�@�X@�&�@�V@��/@��9@���@�I�@��@�1@��@���@�dZ@�K�@�+@���@�^5@�M�@�E�@�-@�$�@�{@�J@���@��@��@��T@��#@���@��^@���@��@�x�@��@��`@�Ĝ@��@��@�A�@���@|��@r~�@i%@a�@Xr�@Qx�@J�!@B��@;�
@4�D@-?}@'��@!�#@ff@�9@33@�h@	G�@�h@dZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BǮBÖB��B�}BÖBǮBŢBB�qBÖB�}B�RB�^B�3B��B��B~�BcTBy�B�7B��B��B��B�FB�RB�dB��BȴB��B��B��B��B�B�B�/B�5B�/B�BB�TB�TB�NB�NB�B��B��B��B�B�yB�mB�fB�fB�TB�BB�;B�B��B��B��B��BɺB��B�jB��B��B��B�\B~�B`BBR�BH�B5?B)�B�BVBB�B�mB�B�wB�FB�B��B�hBs�Bk�B\)BI�B<jB2-B�BPBB
�B
�;B
��B
ƨB
�LB
�B
��B
��B
�bB
z�B
dZB
Q�B
>wB
/B
%�B
#�B
�B
�B
�B
uB
oB
\B
PB
	7B
B	��B	�B	�TB	�B	��B	ȴB	��B	�?B	��B	��B	��B	��B	��B	�uB	�=B	z�B	dZB	]/B	R�B	H�B	6FB	&�B	�B	hB	\B	VB	PB	1B	B	B��B��B	B	B��B��B��B�B�B�`B�;B�B�B��B��B��BɺBǮBÖB�wB�wB�dB�LB�9B�-B�B��B��B��B��B�oB�\B�PB�=B�7B�%B�B�B� B~�B|�Bz�By�Bz�Bz�Bx�Bx�Bx�Bw�Bw�Bv�Bt�Bu�Br�Bq�Bo�Bm�Bo�Bm�BjBffBcTBcTB`BB]/BZB[#B]/BYBYBW
BT�BR�BO�BJ�BH�BG�BF�BI�BL�BM�BN�BQ�BT�B_;BaHB`BB^5B_;B_;B]/B^5B^5B\)B^5B`BB`BB]/B\)BYBR�BM�BS�BVBVBYBdZBbNBaHBaHBbNBcTBdZBe`BffBe`BffBgmBffBffBiyBjBjBl�Bm�Bn�Bp�Br�Bu�By�B{�B�B�B�B�+B�DB�DB�DB�=B�=B�7B�+B�B�B�B�+B�=B�+B�%B�B�%B�1B�PB�\B�bB�oB��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B�B�'B�'B�'B�B�'B�?B�9B�RB�jB�}BÖBɺB��B��B��B�B�
B�)B�/B�)B�#B�)B�;B�B�B�B�B�B�B�B�B�B��B��B��B	B	B	B	B	B	B	
=B	1B	1B	
=B	JB	\B	�B	�B	�B	�B	�B	"�B	%�B	'�B	'�B	)�B	+B	-B	0!B	1'B	33B	6FB	8RB	:^B	;dB	<jB	@�B	D�B	F�B	I�B	K�B	M�B	O�B	Q�B	W
B	ZB	_;B	`BB	aHB	cTB	cTB	cTB	cTB	hsB	m�B	o�B	t�B	v�B	x�B	z�B	{�B	{�B	{�B	|�B	� B	�B	�B	�+B	�7B	�7B	�=B	�PB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�FB	�RB	�RB	�RB	�XB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�dB	�dB	�dB	�jB	�qB	�qB	��B	ÖB	ÖB	ÖB	ĜB	ƨB	��B	�B	��B
bB
�B
%�B
0!B
7LB
@�B
F�B
M�B
S�B
XB
ZB
_;B
dZB
iyB
o�B
t�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BŢBB�}BĜBȴBƨBĜB��BƨBÖB�wBŢB��B��B�dB��By�B�7B�uB��B��B�B�jB�wBŢB��B��B��B��B�B�)B�;B�BB�NB�TB�ZB�mB�fB�mB�sB�sB�B��BBB�B�B�B�B�yB�mB�ZB�`B�;B�#B��B��B�B��B�BɺB�?B�B��B��B��BjB`BB_;BA�B8RB'�B�BDB��B��B�mBŢB�wB�RB�B��B{�By�Bk�BS�BF�BF�B-B�BoBB
�B
�)B
��B
ÖB
�RB
��B
��B
��B
�\B
v�B
e`B
Q�B
6FB
2-B
49B
#�B
�B
�B
�B
�B
hB
bB
\B
hB
+B	��B	�B	�TB	�
B	��B	��B	ƨB	�FB	�B	��B	��B	��B	��B	��B	�bB	n�B	iyB	`BB	\)B	D�B	5?B	!�B	oB	hB	oB	uB	JB	DB		7B	B	B	+B	1B	1B��B��B��B��B�B�`B�;B�BB�NB�
B��B��B��B��BŢBB��B��B��B�jB�-B�-B��B��B��B��B�oB�hB�hB�hB�VB�DB�=B�B�B�B�B�B�B|�B|�B}�B|�Bz�Bz�Bz�B|�Bz�Bv�Bv�Bw�Bw�Bw�Bs�Br�Bq�Bl�BhsBgmBgmBaHBbNBcTB\)B\)B[#B[#B[#BZBM�BJ�BL�BI�BL�BO�BO�BN�BP�BR�BdZBhsBdZBcTBcTBe`BdZBcTB^5B\)B^5BffBdZB_;B`BBe`BR�BM�BS�BVBYBcTBiyBbNBdZBaHBffBffBdZBhsBffBhsBgmBgmBffBk�BiyBl�Bl�Bl�Bn�Bn�Bq�Bu�Bx�By�B{�B�B�B�DB�\B�VB�JB�PB�=B�VB�PB�VB�B�+B�1B�DB�VB�+B�=B�B�1B�=B�VB�\B�bB�oB��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B�!B�'B�9B�'B�B�9B�?B�RB�jB�wB�}BB��B��B��B��B�B�
B�)B�HB�)B�/B�5B�TB�B�B�B�B�B�B�B�B��B��B��B��B	B	B	1B	+B	B	B	
=B	
=B	1B	DB	PB	hB	�B	�B	�B	�B	�B	"�B	%�B	(�B	(�B	)�B	,B	-B	1'B	33B	5?B	6FB	9XB	:^B	=qB	?}B	C�B	D�B	G�B	J�B	K�B	N�B	P�B	Q�B	XB	ZB	_;B	`BB	aHB	cTB	cTB	dZB	cTB	jB	n�B	o�B	t�B	w�B	y�B	z�B	{�B	{�B	|�B	|�B	�B	�B	�B	�+B	�7B	�7B	�DB	�VB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�3B	�LB	�RB	�RB	�RB	�XB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�dB	�dB	�dB	�jB	�qB	�wB	��B	ÖB	ÖB	ÖB	ĜB	ƨB	��B	�B	��B
bB
�B
%�B
0!B
7LB
@�B
F�B
M�B
S�B
XB
ZB
_;B
dZB
iyB
o�B
t�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<ě�=o=t�<�9X<�9X<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<T��<49X<#�
<u<�o<ě�<#�
<T��<�9X<D��<e`B<#�
<#�
<#�
<#�
<�o<u<#�
<#�
<49X<u<�1<#�
<e`B<u<#�
<#�
<��
<u<#�
<�o<�C�<49X<49X<D��<49X<D��<#�
<#�
<���<��
<�t�<�t�<���<#�
<D��<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<e`B<49X<#�
<#�
<#�
<�o<�C�<T��<49X<#�
<#�
<#�
<D��<�1<�1<#�
<D��<T��<���<e`B<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<49X<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446452012010314464520120103144645  AO  ARGQ                                                                        20111130135813  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135813  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144645  IP                  G�O�G�O�G�O�                