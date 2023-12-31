CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:03Z UW 3.1 conversion   
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
_FillValue        G�O�     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               *A   AO  20111130140020  20190522121825  1727_5046_042                   2C  D   APEX                            2143                            040306                          846 @�K�ʆ 1   @�K�\���@7\�hr��c��hr�!1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  @���A   A@  A^ffA�  A�  A�33A�  A���A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BHffBP  BW��B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�33B�  B�  B�33B�33B�  C   C  C  C�fC  C
  C  C  C�fC�fC�fC�C  C  C  C  C �C"  C$  C&  C'�fC*  C,�C.  C/�fC2  C4�C6�C8�C:  C<  C>  C@  CA�fCD  CF�CH�CJ  CL  CM�fCP  CR�CT  CV  CW�fCZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Cs�fCv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C��3C��C��C��3C��3C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C��C��C�  C�  C��3C��C��C��3C��3C�  C��C�  C�  C��C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C�  C�  C�  C��C��C��C��C�  C��C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D �fD  Dy�D��D� D  Dy�D��Dy�D  D�fDfD�fDfD� D��Dy�D��D	� D
  D
� DfD� D  D�fD  Dy�D  D�fD  D� D  D� D  D� DfD� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D� D  D� DfD�fD  Dy�D��D� D  D�fD  D� D fD � D ��D!� D"  D"� D#  D#� D$  D$y�D%  D%� D&  D&�fD'  D'� D(fD(� D)  D)� D)��D*� D+  D+� D,  D,� D,��D-� D.  D.y�D/  D/� D0  D0�fD1  D1� D2  D2� D3  D3y�D4  D4�fD5  D5� D6  D6y�D7  D7�fD8  D8� D9  D9� D:  D:� D;fD;� D;��D<� D=  D=� D>  D>�fD?  D?� D@  D@�fDA  DA� DB  DB�fDCfDC�fDD  DD� DE  DEy�DF  DF� DF��DG� DH  DH� DH��DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DR  DR� DS  DS� DS��DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj�fDkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy�fD�9�D�` D���D��D�  D�S3D��3D��fD�33D�Y�D��fD���D�&fD�\�Dڣ3D�� D�0 D�i�D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @9��@�  @�  @���A   A@  A^ffA�  A�  A�33A�  A���A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BHffBP  BW��B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�33B�  B�  B�33B�33B�  C   C  C  C�fC  C
  C  C  C�fC�fC�fC�C  C  C  C  C �C"  C$  C&  C'�fC*  C,�C.  C/�fC2  C4�C6�C8�C:  C<  C>  C@  CA�fCD  CF�CH�CJ  CL  CM�fCP  CR�CT  CV  CW�fCZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Cs�fCv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C��3C��C��C��3C��3C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C��C��C�  C�  C��3C��C��C��3C��3C�  C��C�  C�  C��C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C�  C�  C�  C��C��C��C��C�  C��C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D �fD  Dy�D��D� D  Dy�D��Dy�D  D�fDfD�fDfD� D��Dy�D��D	� D
  D
� DfD� D  D�fD  Dy�D  D�fD  D� D  D� D  D� DfD� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D� D  D� DfD�fD  Dy�D��D� D  D�fD  D� D fD � D ��D!� D"  D"� D#  D#� D$  D$y�D%  D%� D&  D&�fD'  D'� D(fD(� D)  D)� D)��D*� D+  D+� D,  D,� D,��D-� D.  D.y�D/  D/� D0  D0�fD1  D1� D2  D2� D3  D3y�D4  D4�fD5  D5� D6  D6y�D7  D7�fD8  D8� D9  D9� D:  D:� D;fD;� D;��D<� D=  D=� D>  D>�fD?  D?� D@  D@�fDA  DA� DB  DB�fDCfDC�fDD  DD� DE  DEy�DF  DF� DF��DG� DH  DH� DH��DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DR  DR� DS  DS� DS��DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj�fDkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy�fD�9�D�` D���D��D�  D�S3D��3D��fD�33D�Y�D��fD���D�&fD�\�Dڣ3D�� D�0 D�i�D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A̗�A̝�A̙�A̝�A̗�A̗�A̙�A̙�A̓uẢ7Ả7A̋DA̋DẢ7A̋DA̋DA̋DA̍PA̋DẢ7A̋DA̋DA̍PA̍PȀ\A̋DA̍PA̍PȀ\Ȁ\A̍PA̍PA̍PȦ+A�7LAƬA��-A�K�A�"�A�C�A��TA���A�=qA��DA���A��wA�S�A���A��\A��jA�oA��A��A��^A�r�A���A��jA��A���A�S�A�{A��A�VA�n�A�ƨA� �A�=qA��A�z�A���A�p�A�n�A���A��A�"�A���A��RA� �A�33A��A���A��uA��+A���A�$�A�ĜA�n�A��A�r�A��
A�VA��A��^A�z�A�^5A���A�hsA��A���A���A�;dA�A���A��hA��9A�x�AA�A~VA|�Az~�Ay%Av�\As��As&�Aq33ApJApAo�TAo�Al�Ai��Ag7LAe��Ad=qAc�Ab�A`9XA]��AZbNAY7LAX��AW�7AT�jAS�
AR�DAR{AQ��AN��AM�-AMx�AL��ALJALZAK�;AJȴAIS�AH�9AH�AF��AE�-AD�AC��ACO�ACC�ABbNA@�A?�A>��A=�
A<bNA:��A9�A9l�A9G�A8��A7|�A6z�A4v�A3oA1�TA1�A/�#A.(�A-;dA,��A+O�A*M�A)��A)G�A(��A(9XA'�wA'33A&v�A%�hA%�A$�A$��A$�A#O�A"VA!/A bNA��A  A�uA\)Av�An�A$�A%AVA��A�A�A"�AoA~�At�A1'A/A^5A��A`BA�A��A�uA9XAt�A
�9A
ffA	�A�AQ�A9XA|�AE�A;dA%Ar�A�yAƨA+A bN@��m@���@���@�r�@�1@�-@���@��@��@�t�@���@�7L@�33@���@��@�1@�r�@�-@�w@�-@㝲@�j@�@�G�@�  @���@��@أ�@׍P@�@ԣ�@��@�@�(�@�"�@͙�@�r�@ʟ�@���@��@ȓu@��
@ǥ�@�C�@Ɨ�@Ų-@�hs@�%@ēu@�Q�@�1@�;d@��@��^@��j@�@���@���@�n�@�hs@�z�@�"�@�-@���@��@���@�b@�ƨ@�S�@��H@�5?@��@���@�~�@��@��@�J@��^@���@�b@��@�@�?}@�I�@���@��@�~�@�{@�?}@��@�C�@�
=@��@�ȴ@��@��`@�1@��H@�=q@��#@�hs@���@��9@� �@�"�@�J@�x�@�X@�O�@���@�1'@���@�ƨ@���@���@�|�@�\)@�o@��@��@���@�~�@�^5@�M�@�$�@��@�@���@�x�@�p�@�?}@��/@���@�bN@�  @���@��P@��@�~�@�V@�5?@���@��#@���@�7L@�Z@�  @���@���@�?}@�r�@�33@��@�C�@�\)@�\)@�t�@��@��F@�  @�A�@�z�@�j@��@��m@��w@�o@��@��!@�J@��7@��@�Z@���@��F@�K�@��H@���@���@��\@�v�@�M�@�5?@�{@�@�G�@��@���@���@�1'@�b@�1@��@��w@�|�@�C�@���@�E�@��#@�x�@�?}@��`@��@�Z@�b@��@��@��m@���@���@�dZ@�+@��y@���@�n�@�ff@�E�@��#@�x�@�/@���@��@�z�@�j@�9X@��F@���@�|�@�S�@��@��!@�v�@�=q@��@���@�p�@�`B@�7L@�%@�Ĝ@���@���@��@�(�@�;@��@�w@�w@�w@�@�@\)@~�R@~��@|�D@v��@n�R@ep�@` �@Z-@T��@O;d@F�@?�;@7�w@3�m@-�@'�@"�\@�@ff@�^@p�@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A̗�A̝�A̙�A̝�A̗�A̗�A̙�A̙�A̓uẢ7Ả7A̋DA̋DẢ7A̋DA̋DA̋DA̍PA̋DẢ7A̋DA̋DA̍PA̍PȀ\A̋DA̍PA̍PȀ\Ȁ\A̍PA̍PA̍PȦ+A�7LAƬA��-A�K�A�"�A�C�A��TA���A�=qA��DA���A��wA�S�A���A��\A��jA�oA��A��A��^A�r�A���A��jA��A���A�S�A�{A��A�VA�n�A�ƨA� �A�=qA��A�z�A���A�p�A�n�A���A��A�"�A���A��RA� �A�33A��A���A��uA��+A���A�$�A�ĜA�n�A��A�r�A��
A�VA��A��^A�z�A�^5A���A�hsA��A���A���A�;dA�A���A��hA��9A�x�AA�A~VA|�Az~�Ay%Av�\As��As&�Aq33ApJApAo�TAo�Al�Ai��Ag7LAe��Ad=qAc�Ab�A`9XA]��AZbNAY7LAX��AW�7AT�jAS�
AR�DAR{AQ��AN��AM�-AMx�AL��ALJALZAK�;AJȴAIS�AH�9AH�AF��AE�-AD�AC��ACO�ACC�ABbNA@�A?�A>��A=�
A<bNA:��A9�A9l�A9G�A8��A7|�A6z�A4v�A3oA1�TA1�A/�#A.(�A-;dA,��A+O�A*M�A)��A)G�A(��A(9XA'�wA'33A&v�A%�hA%�A$�A$��A$�A#O�A"VA!/A bNA��A  A�uA\)Av�An�A$�A%AVA��A�A�A"�AoA~�At�A1'A/A^5A��A`BA�A��A�uA9XAt�A
�9A
ffA	�A�AQ�A9XA|�AE�A;dA%Ar�A�yAƨA+A bN@��m@���@���@�r�@�1@�-@���@��@��@�t�@���@�7L@�33@���@��@�1@�r�@�-@�w@�-@㝲@�j@�@�G�@�  @���@��@أ�@׍P@�@ԣ�@��@�@�(�@�"�@͙�@�r�@ʟ�@���@��@ȓu@��
@ǥ�@�C�@Ɨ�@Ų-@�hs@�%@ēu@�Q�@�1@�;d@��@��^@��j@�@���@���@�n�@�hs@�z�@�"�@�-@���@��@���@�b@�ƨ@�S�@��H@�5?@��@���@�~�@��@��@�J@��^@���@�b@��@�@�?}@�I�@���@��@�~�@�{@�?}@��@�C�@�
=@��@�ȴ@��@��`@�1@��H@�=q@��#@�hs@���@��9@� �@�"�@�J@�x�@�X@�O�@���@�1'@���@�ƨ@���@���@�|�@�\)@�o@��@��@���@�~�@�^5@�M�@�$�@��@�@���@�x�@�p�@�?}@��/@���@�bN@�  @���@��P@��@�~�@�V@�5?@���@��#@���@�7L@�Z@�  @���@���@�?}@�r�@�33@��@�C�@�\)@�\)@�t�@��@��F@�  @�A�@�z�@�j@��@��m@��w@�o@��@��!@�J@��7@��@�Z@���@��F@�K�@��H@���@���@��\@�v�@�M�@�5?@�{@�@�G�@��@���@���@�1'@�b@�1@��@��w@�|�@�C�@���@�E�@��#@�x�@�?}@��`@��@�Z@�b@��@��@��m@���@���@�dZ@�+@��y@���@�n�@�ff@�E�@��#@�x�@�/@���@��@�z�@�j@�9X@��F@���@�|�@�S�@��@��!@�v�@�=q@��@���@�p�@�`B@�7L@�%@�Ĝ@���@���@��@�(�@�;@��@�w@�w@�w@�@�@\)@~�R@~��@|�D@v��@n�R@ep�@` �@Z-@T��@O;d@F�@?�;@7�w@3�m@-�@'�@"�\@�@ff@�^@p�@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�ZB�ZB�fB�ZB�ZB�ZB�`B�`B�`B�`B�fB�`B�`B�`B�`B�`B�`B�fB�fB�fB�fB�fB�fB�fB�`B�fB�`B�`B�`B�ZB�TB�HB�/B�B��B��B�
B��B��B��B��B��BƨBĜBÖBB��BÖBB��B�}B�qB�dB�RB�9B�FB�?B�FB��B��B�B��B��B��B��B�bB�Bt�Bl�BbNBQ�B;dB:^B&�B�B�B\B��B�ZB��B��BƨBB�3B�uBw�BdZBZBP�BB�B5?B#�B	7B
�B
�5B
��B
�FB
�uB
�+B
|�B
p�B
jB
aHB
VB
J�B
F�B
D�B
?}B
7LB
)�B
�B
hB
B	�B	�B	�ZB	�/B	�#B	�B	��B	��B	�B	��B	��B	�DB	�B	{�B	p�B	dZB	[#B	T�B	P�B	K�B	K�B	D�B	C�B	A�B	=qB	49B	/B	.B	(�B	(�B	2-B	49B	-B	#�B	 �B	�B	�B	�B	�B	�B	�B	uB	\B	DB	B��B��B�B�B�B�B�B�B�ZB�5B�B��B��BɺBŢB��B�wB�jB�XB�FB�9B�-B�'B�B�B�B��B��B��B��B��B��B��B��B��B�oB�VB�=B�1B�%B�B�B�B}�B{�Bz�Bw�Bu�Bs�Bq�Bo�Bm�BjBjBiyBhsBffBdZBcTB_;B]/B[#B[#B\)B]/BXBW
BZBVBQ�BQ�BP�BN�BK�BH�BE�BD�BB�BH�BI�BH�BF�BD�BA�BA�BG�BP�BN�BP�BN�BH�BA�B<jBG�BC�B@�B:^B2-B+B'�B'�B'�B(�B'�B(�B'�B(�B'�B'�B&�B'�B&�B(�B)�B.B/B/B0!B1'B1'B1'B2-B49B49B49B49B49B33B33B6FB5?B6FB:^B:^B:^B:^B<jB<jB>wB@�BA�BA�BC�BC�BC�BB�BB�BC�BC�BC�BK�BL�BL�BL�BM�BO�BQ�BS�BW
BW
BZB]/B^5B^5B_;Be`BffBhsBiyBjBiyBk�Bq�Bv�B|�B� B�B�B�+B�+B�=B�\B��B��B��B��B��B��B��B��B�B�B�B�-B�9B�FB�XB�wBBĜBŢBǮB��B��B��B��B��B�B�)B�/B�;B�NB�fB�yB�B�B�B�B��B��B��B	B		7B	VB	�B	�B	�B	 �B	$�B	&�B	)�B	1'B	33B	33B	49B	6FB	8RB	:^B	=qB	B�B	B�B	D�B	K�B	O�B	P�B	Q�B	VB	XB	[#B	aHB	dZB	e`B	hsB	k�B	l�B	m�B	n�B	n�B	p�B	p�B	q�B	s�B	x�B	|�B	~�B	�B	�+B	�7B	�7B	�DB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�?B	�FB	�FB	�FB	�LB	�XB	�^B	�jB	�wB	��B	B	ĜB	ȴB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�HB	�HB	�fB	�B
B
{B
�B
!�B
(�B
2-B
9XB
@�B
G�B
N�B
VB
ZB
]/B
aHB
ffB
jB
o�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�ZB�ZB�fB�ZB�ZB�ZB�`B�`B�`B�`B�fB�`B�`B�`B�`B�`B�`B�fB�fB�fB�fB�fB�fB�fB�`B�fB�`B�`B�`B�ZB�TB�NB�NB�BBB��B��B�fB�;B�B��B��B��B��B��B��B��B��BɺBĜBĜB��B��BŢBŢB�}BƨB�B��B�'B�LB�XB��B��B��B�oB|�By�Bo�B^5BC�BG�B/B"�B,B �BB�B��B��B��B��BǮB��B�+Bk�BbNBYBL�BE�B;dB�BB
�B
�`B
��B
��B
�uB
�B
u�B
r�B
n�B
_;B
O�B
N�B
J�B
G�B
E�B
6FB
(�B
!�B
oB	��B	��B	�B	�5B	�;B	�HB	�`B	��B	�jB	�B	��B	�{B	�JB	�JB	�B	u�B	cTB	[#B	\)B	[#B	R�B	K�B	H�B	I�B	M�B	9XB	2-B	33B	-B	(�B	7LB	<jB	6FB	(�B	&�B	%�B	 �B	�B	�B	�B	�B	�B	�B	{B		7B	+B	B��B��B�B�B��B��B�B�B�NB�#B�B��B��BǮBĜBĜB�}B�^B�RB�FB�9B�3B�3B�'B�B�B��B��B��B��B��B��B��B��B��B�{B�bB�DB�+B�+B�1B�B�B� B~�Bz�Bu�Bv�Bw�Bu�Bp�Bp�Bn�Bk�Bk�Bl�BiyBbNBbNB`BB^5BaHBe`BZBZB`BB^5BXBT�BW
BXBR�BM�BJ�BH�BH�BK�BK�BK�BL�BI�BD�BB�BI�BT�BT�BYBXBQ�BF�B>wBO�BL�BH�BB�B<jB+B.B'�B'�B-B-B(�B'�B(�B-B'�B&�B'�B&�B.B)�B.B/B1'B2-B2-B1'B49B5?B49B6FB49B5?B49B7LB7LB8RB:^B;dB;dB:^B<jB:^B<jB<jB>wB@�BA�BA�BC�BC�BC�BB�BB�BC�BG�BI�BL�BL�BL�BN�BM�BO�BVBXBW
B[#B]/B]/B`BB^5BbNBhsBjBiyBjBl�BiyBo�Bt�Bz�B|�B�B�B�B�7B�=B�VB�\B��B��B��B��B��B��B��B�B�B�B�!B�3B�9B�FB�XB�}BÖBĜBŢBȴB��B��B��B��B��B�B�)B�5B�BB�TB�mB�B�B�B�B�B��B��B��B	B		7B	JB	�B	�B	�B	#�B	$�B	&�B	)�B	1'B	33B	33B	33B	5?B	8RB	:^B	=qB	B�B	C�B	E�B	M�B	O�B	Q�B	S�B	XB	YB	]/B	aHB	e`B	ffB	iyB	k�B	l�B	m�B	n�B	n�B	p�B	q�B	q�B	t�B	y�B	|�B	� B	�B	�+B	�7B	�7B	�JB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	�?B	�FB	�FB	�LB	�RB	�^B	�dB	�qB	�wB	��B	ÖB	ŢB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�HB	�HB	�fB	�B
B
{B
�B
!�B
(�B
2-B
9XB
@�B
H�B
N�B
VB
ZB
]/B
aHB
ffB
jB
o�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��
=@�=H�9<�h=#�
<��
<�o<#�
<#�
<#�
<49X<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<u<#�
<�o<T��<#�
<#�
<e`B<�C�<#�
<#�
<T��<T��<#�
<T��<e`B<D��<#�
<T��<#�
<#�
<�o<�C�<D��<49X<#�
<#�
<#�
<u<��
<�o<u<#�
<#�
<#�
<#�
<�o<�j<�C�<u<e`B<��
<ě�<���<D��<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<e`B<D��<T��<�o<�o<#�
<D��<#�
<#�
<#�
<#�
<��
<���<u<#�
<#�
<#�
<#�
<�o<���<�C�<#�
<#�
<49X<u<#�
<#�
<#�
<#�
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446482012010314464820120103144648  AO  ARGQ                                                                        20111130140020  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140020  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144648  IP                  G�O�G�O�G�O�                