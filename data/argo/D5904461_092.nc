CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-31T10:17:27Z AOML 3.0 creation; 2016-08-07T21:36:42Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151231101727  20160807143642  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               \A   AO  5286_8897_092                   2C  D   APEX                            6531                            072314                          846 @׊SwF��1   @׊T`��@2���$��c@���F1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    \A   B   B   @�33@�  A   A!��A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�33B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3DyL�D��3D�FfD�s3D�� D�fD�C3D��fD�� D�fD�33D���D�ɚD�  D�0 D�|�D��fD�3D�6fD�y�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�  A   A!��A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�33B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3DyL�D��3D�FfD�s3D�� D�fD�C3D��fD�� D�fD�33D���D�ɚD�  D�0 D�|�D��fD�3D�6fD�y�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A�bA�bA�VA�oA�{A��A��A��A�$�A�$�A� �A��`A���A��/A�ƨA̲-A̟�A̩�A�ȴA��#A��
A̼jA̺^A̰!ȂhA�t�A�Q�A�9XA�33A��A��A���A��
A��HA���Aˉ7A�ffA�O�A�;dA�5?A�oA��A�Aʰ!A�l�A���A��HA� �A�$�A�1A�C�A�\)A�ƨA���A�XA�K�A�ĜA��A�33A���A�n�A���A��A���A�A�33A�O�A��^A�
=A��A�O�A�&�A�bA��jA��A�l�A�&�A�Q�A�"�A�z�A���A�O�A�I�A���A�/A��A��DA��A�VA���A�(�A\)A|��Ax�yAs�PAr�+Aq�Aq��Aq/An��Am�Ag��Aa�mA^A\�`A[|�AX��AW��AU��AO"�AN�+AM��AJ�yAG��AGVAD�9A>r�A;��A:�`A9��A9x�A9+A8M�A7��A6�A4�yA2=qA1��A1VA0�A.�A.^5A.{A,�HA,M�A+oA)��A)��A(r�A'�A'��A(��A'�A%�mA%�A#x�A"�\A!K�A&�AjA��AM�A��A�AoAv�A��AC�A��A�
AVA&�A+AC�A��A��A(�AM�A^5AbNAE�AJA��A��A�A
�A	��A	\)AĜA��A�yAt�AG�A��A�hA�Av�Ahs@���@��F@���@�v�@��@�r�@��\@�@���@���@�\@���@��@��@���@웦@�j@��@�1'@�Q�@�9X@��#@߶F@�&�@�bN@ۅ@ڇ+@�$�@�Ĝ@�S�@��@ա�@�1'@Ӆ@��@�V@��/@�K�@�{@ͩ�@�7L@�j@�K�@�n�@ȋD@�+@ǶF@Ǿw@ǝ�@ư!@�p�@��/@�t�@�@�=q@��T@�@�%@�Z@��@��R@�ff@��#@���@�hs@�%@��D@�1@���@���@�@�X@���@�b@��@�M�@�hs@��@��@��u@��u@�Q�@� �@���@��H@��R@�{@��@�x�@�G�@��@�I�@�
=@��@��T@�x�@�&�@�X@�/@��@�1'@��
@�l�@��@��!@���@�@��@�Ĝ@��u@��D@��@��@��F@��
@�bN@��;@�=q@�&�@�/@�G�@��@��F@��F@��@��^@��@��@�1'@�t�@���@��!@���@��+@�^5@�V@�@���@���@�7L@���@��@�Q�@��;@��@���@�X@�@�=q@��!@���@�(�@���@�o@���@��+@�$�@��@�x�@�/@�r�@��w@�"�@���@�5?@�M�@�ff@�x�@�%@��j@��@�ƨ@��P@�K�@�-@�X@�j@��
@�"�@�1@�/@��h@�`B@��@���@��@�(�@�  @�b@��
@��P@�dZ@�\)@�+@��@��@�+@��@�{@�G�@���@�1'@�K�@��H@�V@��R@�V@��@��7@���@���@���@�Q�@�(�@���@���@��@�|�@�;d@��@���@�v�@�ff@�V@�^5@�^5@�@��@���@���@�Ĝ@��9@�j@�9X@�1@��@��@�\)@��@�V@�@�@�x�@�7L@��j@�z�@�1@��P@�t�@�S�@�C�@�"�@��@�
=@���@���@��y@��@�ȴ@��R@���@�~�@�^5@�E�@�=q@�$�@�@���@�O�@�V@���@���@��D@�bN@�  @���@��P@�t�@�K�@�o@���@�$�@��@��^@�hs@�/@�7L@��@��@���@���@�Q�@�(�@�1@�w@�P@\)@~��@}�T@yX@p  @f��@[ƨ@S@O�@G+@@b@:�@2�@.�+@)��@%V@�;@9X@b@�j@r�@�m@�;@`B11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�%A�bA�bA�VA�oA�{A��A��A��A�$�A�$�A� �A��`A���A��/A�ƨA̲-A̟�A̩�A�ȴA��#A��
A̼jA̺^A̰!ȂhA�t�A�Q�A�9XA�33A��A��A���A��
A��HA���Aˉ7A�ffA�O�A�;dA�5?A�oA��A�Aʰ!A�l�A���A��HA� �A�$�A�1A�C�A�\)A�ƨA���A�XA�K�A�ĜA��A�33A���A�n�A���A��A���A�A�33A�O�A��^A�
=A��A�O�A�&�A�bA��jA��A�l�A�&�A�Q�A�"�A�z�A���A�O�A�I�A���A�/A��A��DA��A�VA���A�(�A\)A|��Ax�yAs�PAr�+Aq�Aq��Aq/An��Am�Ag��Aa�mA^A\�`A[|�AX��AW��AU��AO"�AN�+AM��AJ�yAG��AGVAD�9A>r�A;��A:�`A9��A9x�A9+A8M�A7��A6�A4�yA2=qA1��A1VA0�A.�A.^5A.{A,�HA,M�A+oA)��A)��A(r�A'�A'��A(��A'�A%�mA%�A#x�A"�\A!K�A&�AjA��AM�A��A�AoAv�A��AC�A��A�
AVA&�A+AC�A��A��A(�AM�A^5AbNAE�AJA��A��A�A
�A	��A	\)AĜA��A�yAt�AG�A��A�hA�Av�Ahs@���@��F@���@�v�@��@�r�@��\@�@���@���@�\@���@��@��@���@웦@�j@��@�1'@�Q�@�9X@��#@߶F@�&�@�bN@ۅ@ڇ+@�$�@�Ĝ@�S�@��@ա�@�1'@Ӆ@��@�V@��/@�K�@�{@ͩ�@�7L@�j@�K�@�n�@ȋD@�+@ǶF@Ǿw@ǝ�@ư!@�p�@��/@�t�@�@�=q@��T@�@�%@�Z@��@��R@�ff@��#@���@�hs@�%@��D@�1@���@���@�@�X@���@�b@��@�M�@�hs@��@��@��u@��u@�Q�@� �@���@��H@��R@�{@��@�x�@�G�@��@�I�@�
=@��@��T@�x�@�&�@�X@�/@��@�1'@��
@�l�@��@��!@���@�@��@�Ĝ@��u@��D@��@��@��F@��
@�bN@��;@�=q@�&�@�/@�G�@��@��F@��F@��@��^@��@��@�1'@�t�@���@��!@���@��+@�^5@�V@�@���@���@�7L@���@��@�Q�@��;@��@���@�X@�@�=q@��!@���@�(�@���@�o@���@��+@�$�@��@�x�@�/@�r�@��w@�"�@���@�5?@�M�@�ff@�x�@�%@��j@��@�ƨ@��P@�K�@�-@�X@�j@��
@�"�@�1@�/@��h@�`B@��@���@��@�(�@�  @�b@��
@��P@�dZ@�\)@�+@��@��@�+@��@�{@�G�@���@�1'@�K�@��H@�V@��R@�V@��@��7@���@���@���@�Q�@�(�@���@���@��@�|�@�;d@��@���@�v�@�ff@�V@�^5@�^5@�@��@���@���@�Ĝ@��9@�j@�9X@�1@��@��@�\)@��@�V@�@�@�x�@�7L@��j@�z�@�1@��P@�t�@�S�@�C�@�"�@��@�
=@���@���@��y@��@�ȴ@��R@���@�~�@�^5@�E�@�=q@�$�@�@���@�O�@�V@���@���@��D@�bN@�  @���@��P@�t�@�K�@�o@���@�$�@��@��^@�hs@�/@�7L@��@��@���@���@�Q�@�(�@�1@�w@�P@\)@~��G�O�@yX@p  @f��@[ƨ@S@O�@G+@@b@:�@2�@.�+@)��@%V@�;@9X@b@�j@r�@�m@�;@`B11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	ɺB	ɺB	ɺB	ɺB	��B	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	�B	�yB
B
!�B
 �B
!�B
'�B
7LB
L�B
\)B
e`B
n�B
y�B
�bB
��B
�LB
ȴB
��B
��B
��B
��B
�#B
�5B
�ZB
�yB
�sB
�B
�B
��BB+B�B5?BA�BC�B8RB:^B6FBA�B�B<jBXB�%B��Bt�BbB��B�'B=qBPB
��B
�B
�?B
�3B
�-B
�B
��B
��B
��B
�\B
~�B
l�B
`BB
_;B
\)B
T�B
A�B
-B
"�B
�B
\B
+B	��B	�yB	��B	ǮB	�RB	��B	��B	��B	�DB	y�B	e`B	T�B	W
B	W
B	S�B	Q�B	E�B	7LB	�B��B�B�B�mB�/B�B��B�^B�LB�-B�B�!B�B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�hB�bB�\B�\B�VB�PB�DB�=B�DB�JB�VB��BǮB�BB�B�B�B�ZB�/B��B��B��BȴB�qB�-B�RB��B�qB�LB�9B�'B�B��B��B�\B�B{�Bx�Bz�B�B�1B�DB�JB�PB�JB�=B�+B�%B�1B�7B�=B�1B�B�B�B~�By�Bx�Bv�Bq�Bp�Bv�Bw�Bs�BjBcTB_;BcTB�%B� Bw�Bo�BffBZBiyBz�Bp�BffBcTBdZBdZBbNB^5B^5B]/B_;B`BB_;BbNBdZBgmBgmBjBl�Bo�Br�Bt�Bu�Bx�Bz�B~�B� B�B�B�B�B�DB�bB�{B��B��B��B��B��B��B��B��B��B��B�B�'B�-B�3B�3B�3B�9B�?B�LB�LB�XB�dB�qB�}B��BŢBǮB��B��B��B�B�5B�NB�TB�`B�mB�B�B�B�B��B��B��B��B��B	  B	B		7B	PB	\B	hB	{B	�B	�B	�B	�B	�B	%�B	+B	.B	.B	1'B	5?B	5?B	7LB	;dB	D�B	E�B	@�B	?}B	C�B	G�B	O�B	[#B	]/B	dZB	m�B	m�B	cTB	[#B	\)B	\)B	]/B	^5B	^5B	`BB	bNB	dZB	e`B	e`B	hsB	l�B	m�B	p�B	t�B	y�B	~�B	�B	�+B	�DB	�VB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�?B	�LB	�RB	�^B	�qB	�qB	�jB	�jB	�jB	�^B	�LB	�?B	�FB	�RB	�wB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�#B	�B	�B	�#B	�)B	�BB	�HB	�TB	�ZB	�`B	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
1B
1B
1B
+B
+B
+B
1B
1B
1B
DB
JB
PB
PB
PB
VB
bB
bB
\B
�B
VB
�B
!�B
,B
1'B
49B
<jB
B�B
G�B
O�B
T�B
XB
\)B
`BB
dZB
gmB
l�B
o�B
u�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
B
!�B
 �B
!�B
'�B
7RB
L�B
\,B
egB
n�B
y�B
�iB
��B
�SB
ȴB
��B
��B
��B
��B
�'B
�;B
�]B
�}B
�vB
�B
�B
��BB/B�B5ABA�BC�B8TB:]B6FBA�B�B<mBXB�&B��Bt�BfB��B�%B=uBOB
��B
�
B
�AB
�6B
�0B
�B
�B
��B
��B
�_B
~�B
l�B
`GB
_CB
\.B
UB
A�B
-B
"�B
�B
cB
4B	��B	�B	�B	ǹB	�\B	�B	��B	��B	�QB	y�B	eoB	UB	WB	WB	TB	Q�B	E�B	7ZB	�B��B�B�B�|B�@B�%B��B�rB�aB�AB�#B�5B�0B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�zB�sB�sB�lB�fB�XB�SB�ZB�aB�kB��BǾB�VB��B�B�B�lB�@B�B��B��B��B��B�@B�eB��B��B�bB�MB�<B�B�B��B�qB�.B{�Bx�Bz�B�0B�GB�ZB�^B�dB�`B�UB�@B�<B�FB�MB�SB�GB�3B�B�BBy�Bx�Bv�Bq�Bp�Bv�Bw�Bs�Bj�BcjB_QBciB�9B�Bw�Bo�Bf}BZ3Bi�Bz�Bp�Bf|BcmBdnBdpBbdB^KB^LB]DB_QB`ZB_RBbdBdpBg�Bg�Bj�Bl�Bo�Br�Bt�Bu�Bx�Bz�BB�B�!B�4B�4B�3B�YB�vB��B��B��B��B��B��B��B��B��B�B�B�B�7B�@B�BB�FB�DB�MB�RB�_B�]B�jB�xB��B��B��BųBǾB��B��B�B�'B�EB�^B�eB�pB�}B�B�B�B�B��B��B��B�B�
B	 B	,B		GB	`B	lB	vB	�B	�B	�B	�B	�B	�B	%�B	+B	. B	.!B	13B	5LB	5KB	7XB	;qB	D�B	E�B	@�B	?�B	C�B	G�B	O�B	[/B	]:B	dfB	m�B	m�B	caB	[/B	\6B	\6B	]:B	^CB	^@B	`MB	b[B	dfB	ekB	enB	h�B	l�B	m�B	p�B	t�B	y�B	B	�"B	�5B	�NB	�_B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�%B	�/B	�<B	�EB	�VB	�[B	�fB	�zB	�xB	�rB	�rB	�rB	�eB	�WB	�HB	�NB	�ZB	��B	ŪB	ȻB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�$B	�,B	�,B	�%B	�#B	�*B	�0B	�KB	�NB	�\B	�`B	�hB	�tB	�tB	�|B	�zB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
 B
B

B
B
B
B
B
B
B
B
B
B
B
!B
!B
"B
!B
'B
%B
$B
,B
-B
,B
+B
9B
5B
8B
8B
5B
	<B
	>B
	=B
	=B
	>B
8B
9B
:B
3B
3B
4B
7B
8B
8B
GB
SB
VB
UB
VB
[B
hB
hB
aG�O�B
^B
�B
!�B
,B
1-B
4=B
<qB
B�B
G�B
O�B
UB
XB
\-B
`DB
d^B
grB
l�B
o�B
u�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436422016080714364220160807143642  AO  ARCAADJP                                                                    20151231101727    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151231101727  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151231101727  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143642  IP                  G�O�G�O�G�O�                