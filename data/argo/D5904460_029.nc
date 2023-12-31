CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:09Z AOML 3.0 creation; 2016-08-07T21:17:33Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cx   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KX   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20150226221309  20160807141733  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_029                   2C  D   APEX                            6487                            072314                          846 @�5����1   @�5�UU@
@-.z�G��d/��w1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B��B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bo��Bx  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�33B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy� D�3D�9�D�l�D�� D�� D�9�D��3D�ɚD� D�S3D�y�Dǩ�D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@c34@���@���A
ffA*ffAJffAjffA�33A�33A�33A�33A�33A�33A�33A�33B��B
34B34B��B"��B*��B2��B:��BB��BJ��BR��B[  Bc  Bj��Br34Bz��B�L�B�L�B�L�B�L�B�L�B�� B��gB��B�L�B�L�B�L�B�� B��B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C �fC�fC�fC�fC�fC
�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC �fC"�fC$�fC&�fC(�fC*�fC,�fC.�fC0�fC2�fC4�fC6�fC8�fC:�fC<�fC>� C@�fCB�fCD�fCF�fCH�fCJ�fCL�fCN�fCP�fCR�fCT�fCV�fCX�fCZ�fC\�fC^�fC`�fCb�fCd�fCf�fCh�fCj�fCl�fCn�fCp�fCr�fCt�fCv�fCx�fCz�fC|�fC~�fC�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3D )�D ��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D	)�D	��D
)�D
��D)�D��D)�D��D)�D��D)�D��D)�D��D0 D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D )�D ��D!)�D!��D")�D"��D#)�D#� D$)�D$��D%)�D%��D&)�D&��D')�D'��D()�D(��D))�D)��D*)�D*��D+)�D+��D,)�D,��D-)�D-��D.)�D.��D/)�D/��D0)�D0��D1)�D1��D2)�D2��D3)�D3��D4)�D4��D5)�D5��D6)�D6��D7)�D7��D8)�D8��D9)�D9��D:)�D:��D;)�D;��D<)�D<��D=)�D=��D>)�D>��D?)�D?��D@)�D@��DA)�DA��DB)�DB��DC)�DC��DD)�DD��DE)�DE��DF)�DF��DG)�DG��DH)�DH��DI)�DI��DJ)�DJ��DK)�DK��DL)�DL��DM)�DM��DN)�DN��DO)�DO��DP)�DP��DQ)�DQ��DR)�DR��DS)�DS��DT)�DT��DU)�DU��DV)�DV��DW)�DW��DX)�DX��DY)�DY��DZ)�DZ��D[)�D[��D\)�D\��D])�D]��D^)�D^��D_)�D_��D`)�D`��Da)�Da��Db)�Db��Dc)�Dc��Dd)�Dd��De)�De��Df)�Df��Dg)�Dg��Dh)�Dh��Di)�Di��Dj)�Dj��Dk)�Dk��Dl)�Dl��Dm)�Dm��Dn)�Dn��Do)�Do��Dp)�Dp��Dq)�Dq��Dr)�Dr��Ds)�Ds��Dt)�Dt��Du�Dy�D�( D�NgD���D���D��D�NgD�� D��gD�$�D�h D��gDǾgD�( 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yA��A��A��A��A��mA��A��A��A��A��A��A���A���A�  A��A�ffA�ZA�oẠ�A̟�A̓uȂhA̅A�hsA�=qA�33A�$�A���A��A��mA��;A�A��#A˕�A�hsAʲ-A�A�A��#A�bA�+A���A��HA��A��9A��-A��A��A�9XA�{A�S�A��A�t�A� �A��jA��A�G�A�(�A��A�"�A�-A�
=A�VA�p�A�O�A�7LA�I�A��A��DA�t�A��-A�A�A�dZA��A���A�ffA�ĜA���A�oA�VA��hA��RA�K�A���A���A�bNA|��Az�`Av-As�
Apv�AlE�Ai��Af��Ac�Ab��Aa��A`�A]�hA\�AZ=qATM�APJAM7LAJv�AI
=AFJAB�9AA��A@z�A@n�A>VA;�
A;��A9�#A8��A61'A4�DA1�PA/&�A-7LA+
=A+��A-��A0-A1��A0v�A,�A*��A(�9A%?}A$��A$�A#��A#�A"��A"ffA"A�A"�DA"�A#dZA$��A%7LA%O�A%�TA&  A& �A%�A%t�A%;dA%+A$��A$$�A#p�A#%A"(�A!p�A ��A JAG�A�HA��AM�A�PAZA�
A
=A�9AjAM�AE�AM�A��A�DA��AG�A�RAZA��Al�A�A�`A1'A��AK�A�9A��A��A=qA�AK�A�A�RA{AAdZA�A�mA�FA��A`BA&�A%A
�`A
��A
VA	��A	�A�`A�A$�A��AƨAC�A��A��Ar�A  A�FAS�A��AĜA�DA�#AdZA��AZA�;A33A �9A =q@�ƨ@�+@�~�@�5?@���@���@��F@�~�@�/@�b@�
=@���@��#@��@�(�@�dZ@��@�@�7L@�r�@� �@�+@���@�O�@�&�@�r�@�ƨ@땁@띲@띲@�o@�+@�@�`B@蛦@�ƨ@�dZ@�R@�n�@�$�@��@�`B@��@��/@�bN@� �@�w@�R@�^5@�E�@�{@�%@��m@�
=@�ff@�M�@�5?@��@ݡ�@�`B@�9X@۾w@ە�@ۍP@�dZ@�o@��@�ȴ@ڏ\@�V@�J@ى7@�Q�@�K�@��@�V@�@��#@Ձ@�&�@���@�j@��H@�V@�5?@љ�@���@�Q�@Ͼw@���@��@͑h@�%@�j@�1'@���@�o@�n�@��@���@�dZ@ƸR@�@�?}@�z�@�Q�@�b@î@�
=@�^5@���@��h@�hs@���@�Q�@�  @�S�@�ff@��#@���@�(�@��
@�;d@���@�-@���@�p�@�/@��@��/@�j@��@���@�K�@�~�@�M�@�E�@�@�p�@��@���@�r�@�I�@�  @�K�@��@���@��\@�ff@�=q@���@��@��h@��D@�1'@��
@���@�
=@�ff@��T@��7@�`B@��@���@�Z@� �@���@�;d@��R@�V@�{@��@�X@�&�@���@�bN@�Q�@� �@���@���@�l�@��@�v�@�{@��T@��7@�`B@�/@���@��`@��@�j@� �@���@�"�@��@�$�@��-@�&�@���@��@��u@�Q�@��m@��@��@��H@���@�ff@�V@�E�@�-@�J@��@���@���@��/@�Z@� �@��
@���@�+@���@��H@���@�{@���@�@���@���@���@��@��@���@�r�@�9X@�  @���@�t�@�\)@�C�@��@��@���@��R@�n�@�^5@�-@��@��-@�hs@���@�bN@�1@�l�@�33@���@���@�E�@���@�v�@��T@��j@vV@n��@f�+@_��@W�w@R-@I�^@@��@:~�@4z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111A��yA��A��A��A��A��mA��A��A��A��A��A��A���A���A�  A��A�ffA�ZA�oẠ�A̟�A̓uȂhA̅A�hsA�=qA�33A�$�A���A��A��mA��;A�A��#A˕�A�hsAʲ-A�A�A��#A�bA�+A���A��HA��A��9A��-A��A��A�9XA�{A�S�A��A�t�A� �A��jA��A�G�A�(�A��A�"�A�-A�
=A�VA�p�A�O�A�7LA�I�A��A��DA�t�A��-A�A�A�dZA��A���A�ffA�ĜA���A�oA�VA��hA��RA�K�A���A���A�bNA|��Az�`Av-As�
Apv�AlE�Ai��Af��Ac�Ab��Aa��A`�A]�hA\�AZ=qATM�APJAM7LAJv�AI
=AFJAB�9AA��A@z�A@n�A>VA;�
A;��A9�#A8��A61'A4�DA1�PA/&�A-7LA+
=A+��A-��A0-A1��A0v�A,�A*��A(�9A%?}A$��A$�A#��A#�A"��A"ffA"A�A"�DA"�A#dZA$��A%7LA%O�A%�TA&  A& �A%�A%t�A%;dA%+A$��A$$�A#p�A#%A"(�A!p�A ��A JAG�A�HA��AM�A�PAZA�
A
=A�9AjAM�AE�AM�A��A�DA��AG�A�RAZA��Al�A�A�`A1'A��AK�A�9A��A��A=qA�AK�A�A�RA{AAdZA�A�mA�FA��A`BA&�A%A
�`A
��A
VA	��A	�A�`A�A$�A��AƨAC�A��A��Ar�A  A�FAS�A��AĜA�DA�#AdZA��AZA�;A33A �9A =q@�ƨ@�+@�~�@�5?@���@���@��F@�~�@�/@�b@�
=@���@��#@��@�(�@�dZ@��@�@�7L@�r�@� �@�+@���@�O�@�&�@�r�@�ƨ@땁@띲@띲@�o@�+@�@�`B@蛦@�ƨ@�dZ@�R@�n�@�$�@��@�`B@��@��/@�bN@� �@�w@�R@�^5@�E�@�{@�%@��m@�
=@�ff@�M�@�5?@��@ݡ�@�`B@�9X@۾w@ە�@ۍP@�dZ@�o@��@�ȴ@ڏ\@�V@�J@ى7@�Q�@�K�@��@�V@�@��#@Ձ@�&�@���@�j@��H@�V@�5?@љ�@���@�Q�@Ͼw@���@��@͑h@�%@�j@�1'@���@�o@�n�@��@���@�dZ@ƸR@�@�?}@�z�@�Q�@�b@î@�
=@�^5@���@��h@�hs@���@�Q�@�  @�S�@�ff@��#@���@�(�@��
@�;d@���@�-@���@�p�@�/@��@��/@�j@��@���@�K�@�~�@�M�@�E�@�@�p�@��@���@�r�@�I�@�  @�K�@��@���@��\@�ff@�=q@���@��@��h@��D@�1'@��
@���@�
=@�ff@��T@��7@�`B@��@���@�Z@� �@���@�;d@��R@�V@�{@��@�X@�&�@���@�bN@�Q�@� �@���@���@�l�@��@�v�@�{@��T@��7@�`B@�/@���@��`@��@�j@� �@���@�"�@��@�$�@��-@�&�@���@��@��u@�Q�@��m@��@��@��H@���@�ff@�V@�E�@�-@�J@��@���@���@��/@�Z@� �@��
@���@�+@���@��H@���@�{@���@�@���@���@���@��@��@���@�r�@�9X@�  @���@�t�@�\)@�C�@��@��@���@��R@�n�@�^5@�-@��@��-@�hs@���@�bN@�1@�l�@�33@���@���G�O�@���@�v�@��T@��j@vV@n��@f�+@_��@W�w@R-@I�^@@��@:~�@4z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	(�B	/B	J�B	�=B	�B
�B
�XB
��B
��B
�
B
�B
�
B
��B
�B
�)B
�)B
�fB
��B-B^5Bp�Bt�B{�B�bB��B!�B0!B)�B�B33BA�BJ�BE�Bl�BƨB��B�#B�BŢB��B�B�%B�B�XB�9B��By�Bo�BdZBH�B:^B;dB>wB.B>wB^5BR�BB�B�B�B�B��B��B��Bx�B33B
�B
��B
�B
~�B
`BB
J�B
)�B	��B	�B	��B	�dB	��B	�VB	{�B	hsB	VB	Q�B	K�B	?}B	33B	,B	!�B	DB��B��B��B��B��B	&�B	7LB	8RB	D�B	_;B	XB	XB	W
B	R�B	L�B	G�B	J�B	D�B	:^B	7LB	R�B	x�B	�B	��B	��B	�FB	�B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	�^B	ÖB	��B	�sB	��B	��B
PB
(�B
8RB
=qB
B�B
B�B
B�B
A�B
@�B
@�B
A�B
A�B
?}B
>wB
>wB
?}B
A�B
A�B
@�B
A�B
B�B
E�B
I�B
J�B
L�B
M�B
T�B
^5B
[#B
ZB
VB
R�B
P�B
Q�B
P�B
N�B
M�B
L�B
I�B
G�B
E�B
@�B
:^B
6FB
33B
1'B
0!B
/B
-B
+B
)�B
(�B
&�B
%�B
%�B
%�B
$�B
$�B
#�B
#�B
"�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
oB
hB
\B
VB
VB
VB
PB
JB
JB
PB
JB
DB
DB
DB

=B
1B
1B
+B
+B
+B
+B
+B
1B
1B
1B
1B

=B
	7B
%B
+B
+B
1B

=B
DB
DB
DB
DB
DB

=B

=B

=B

=B
	7B
1B
	7B
	7B
	7B
	7B
	7B
1B
1B
+B
+B
+B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
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

=B
DB
DB
JB
PB
PB
VB
VB
VB
VB
VB
PB
VB
VB
VB
\B
\B
hB
hB
hB
oB
oB
uB
uB
oB
uB
uB
uB
uB
{B
{B
{B
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
'�B
+B
0!B
6FB
9XB
B�B
I�B
M�B
O�B
VB
[#B
_;B
dZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	(�B	.�B	J�B	�B	�B
��B
�(B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�4B
��B,�B^ BppBt�B{�B�-B�RB!�B/�B)�BrB2�BASBJ�BEgBlPB�oBΡB��B��B�iB��B��B��B��B� B� B�`By�Bo`BdBHwB:B;&B><B-�B>:B]�BR�BBSBOB�fB��B�FB��B�GBx�B2�B
�yB
ЧB
��B
~�B
`B
J�B
)�B	��B	�QB	ϦB	�.B	��B	�B	{�B	h<B	U�B	Q�B	K�B	?HB	2�B	+�B	!�B	B��B��B��B��B��B	&�B	7B	8B	DbB	_B	W�B	W�B	V�B	R�B	L�B	GqB	J�B	D_B	:"B	7B	R�B	x�B	��B	ӵB	ѫB	�B	��B	�~B	�:B	�XB	�|B	�qB	�dB	�eB	��B	��B	�B	�TB	̋B	�1B	��B	��B

B
(�B
8B
=*B
BIB
BJB
BHB
AEB
@>B
@9B
ACB
ACB
?3B
>/B
>1B
?5B
AAB
ACB
@;B
AAB
BJB
E[B
ItB
JzB
L�B
M�B
T�B
]�B
Z�B
Y�B
U�B
R�B
P�B
Q�B
P�B
N�B
M�B
L�B
IqB
GfB
EZB
@;B
:B
5�B
2�B
0�B
/�B
.�B
,�B
*�B
)�B
(�B
&�B
%�B
%�B
%�B
$�B
$�B
#�B
#�B
"�B
!�B
!�B
 ~B
vB
vB
oB
iB
fB
TB
_B
_B
_B
WB
SB
KB
>B
>B
1B
)B
&B
%B
!B
B
B
B
B
B
 B
B
B
B

�B

�B

�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
�B
�B
�B
�B
�B
	�B

�B

�B

�B

�B

�B
	�B
	�B
	�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�vB	�B	��B	��B	��B	��B	��B	��B	�~B	�}B	�}B	�vB	�uB	�{B	�~B	�B	�wB	�vB	�pB	�pB	�oB	�oB	�oB	�oB	�rB	�}B	�~B	�wB	�vB	�qB	�rB	�mB	�rB	�oB	�yB	�vB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B
	�B
	�B

�B

�B

�B

�B

�B
	�B

�B

�B
�B
B
B
B
B
	B
B
B
B
B
	B
B
B
B
B
B
B
B
 B
(B
(B
!B
'B
'B
%B
)B
+B
+B
-B
&B
*B
,B
+B
2B
3B
3B
9B
9B
;B
:B
8B
=B
=B
8B
9B
8B
7B
?B
@B
>B
=B
EB
FB
MB
QB
KB
KB
JB
SB
QB
JB
PB
QB
PB
SB
QB
QB
QB
ZB
XB
XB
VB
WB
WB
[B
[B
eB
[B
kB
iB
iB
dB
gB
hG�O�B
!zB
'�B
*�B
/�B
5�B
9B
BAB
IlB
M�B
O�B
U�B
Z�B
^�B
d
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.65 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417332016080714173320160807141733  AO  ARCAADJP                                                                    20150226221309    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221309  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221309  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141733  IP                  G�O�G�O�G�O�                