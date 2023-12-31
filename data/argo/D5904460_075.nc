CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-24T19:17:35Z AOML 3.0 creation; 2016-08-07T21:17:41Z UW 3.1 conversion     
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
_FillValue                 �  A|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ct   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
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
resolution        :�o     �  z|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150924191735  20160807141741  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               KA   AO  5285_8895_075                   2C  D   APEX                            6487                            072314                          846 @�q��{1   @�q��DC@,��E��c��hr�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    KA   B   B   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B/��B8ffB?33BG��BP  BX  B`  Bh��Bo��Bx  B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy� D�fD�S3D���D��3D��D�\�D���D��fD�fD�VfD�p Dǰ D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\��@���@���A
ffA*ffAJffAjffA�33A�33A�33A�33A�33A�33A�33A�33B��B
��B��BfgB"��B*��B234B;  BA��BJ34BR��BZ��Bb��BkfgBr34Bz��B�L�B�L�B�L�B�L�B�L�B�� B�L�B��B��B�L�B�L�B�L�B�L�B�L�B�L�B�� B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C �fC�fC�fC�fC�fC
�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC �fC"�fC$�fC&�fC(�fC*� C,� C.�fC0�fC2�fC4�fC6�fC8�fC:�fC<�fC>�fC@�fCB�fCD�fCF�fCH�fCJ�fCL�fCN�fCP�fCR�fCT�fCV�fCX�fCZ�fC\�fC^�fC`�fCb�fCd�fCf�fCh�fCj�fCl�fCn�fCp�fCr�fCt�fCv�fCx�fCz�fC|�fC~�fC�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�` C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3D )�D ��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D	)�D	��D
)�D
��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D )�D ��D!)�D!��D")�D"��D#)�D#��D$)�D$��D%)�D%��D&)�D&��D')�D'��D()�D(��D))�D)��D*)�D*��D+)�D+��D,)�D,��D-)�D-��D.)�D.��D/)�D/��D0)�D0��D1)�D1��D2)�D2��D3)�D3��D4)�D4��D5)�D5��D6)�D6��D7)�D7��D8)�D8��D9)�D9��D:)�D:��D;)�D;��D<)�D<��D=)�D=��D>)�D>��D?)�D?��D@)�D@��DA)�DA��DB)�DB��DC)�DC��DD)�DD��DE)�DE��DF)�DF��DG)�DG��DH)�DH��DI)�DI��DJ)�DJ��DK)�DK��DL)�DL��DM)�DM��DN)�DN��DO)�DO��DP)�DP��DQ)�DQ��DR)�DR��DS)�DS��DT)�DT��DU)�DU��DV)�DV��DW0 DW��DX)�DX��DY)�DY��DZ)�DZ��D[)�D[��D\)�D\��D])�D]��D^)�D^��D_)�D_��D`)�D`��Da)�Da��Db)�Db��Dc)�Dc��Dd)�Dd��De)�De��Df)�Df��Dg)�Dg��Dh)�Dh��Di)�Di��Dj)�Dj��Dk)�Dk��Dl)�Dl��Dm)�Dm��Dn)�Dn��Do)�Do��Dp)�Dp��Dq)�Dq��Dr)�Dr��Ds)�Ds��Dt)�Dt��Dy��D�+3D�h D��gD�� D�!�D�q�D��gD��3D�3D�k3D���D���D�g11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ZA�dZA�hsA�jA�jA�jA�n�A�z�A�~�A�A闍A��A��A��A镁A�+A�S�A��A�5?A�A�z�A�K�A��mA� �A�$�AׁA��AՕ�Aљ�A�dZA��mA�{A�9XAɕ�Aȴ9A��Aǩ�A��mA��;Aß�A�1A��A��A�A���A��-A�C�A��RA��hA�7LA�?}A�A�ȴA�JA��hA�33A��DA�%A���A�ZA���A���A�9XA��jA���A���A�x�A��wA�K�A���A��mA��RA�A��A�A�{A��yA~�Ay�Aw�Aut�Ar^5AnjAi�PAeAbȴA_�A[��AYS�ASp�APjAM�7AJ�yAI�AD��ABz�A>9XA< �A:-A81'A6��A5��A6-A5��A5;dA4��A3�mA3K�A2�A2��A2n�A2M�A1�FA0ĜA/��A/dZA.��A.�A-t�A,�yA+�TA+`BA+&�A*�jA*r�A*�A)ƨA)33A(Q�A'p�A';dA'�A&�uA&-A%XA$�!A$�/A$ZA"�A!\)A �9A ��A\)At�A(�A��A -A ZA E�A�A��A��A�TA�AƨAdZAoAA�A�jA1'A�
AQ�A��A`BA(�A�AffA��A�TA�-A%A~�A�A�7A�A�/An�AJA�^A�wAA�^A��A�FA�wA7LA�+A�TA7LA�`A�uAffA(�A�AO�AM�A��A�TA��A��A��A��AS�A%A
�RA
r�A	ƨA	G�A��A��A��A=qA�wA�A��A~�A��A�FA�^AK�A%A�\A��A��A��A&�A�uAVAA�A$�A�A�-A�hA�A;dA �RA M�@��@�"�@�~�@��@�?}@�Z@�C�@��#@���@��@��9@���@��!@��h@�%@�;d@�v�@�x�@�b@�n�@�O�@웦@�9X@�1@�P@�X@�j@�t�@柾@��@�V@�Q�@�F@�R@�@��@�(�@�  @��m@��y@��#@�?}@ܴ9@�9X@۶F@�
=@�v�@��@�7L@���@���@׶F@��@ղ-@�J@��#@���@���@�33@���@ҟ�@���@с@щ7@���@д9@ύP@�{@�hs@�x�@�?}@���@��m@�+@�@���@ʰ!@�v�@ɑh@�z�@�bN@�Z@�Z@�A�@�1@�S�@Ə\@�ff@�^5@���@���@���@���@���@��H@�J@��h@�p�@�V@��@���@�\)@�C�@�33@���@�M�@���@�G�@��`@�Ĝ@���@��@�I�@���@�l�@�33@�+@�@�=q@�@�V@�1@��@�
=@�n�@�=q@�@��@���@��@�A�@��
@��@�"�@��H@�M�@���@��@���@���@��`@���@��j@���@��D@�9X@���@��@�
=@���@���@�~�@�^5@�=q@��@�@��-@�j@�b@���@��
@��F@�|�@�33@�^5@��@��@���@�%@�(�@��P@�+@���@�^5@���@���@��7@��9@�I�@�(�@��;@�|�@�dZ@�C�@�o@��y@�ȴ@���@�=q@�@�@�/@��/@�z�@�Z@�9X@��@��F@�dZ@��@�v�@���@���@��@�G�@��@��j@�bN@��;@��P@�|�@�S�@�33@�"�@�
=@�ȴ@�~�@�=q@��-@�O�@�&�@�%@�Ĝ@�9X@��
@��F@�t�@�33@��@��R@��+@�=q@���@���@�X@�&�@��`@�Ĝ@��@�I�@� �@��@���@�t�@�dZ@�\)@�33@��@�
=@�ȴ@���@�~�@�Q�@��@���@}�@v�+@l9X@a��@XQ�@P  @G�w@A%@:~�@3"�@,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 A�ZA�dZA�hsA�jA�jA�jA�n�A�z�A�~�A�A闍A��A��A��A镁A�+A�S�A��A�5?A�A�z�A�K�A��mA� �A�$�AׁA��AՕ�Aљ�A�dZA��mA�{A�9XAɕ�Aȴ9A��Aǩ�A��mA��;Aß�A�1A��A��A�A���A��-A�C�A��RA��hA�7LA�?}A�A�ȴA�JA��hA�33A��DA�%A���A�ZA���A���A�9XA��jA���A���A�x�A��wA�K�A���A��mA��RA�A��A�A�{A��yA~�Ay�Aw�Aut�Ar^5AnjAi�PAeAbȴA_�A[��AYS�ASp�APjAM�7AJ�yAI�AD��ABz�A>9XA< �A:-A81'A6��A5��A6-A5��A5;dA4��A3�mA3K�A2�A2��A2n�A2M�A1�FA0ĜA/��A/dZA.��A.�A-t�A,�yA+�TA+`BA+&�A*�jA*r�A*�A)ƨA)33A(Q�A'p�A';dA'�A&�uA&-A%XA$�!A$�/A$ZA"�A!\)A �9A ��A\)At�A(�A��A -A ZA E�A�A��A��A�TA�AƨAdZAoAA�A�jA1'A�
AQ�A��A`BA(�A�AffA��A�TA�-A%A~�A�A�7A�A�/An�AJA�^A�wAA�^A��A�FA�wA7LA�+A�TA7LA�`A�uAffA(�A�AO�AM�A��A�TA��A��A��A��AS�A%A
�RA
r�A	ƨA	G�A��A��A��A=qA�wA�A��A~�A��A�FA�^AK�A%A�\A��A��A��A&�A�uAVAA�A$�A�A�-A�hA�A;dA �RA M�@��@�"�@�~�@��@�?}@�Z@�C�@��#@���@��@��9@���@��!@��h@�%@�;d@�v�@�x�@�b@�n�@�O�@웦@�9X@�1@�P@�X@�j@�t�@柾@��@�V@�Q�@�F@�R@�@��@�(�@�  @��m@��y@��#@�?}@ܴ9@�9X@۶F@�
=@�v�@��@�7L@���@���@׶F@��@ղ-@�J@��#@���@���@�33@���@ҟ�@���@с@щ7@���@д9@ύP@�{@�hs@�x�@�?}@���@��m@�+@�@���@ʰ!@�v�@ɑh@�z�@�bN@�Z@�Z@�A�@�1@�S�@Ə\@�ff@�^5@���@���@���@���@���@��H@�J@��h@�p�@�V@��@���@�\)@�C�@�33@���@�M�@���@�G�@��`@�Ĝ@���@��@�I�@���@�l�@�33@�+@�@�=q@�@�V@�1@��@�
=@�n�@�=q@�@��@���@��@�A�@��
@��@�"�@��H@�M�@���@��@���@���@��`@���@��j@���@��D@�9X@���@��@�
=@���@���@�~�@�^5@�=q@��@�@��-@�j@�b@���@��
@��F@�|�@�33@�^5@��@��@���@�%@�(�@��P@�+@���@�^5@���@���@��7@��9@�I�@�(�@��;@�|�@�dZ@�C�@�o@��y@�ȴ@���@�=q@�@�@�/@��/@�z�@�Z@�9X@��@��F@�dZ@��@�v�@���@���@��@�G�@��@��j@�bN@��;@��P@�|�@�S�@�33@�"�@�
=@�ȴ@�~�@�=q@��-@�O�@�&�@�%@�Ĝ@�9X@��
@��F@�t�@�33@��@��R@��+@�=q@���@���@�X@�&�@��`@�Ĝ@��@�I�@� �@��@���@�t�@�dZ@�\)@�33@��@�
=@�ȴ@���G�O�@�Q�@��@���@}�@v�+@l9X@a��@XQ�@P  @G�w@A%@:~�@3"�@,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B��B��B��B	+B	{B	�B	#�B	'�B	(�B	,B	:^B	{�B	�wB	��B	�/B
VB
-B
A�B
_;B
�VB
��B+BT�B�1B�^B�BDB1'BN�BVBffBw�B�B�JB�7B�=B�B�fB��B�BhB�^B�jB�B�B��BBBB��B�B�NBB��B�uB{�B`BBG�B'�B��B�B�jB��B�=BaHBuB
��B
��B
�B
�B	�wB	�uB	�1B	�B	o�B	S�B	;dB	,B	$�B	�B	+B��B�B�NB�/B�)B�
B�)B�B�5B�fB�B�B�B��B	  B	"�B	,B	0!B	J�B	W
B	bNB	dZB	k�B	y�B	�B	��B	��B	��B	�3B	�jB	ÖB	B	��B	B	ƨB	��B	��B	��B	�)B	�BB	�;B	�B	�#B	�TB	�B	�B	��B	��B
B
B	��B	�B	�B	��B	�B	�TB	��B
�B
#�B
%�B
)�B
)�B
&�B
%�B
&�B
)�B
)�B
,B
'�B
�B
�B
�B
�B
-B
/B
+B
#�B
 �B
�B
�B
#�B
'�B
%�B
#�B
#�B
"�B
 �B
 �B
!�B
!�B
"�B
"�B
"�B
#�B
$�B
&�B
-B
/B
.B
+B
'�B
'�B
'�B
'�B
&�B
%�B
#�B
 �B
 �B
'�B
-B
1'B
1'B
1'B
1'B
0!B
/B
.B
-B
+B
+B
+B
)�B
'�B
#�B
!�B
"�B
#�B
 �B
"�B
&�B
&�B
$�B
!�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
hB
hB
bB
\B
PB

=B

=B
	7B
+B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
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
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
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

=B
DB
DB
DB
DB
JB
JB
JB
JB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
\B
\B
\B
bB
bB
hB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
#�B
)�B
.B
33B
7LB
>wB
E�B
M�B
Q�B
YB
^5B
aHB
e`B
hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 B�B�B�B�B�B�B�B��B��B��B	B	_B	�B	#�B	'�B	(�B	+�B	:AB	{�B	�UB	��B	�	B
-B
,�B
A_B
_B
�'B
�aB�BT�B��B�,B�iBB0�BN�BU�Bf4Bw�B��B�B��B�B��B�1B��BIB1B�(B�3B��B�QB��B�B �B�B��B�|B�B�TB��B�9B{�B`BGoB'�B��B��B�-B�oB��BaB8B
��B
ѭB
��B
eB	�@B	�AB	��B	��B	ogB	S�B	;1B	+�B	$�B	QB	�B��B�JB�B��B��B��B��B��B� B�.B�PB�FB�zB�B��B	"�B	+�B	/�B	J�B	V�B	bB	dB	kHB	y�B	��B	�BB	��B	��B	��B	�(B	�SB	�OB	�FB	�NB	�fB	̉B	ΗB	ѨB	��B	� B	��B	��B	��B	�B	�<B	�`B	�xB	��B
�B
�B	�B	�AB	�pB	��B	�MB	�B	��B
WB
#�B
%�B
)�B
)�B
&�B
%�B
&�B
)�B
)�B
+�B
'�B
yB
UB
XB
aB
,�B
.�B
*�B
#�B
 ~B
UB
SB
#�B
'�B
%�B
#�B
#�B
"�B
 }B
 B
!�B
!�B
"�B
"�B
"�B
#�B
$�B
&�B
,�B
.�B
-�B
*�B
'�B
'�B
'�B
'�B
&�B
%�B
#�B
 }B
 |B
'�B
,�B
0�B
0�B
0�B
0�B
/�B
.�B
-�B
,�B
*�B
*�B
*�B
)�B
'�B
#�B
!B
"�B
#�B
 zB
"�B
&�B
&�B
$�B
!�B
!�B
#�B
"�B
tB
nB
kB
kB
iB
nB
vB
sB
oB
kB
bB
YB
IB
BB
DB
?B
3B
-B
&B
B
B
B
B
B
	�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�zB	�mB	�cB	�wB	�jB	�RB	�WB	�rB	�yB	�jB	�ZB	�XB	�UB	�SB	�aB	�jB	�}B	�~B	��B	�}B	�jB	�jB	�qB	�|B	�vB	�vB	�wB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�}B	�}B	�wB	�pB	�jB	�jB	�nB	�pB	�jB	�iB	�pB	�wB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
	B
B
B
B
B
	B
	B
B
B
B
B
B
B
B
 B
!B
"B
'B
'B
'B
&B
-B
,B
,B
3B
2B
:B
:B
:B
9B
6B
?B
EB
EB
DB
EB
DB
EB
MB
OB
RB
PB
YB
^B
]B
]B
]B
^B
\B
\B
]B
\B
[B
YB
WB
VB
WB
XB
]B
]B
\B
\B
\B
cB
gB
iB
oB
oB
mB
 wB
 wG�O�B
#�B
)�B
-�B
2�B
6�B
>&B
EQB
M�B
Q�B
X�B
]�B
`�B
eB
h#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.65 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417412016080714174120160807141741  AO  ARCAADJP                                                                    20150924191735    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150924191735  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150924191735  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141741  IP                  G�O�G�O�G�O�                