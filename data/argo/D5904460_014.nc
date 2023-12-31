CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:12:51Z AOML 3.0 creation; 2016-08-07T21:17:30Z UW 3.1 conversion     
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20150226221251  20160807141731  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_014                   2C  D   APEX                            6487                            072314                          846 @�"���1   @�">���@,d�/���c͉7Kƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp��Bw��B��B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  DuFfDy�fD���D�@ D�� D���D�  D�C3D�I�D�� D�fD�FfD�vfDǰ D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @���A
ffA*ffAJffAjffA�33A�33A�33A�33A�33A�33A�33A�33B��B
��B��B��B"��B*��B2��B:��BB��BJ��BR��BZ��Bb��Bj��BsfgBz34B��B�L�B�L�B�L�B�L�B�� B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C �fC�fC�fC�fC�fC
�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC �fC"�fC$�fC&�fC(�fC*�fC,�fC.�fC0�fC2�fC4�fC6�fC8�fC:�fC<�fC>�fC@�fCB�fCD�fCF�fCH�fCJ�fCL�fCN�fCP�fCR�fCT�fCV�fCX�fCZ�fC\�fC^�fC`�fCb�fCd�fCf�fCh�fCj�fCl�fCn�fCp�fCr�fCt�fCv�fCx�fCz�fC|�fC~�fC�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�` C�` C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3D )�D ��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D	)�D	��D
)�D
��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D )�D ��D!)�D!��D")�D"��D#)�D#��D$)�D$��D%)�D%��D&)�D&��D')�D'��D()�D(��D))�D)��D*)�D*��D+)�D+��D,)�D,��D-)�D-��D.)�D.��D/)�D/��D0)�D0��D1)�D1��D2)�D2��D3)�D3��D4)�D4��D5)�D5��D6)�D6��D7)�D7��D8)�D8��D9)�D9��D:)�D:��D;)�D;��D<)�D<��D=)�D=��D>)�D>��D?)�D?��D@)�D@��DA)�DA��DB)�DB��DC)�DC��DD)�DD��DE)�DE��DF)�DF��DG)�DG��DH)�DH��DI)�DI��DJ)�DJ��DK)�DK��DL)�DL��DM)�DM��DN)�DN��DO)�DO��DP)�DP��DQ)�DQ��DR)�DR��DS)�DS��DT)�DT��DU)�DU��DV)�DV��DW)�DW��DX)�DX��DY)�DY��DZ)�DZ��D[)�D[��D\)�D\��D])�D]��D^)�D^��D_)�D_��D`)�D`��Da)�Da��Db)�Db��Dc)�Dc��Dd)�Dd��De)�De��Df)�Df��Dg)�Dg��Dh)�Dh��Di)�Di��Dj)�Dj��Dk)�Dk��Dl)�Dl��Dm)�Dm��Dn)�Dn��Do)�Do��Dp)�Dp��Dq)�Dq��Dr)�Dr��Ds)�Ds��Dt)�Dt��Du)�Dup Dy� D�gD�T�D���D�њD�4�D�X D�^gD���D�+3D�[3D��3D���D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�33A�7LA� �A�|�A�$�A��A�{A�bA�
=A�
=A�%A�A�A�  A���A���A���A���A���A���A���A���A��A��A��A��yA���A��A�1'A���A�l�A�5?A�A�A�O�A��RA�ZA��A�O�A���A�"�A���A�x�A���A�-A���A�=qA��wA���A�S�A�{A�ĜA�|�A�ƨA��HA��A��A��uA��/A�oA�7LA��/A��A��;A���A�1'A��A��DA���A�(�A��A�ZA��A��A�I�A�E�A�VA��PA��A|^5AwoAqC�Aj��Af�jA`{A[�A[%AY|�AV��AO\)AN1AJJAE"�AD(�ACp�AAA>z�A<��A9|�A81'A6 �A4��A3p�A3��A3�wA3G�A2bA/C�A.9XA-��A,{A*��A)�A(I�A&�+A$n�A"�/A!oA ��A A�A A!�PA#�A$�\A#G�A"-A!ƨA!�hA �A bAx�A7LA?}A�7A�jAjA(�A��Ax�AS�A7LA�A�\A�A�A�RAz�AM�A5?A1A�A��AA�Ax�AA�!Ar�A5?A�A�HA~�AI�A�A��A��A�9AM�A�A�;At�A�A��A��A+AVA�A��A5?A{AƨA/A�9AffA�;A;dA
��A
�9A	��A	t�A�A�DA$�A��AhsA7LA^5AVA��A��A~�AbNA�;A33A�Az�A1A�A �RA �\A bN@���@���A 5?A �@��@���@��-@��@���@�x�@���@�S�@�33@���@���@��@��@��+@��^@���@��@�M�@���@��@��;@���@��m@�t�@�$�@�hs@��@���@�`B@��`@�z�@��
@�P@�C�@�!@�@�G�@�7L@�5?@�r�@睲@�P@���@��;@�t�@�K�@�F@�|�@�|�@�^5@�9@�(�@���@�u@��;@�V@�/@�z�@�|�@��@�ȴ@�^5@ݙ�@���@۾w@�ȴ@�-@٩�@ؓu@ץ�@���@�v�@պ^@�%@ԓu@�j@���@�33@҇+@Ѻ^@�V@���@�r�@��@ϕ�@��y@�~�@ͩ�@�/@̼j@̋D@�A�@�b@�ƨ@˕�@�l�@��y@ʗ�@�E�@ɡ�@�X@�%@�bN@��@��;@ǥ�@�l�@�;d@�o@��y@Ɵ�@ř�@���@Ĵ9@ċD@�(�@î@�33@�{@�X@��@�  @��!@��@�`B@�%@�9X@��@��F@�dZ@��@��\@�n�@���@�7L@��j@�j@� �@���@���@�C�@�ȴ@�E�@���@���@��h@�hs@�?}@�/@��@��;@���@�dZ@�V@�$�@���@�@�?}@���@��@��@��@�;d@���@���@�^5@�$�@�{@���@��@���@�j@���@�dZ@�
=@�ȴ@��!@�v�@�5?@�@�@���@��7@�O�@�7L@��@��/@�Ĝ@��@�9X@�1'@�(�@� �@���@��w@���@�S�@�33@�@���@��y@���@�5?@��@���@��`@�z�@�bN@�I�@�(�@�ƨ@�33@���@��H@�ȴ@��!@�ff@�-@��@�@���@��@��@���@�Z@�1@���@���@��@�t�@�\)@���@��\@�M�@��@��#@�x�@�7L@���@�r�@��m@��F@���@��@�dZ@�o@�~�@�=q@�{@��T@��^@��h@�%@��u@�b@�|�@�+@��y@�n�@���@���@��@�O�@��`@��@�j@�1'@��@�dZ@�C�@�o@���@�{@��-@�p�@�O�@�C�@�;d@��@~�R@t�j@ihs@]��@VV@Ol�@K@A�^@;��@5��@0�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111A�33A�7LA� �A�|�A�$�A��A�{A�bA�
=A�
=A�%A�A�A�  A���A���A���A���A���A���A���A���A��A��A��A��yA���A��A�1'A���A�l�A�5?A�A�A�O�A��RA�ZA��A�O�A���A�"�A���A�x�A���A�-A���A�=qA��wA���A�S�A�{A�ĜA�|�A�ƨA��HA��A��A��uA��/A�oA�7LA��/A��A��;A���A�1'A��A��DA���A�(�A��A�ZA��A��A�I�A�E�A�VA��PA��A|^5AwoAqC�Aj��Af�jA`{A[�A[%AY|�AV��AO\)AN1AJJAE"�AD(�ACp�AAA>z�A<��A9|�A81'A6 �A4��A3p�A3��A3�wA3G�A2bA/C�A.9XA-��A,{A*��A)�A(I�A&�+A$n�A"�/A!oA ��A A�A A!�PA#�A$�\A#G�A"-A!ƨA!�hA �A bAx�A7LA?}A�7A�jAjA(�A��Ax�AS�A7LA�A�\A�A�A�RAz�AM�A5?A1A�A��AA�Ax�AA�!Ar�A5?A�A�HA~�AI�A�A��A��A�9AM�A�A�;At�A�A��A��A+AVA�A��A5?A{AƨA/A�9AffA�;A;dA
��A
�9A	��A	t�A�A�DA$�A��AhsA7LA^5AVA��A��A~�AbNA�;A33A�Az�A1A�A �RA �\A bN@���@���A 5?A �@��@���@��-@��@���@�x�@���@�S�@�33@���@���@��@��@��+@��^@���@��@�M�@���@��@��;@���@��m@�t�@�$�@�hs@��@���@�`B@��`@�z�@��
@�P@�C�@�!@�@�G�@�7L@�5?@�r�@睲@�P@���@��;@�t�@�K�@�F@�|�@�|�@�^5@�9@�(�@���@�u@��;@�V@�/@�z�@�|�@��@�ȴ@�^5@ݙ�@���@۾w@�ȴ@�-@٩�@ؓu@ץ�@���@�v�@պ^@�%@ԓu@�j@���@�33@҇+@Ѻ^@�V@���@�r�@��@ϕ�@��y@�~�@ͩ�@�/@̼j@̋D@�A�@�b@�ƨ@˕�@�l�@��y@ʗ�@�E�@ɡ�@�X@�%@�bN@��@��;@ǥ�@�l�@�;d@�o@��y@Ɵ�@ř�@���@Ĵ9@ċD@�(�@î@�33@�{@�X@��@�  @��!@��@�`B@�%@�9X@��@��F@�dZ@��@��\@�n�@���@�7L@��j@�j@� �@���@���@�C�@�ȴ@�E�@���@���@��h@�hs@�?}@�/@��@��;@���@�dZ@�V@�$�@���@�@�?}@���@��@��@��@�;d@���@���@�^5@�$�@�{@���@��@���@�j@���@�dZ@�
=@�ȴ@��!@�v�@�5?@�@�@���@��7@�O�@�7L@��@��/@�Ĝ@��@�9X@�1'@�(�@� �@���@��w@���@�S�@�33@�@���@��y@���@�5?@��@���@��`@�z�@�bN@�I�@�(�@�ƨ@�33@���@��H@�ȴ@��!@�ff@�-@��@�@���@��@��@���@�Z@�1@���@���@��@�t�@�\)@���@��\@�M�@��@��#@�x�@�7L@���@�r�@��m@��F@���@��@�dZ@�o@�~�@�=q@�{@��T@��^@��h@�%@��u@�b@�|�@�+@��y@�n�@���@���@��@�O�@��`@��@�j@�1'@��@�dZ@�C�@�o@���@�{@��-@�p�G�O�@�C�@�;d@��@~�R@t�j@ihs@]��@VV@Ol�@K@A�^@;��@5��@0�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�+B�+B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�+B�\B��B�!B�3B�9B�B��B�-B�wB�5B	7B�B)�B1'B'�B"�B)�B6FBB�B�%B��BƨB��B�B��B�qB��B��B��B��B�=B{�B^5BR�BXB?}B+B�B	7B�TBm�B%�B&�BVB
��B
�fB
��B
�B
�bB
x�B
gmB
VB
H�B
5?B
B	��B	�'B	�%B	e`B	C�B	.B	&�B	�B		7B�yB�BB��B��B��B��BƨBȴBŢB��B�wBÖB�5B�B	B	
=B	\B	\B	\B	�B	�B	�B	�B	�B	�B	�B	oB	�B	uB	�B	(�B	+B	T�B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�3B	ĜB	B	��B	ɺB	��B	�
B	�B	�/B	�HB	�ZB	�B	�B	�B	��B	��B
  B
  B
B
B
B
1B
+B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
%B
	7B

=B
	7B
	7B
1B
%B
B	��B
  B
  B
B
B
  B	��B
B
  B
B
  B	��B	��B	��B	��B	��B
B	��B	��B	��B	��B	�B	�B	�yB	�sB	�B	�B	�B	�B	��B	�B	�mB	�5B	�
B	�5B	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
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
%B
%B
%B
%B
%B
%B
+B
+B
+B
%B
%B
+B
+B
+B
1B
1B
1B
+B
1B
1B
+B
1B
1B
	7B
	7B

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
DB
JB
PB
PB
PB
PB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
+B
49B
:^B
B�B
H�B
N�B
Q�B
XB
[#B
`BB
dZB
gm111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�)B��B��B��B�B��B��B��B�CB��B	BNB)�B0�B'�B"�B)�B6BBXB��BѵB�pB��B��BʆB�6B��B��B�wB�KB�B{�B]�BR�BW�B?@B*�BmB�B�BmUB%�B&�BB
��B
�*B
ΚB
��B
�$B
x�B
g/B
U�B
H{B
5B
�B	��B	��B	��B	e)B	C^B	-�B	&�B	qB		 B�FB�B��BϫB͠BʏB�sB�~B�mB�LB�@B�aB��B�qB	 �B	
B	"B	"B	!B	RB	lB	|B	|B	jB	{B	^B	1B	BB	7B	cB	(�B	*�B	T�B	�B	��B	�wB	�^B	�cB	�eB	�XB	�cB	�kB	��B	��B	�YB	�LB	�EB	�wB	үB	��B	��B	��B	�B	�B	��B	�:B	�dB	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
�B
�B
�B
�B
 �B	��B	��B	��B
�B
�B	��B	��B
 �B	��B
 �B	��B	��B	��B	�yB	��B	��B
 �B	��B	��B	��B	�tB	�VB	�7B	�3B	�)B	�7B	�5B	�>B	�mB	��B	�oB	�$B	��B	��B	��B	�*B	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�mB	�eB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�xB	�aB	�YB	�eB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�~B	�wB	�qB	�jB	�dB	�`B	�`B	�bB	�^B	�dB	�aB	�aB	�`B	�^B	�dB	�cB	�kB	�lB	�kB	�kB	�kB	�dB	�dB	�kB	�rB	�pB	�sB	�qB	�oB	�qB	�qB	�rB	�pB	�pB	�rB	�oB	�qB	�rB	�vB	�|B	�}B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�wB	�qB	�qB	�jB	�jB	�rB	�pB	�pB	�wB	�tB	�yB	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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

�B
	�B

�B

�B

�B
�B
B
B
B
�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
 B
!B
!B
"B
"B
B
)B
)B
(B
'B
(B
,B
-B
+B
5B
2B
3B
3B
4B
3B
6B
:B
?B
>B
?B
=B
;B
EB
EB
FB
JB
LB
JB
QB
QB
RB
PB
GB
QB
QB
RB
YB
\B
\B
WB
\B
^B
dB
dG�O�B
gB
"�B
*�B
3�B
:B
B>B
HcB
N�B
Q�B
W�B
Z�B
_�B
dB
g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.65 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417312016080714173120160807141731  AO  ARCAADJP                                                                    20150226221251    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221251  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221251  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141731  IP                  G�O�G�O�G�O�                