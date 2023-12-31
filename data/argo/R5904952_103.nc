CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:27Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190527  20181005190527  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               gA   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��o��t1   @��pFA�@1Ձ$�/�ct�j~�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      gA   B   B   @���@�  @���A   A>ffA`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B��B��B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B��B�  B�33B���B�  B�  B�  B�  B�33B�33B�ffB�ffB�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��C��C�  C��3C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C��3C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D �fDfD� D��D� D  D� D  D� D  D� D��D� D  D�fD  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD� D��Dy�D��D� D  D�fDfD� D��Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D,��D-y�D.  D.� D/  D/y�D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5�fD6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<�fD=fD=� D>  D>� D?  D?y�D?��D@y�DA  DA� DBfDB�fDC  DC� DD  DD� DE  DE� DFfDF� DF��DG� DH  DH� DH��DIy�DJ  DJ� DJ��DK� DL  DL� DMfDM� DM��DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DSy�DT  DT� DU  DUy�DV  DV� DW  DW� DX  DX� DYfDY� DZfDZ� D[  D[� D\  D\�fD]  D]� D^fD^� D_  D_� D`  D`� Da  Da�fDb  Db� DcfDc�fDdfDd�fDefDe� De��Dfy�Dg  Dg�fDh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dly�Dl��Dm� DnfDn� Dn��Doy�Dp  Dp� Dq  Dqy�Dr  Dr� DsfDs� Dt  Dt� Du  Du�fDv  Dv� Dw  Dw� Dw�3Dy� D�5qD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�
>@�p�A�A"�RAA�Ab�RA�\)A�\)A�\)A�\)A��\A�\)A�\)A�\)B �B�B�BG�B G�B(�B0�B8�B@�BH�BP�BX�B`�BizBp�Bx�B�#�B�W
B��=B�#�B�W
B�W
B�W
B�W
B��=B��=B��pB��pB�W
B�W
B�W
B�W
B�W
B�#�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�#�B�W
B�W
B�W
B��=C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�CtECv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��C��C��C�"�C�"�C��C��C��C��C��C��C��C��C�"�C��C��C�"�C��C��C��C��C�"�C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C�"�C�"�C��C��C��C�"�C�"�C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��D 
�D �GDGD��D{D��D
�D��D
�D��D
�D��D{D��D
�D�GD
�D��D	
�D	��D

�D
��D
�D��D
�D��D
�D��D
�D��DGD��D{D�{D{D��D
�D�GDGD��D{D�{D{D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��DGD��D
�D��D
�D��D 
�D ��D!
�D-{D-�{D.
�D.��D/
�D/�{D0
�D0��D1
�D1��D2
�D2��D3GD3��D4
�D4��D5
�D5�GD6
�D6�{D7
�D7��D8
�D8��D9
�D9��D:
�D:��D;
�D;�{D<
�D<�GD=GD=��D>
�D>��D?
�D?�{D@{D@�{DA
�DA��DBGDB�GDC
�DC��DD
�DD��DE
�DE��DFGDF��DG{DG��DH
�DH��DI{DI�{DJ
�DJ��DK{DK��DL
�DL��DMGDM��DN{DN��DO
�DO��DP
�DP��DQ
�DQ��DR{DR��DS
�DS�{DT
�DT��DU
�DU�{DV
�DV��DW
�DW��DX
�DX��DYGDY��DZGDZ��D[
�D[��D\
�D\�GD]
�D]��D^GD^��D_
�D_��D`
�D`��Da
�Da�GDb
�Db��DcGDc�GDdGDd�GDeGDe��Df{Df�{Dg
�Dg�GDh
�Dh��Di
�Di��Dj
�Dj�{Dk
�Dk��Dl
�Dl�{Dm{Dm��DnGDn��Do{Do�{Dp
�Dp��Dq
�Dq�{Dr
�Dr��DsGDs��Dt
�Dt��Du
�Du�GDv
�Dv��Dw
�Dw��Dw�Dy��D�:�D��41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�{A��A��A�&�A�(�A�(�A�&�A�+A�+A�-A�/A�1'A�/A�-A�-A�-A�/A�/A�1'A�5?A�7LA�5?A�5?A�5?A�7LA�7LA�;dA�9XA�;dA�=qA�=qA�/A�(�A�$�A�%A���A��
A�C�A�1A��AсA�+A��A���A�;dA��A�x�A�%A�O�A�?}A�dZA���A���A�t�A�z�A�7LA���A�|�A���A�p�A�  A�~�A��A��wA�ĜA�+A�-A��FA�?}A���A��A��RA�jA��-A�r�A�ƨA��7A�ƨA��FA�VA�A��FA���A��FA��RA�I�A��uA�1A���A�l�A�r�A���A�bA�^5A�
=A���A�
=A�x�A�r�A�`BA��A�$�A�A�~�A�n�A��HA~�uA|�!Ax5?At��AsC�Ao�#Aj�HAh��Ae/AbE�A_ƨA\��AZ1AX^5AU�-AR�RAQAOK�AN��AN��ANn�AKhsAJȴAH�AF�AEVAC\)AAK�A>Q�A;K�A9S�A7�A5�hA3�#A2ffA2JA1�^A0��A/7LA-�wA-%A+��A*(�A(��A&~�A%�A$��A$ffA#ƨA#�A"�yA"-A!��A!�A!K�A �yA {A��A�wAVA�wA��A=qAhsA��A5?AĜA��A�;A�7A|�A�`AI�A�/A^5A-A�^AG�Av�A�^A�AVAZA^5A^5A5?A��AC�A��AI�A�A�7A	x�A	?}A�Ar�At�AVA��A��Az�A�DA=qA��A ��A �!A n�@�n�@�I�@�j@���@�&�@��D@�j@���@�w@�ƨ@�\)@��@�5?@��;@�C�@�7L@�  @�C�@�=q@�/@�Z@��@�p�@�&�@�X@�@��@�@�b@�V@��@�7L@��@��@؛�@�+@�$�@�hs@�/@��@Լj@�K�@ҸR@�7L@Ϯ@�33@�v�@�x�@���@�1'@˾w@�l�@�$�@���@���@�@�n�@ź^@��`@å�@�=q@���@�p�@���@�z�@�1'@�t�@���@�M�@�{@���@�&�@�I�@��F@�K�@�33@�33@�o@�v�@�-@��@���@���@�`B@���@�Z@��@�K�@��@�o@��H@��!@��+@�M�@���@�I�@�A�@�  @��
@��P@�+@��@�ff@���@�7L@��/@���@��j@��j@�r�@�A�@�A�@�  @��@��@�K�@�K�@�K�@�dZ@�l�@�o@��@���@�V@�E�@���@��@��@�O�@��h@�`B@�7L@��D@�j@� �@��@�33@��@�J@��T@�@�p�@���@�V@��@��@�%@���@�Ĝ@���@���@���@��9@�Q�@�b@���@��;@�t�@�@���@��7@�G�@��@��/@��D@��@��w@��P@�o@�@�@�~�@�5?@�$�@��T@��-@�`B@�7L@�/@��@��@��@�%@��/@���@���@��u@�I�@� �@��@���@��@�l�@�K�@�"�@��@��H@���@���@��@�@��T@���@�@�X@���@�9X@�b@���@�l�@�33@��@�ȴ@���@�v�@�^5@�{@��-@�`B@���@��j@�z�@�I�@�A�@�(�@�b@��F@��@�+@�M�@��@��T@�V@��u@�Q�@��@���@���@�o@��y@��!@�-@���@�p�@�&�@���@��9@�Q�@���@��P@��@�@��y@�ȴ@�ff@�5?@�p�@�r�@�Z@�^5@yA @ij1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�{A��A��A�&�A�(�A�(�A�&�A�+A�+A�-A�/A�1'A�/A�-A�-A�-A�/A�/A�1'A�5?A�7LA�5?A�5?A�5?A�7LA�7LA�;dA�9XA�;dA�=qA�=qA�/A�(�A�$�A�%A���A��
A�C�A�1A��AсA�+A��A���A�;dA��A�x�A�%A�O�A�?}A�dZA���A���A�t�A�z�A�7LA���A�|�A���A�p�A�  A�~�A��A��wA�ĜA�+A�-A��FA�?}A���A��A��RA�jA��-A�r�A�ƨA��7A�ƨA��FA�VA�A��FA���A��FA��RA�I�A��uA�1A���A�l�A�r�A���A�bA�^5A�
=A���A�
=A�x�A�r�A�`BA��A�$�A�A�~�A�n�A��HA~�uA|�!Ax5?At��AsC�Ao�#Aj�HAh��Ae/AbE�A_ƨA\��AZ1AX^5AU�-AR�RAQAOK�AN��AN��ANn�AKhsAJȴAH�AF�AEVAC\)AAK�A>Q�A;K�A9S�A7�A5�hA3�#A2ffA2JA1�^A0��A/7LA-�wA-%A+��A*(�A(��A&~�A%�A$��A$ffA#ƨA#�A"�yA"-A!��A!�A!K�A �yA {A��A�wAVA�wA��A=qAhsA��A5?AĜA��A�;A�7A|�A�`AI�A�/A^5A-A�^AG�Av�A�^A�AVAZA^5A^5A5?A��AC�A��AI�A�A�7A	x�A	?}A�Ar�At�AVA��A��Az�A�DA=qA��A ��A �!A n�@�n�@�I�@�j@���@�&�@��D@�j@���@�w@�ƨ@�\)@��@�5?@��;@�C�@�7L@�  @�C�@�=q@�/@�Z@��@�p�@�&�@�X@�@��@�@�b@�V@��@�7L@��@��@؛�@�+@�$�@�hs@�/@��@Լj@�K�@ҸR@�7L@Ϯ@�33@�v�@�x�@���@�1'@˾w@�l�@�$�@���@���@�@�n�@ź^@��`@å�@�=q@���@�p�@���@�z�@�1'@�t�@���@�M�@�{@���@�&�@�I�@��F@�K�@�33@�33@�o@�v�@�-@��@���@���@�`B@���@�Z@��@�K�@��@�o@��H@��!@��+@�M�@���@�I�@�A�@�  @��
@��P@�+@��@�ff@���@�7L@��/@���@��j@��j@�r�@�A�@�A�@�  @��@��@�K�@�K�@�K�@�dZ@�l�@�o@��@���@�V@�E�@���@��@��@�O�@��h@�`B@�7L@��D@�j@� �@��@�33@��@�J@��T@�@�p�@���@�V@��@��@�%@���@�Ĝ@���@���@���@��9@�Q�@�b@���@��;@�t�@�@���@��7@�G�@��@��/@��D@��@��w@��P@�o@�@�@�~�@�5?@�$�@��T@��-@�`B@�7L@�/@��@��@��@�%@��/@���@���@��u@�I�@� �@��@���@��@�l�@�K�@�"�@��@��H@���@���@��@�@��T@���@�@�X@���@�9X@�b@���@�l�@�33@��@�ȴ@���@�v�@�^5@�{@��-@�`B@���@��j@�z�@�I�@�A�@�(�@�b@��F@��@�+@�M�@��@��T@�V@��u@�Q�@��@���@���@�o@��y@��!@�-@���@�p�@�&�@���@��9@�Q�@���@��P@��@�@��y@�ȴ@�ff@�5?@�p�@�r�@�Z@�^5@yA @ij1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
J�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
K�B
L�B
K�B
XB
bNB
ffB
jB
k�B
m�B
u�B
q�B
�B-B49BA�BW
BjB{�B�\B�B�'B�9B�FB�dB��B��B\BoB�B �B�BoBuBB��B��B
=B&�BA�B�B�B+B�B+B�BoB��B�yB�B�FB��B�oB��B��B�oB�bB�Bm�Be`BW
BB�B7LB/B'�B �B�B�B1B
�B
�B
ĜB
��B
�B
S�B
-B
�B
\B	��B	�B	ɺB	�?B	�DB	s�B	]/B	J�B	>wB	49B	)�B	)�B	!�B	�B	bB	JB	1B	+B	B��B��B�B�fB�BB�B��B��BĜB�qB�XB�?B�-B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�3B��BÖB�wB�}B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��BǮBǮB��B�B�B�B�B�B��B�B�BB�BB�#B�/B�sB�B�mB�HB�HB��BÖB�!B��B��B�B�RB�}B�wB�B��B��B�oB�oB�hB�VB�\B��B��B��B��B��B��B�B�?B�wBǮB��B��B��B��B��B��B��B�B�B�B�
B�B�)B�BB�HB�HB�NB�ZB�mB�yB�B�B�B�B��B��B��B��B	B	
=B	VB	\B	bB	oB	{B	{B	�B	�B	�B	 �B	 �B	$�B	)�B	-B	1'B	1'B	1'B	33B	9XB	=qB	?}B	@�B	@�B	@�B	C�B	H�B	L�B	R�B	ZB	[#B	]/B	_;B	`BB	bNB	ffB	ffB	iyB	l�B	m�B	n�B	p�B	s�B	v�B	z�B	~�B	� B	� B	K�B	��B	�B	�B	�B	�B	�!B	�'B	�9B	�LB	�RB	�^B	�qB	��B	��B	��B	��B	ÖB	ÖB	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�5B	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�TB	�TB	�TB	�TB	�ZB	�mB	�mB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
	7B

=B
DB
PB
PB
VB
PB
DB
DB
JB
JB
DB
DB
JB
PB
JB
JB
VB
\B
VB
VB
PB
VB
\B
bB
bB
bB
bB
hB
hB
bB
bB
bB
�B
�B
-2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
J�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
K�B
L�B
K�B
XB
bNB
ffB
jB
k�B
m�B
u�B
q�B
�B-B49BA�BW
BjB{�B�\B�B�'B�9B�FB�dB��B��B\BoB�B �B�BoBuBB��B��B
=B&�BA�B�B�B+B�B+B�BoB��B�yB�B�FB��B�oB��B��B�oB�bB�Bm�Be`BW
BB�B7LB/B'�B �B�B�B1B
�B
�B
ĜB
��B
�B
S�B
-B
�B
\B	��B	�B	ɺB	�?B	�DB	s�B	]/B	J�B	>wB	49B	)�B	)�B	!�B	�B	bB	JB	1B	+B	B��B��B�B�fB�BB�B��B��BĜB�qB�XB�?B�-B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�3B��BÖB�wB�}B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��BǮBǮB��B�B�B�B�B�B��B�B�BB�BB�#B�/B�sB�B�mB�HB�HB��BÖB�!B��B��B�B�RB�}B�wB�B��B��B�oB�oB�hB�VB�\B��B��B��B��B��B��B�B�?B�wBǮB��B��B��B��B��B��B��B�B�B�B�
B�B�)B�BB�HB�HB�NB�ZB�mB�yB�B�B�B�B��B��B��B��B	B	
=B	VB	\B	bB	oB	{B	{B	�B	�B	�B	 �B	 �B	$�B	)�B	-B	1'B	1'B	1'B	33B	9XB	=qB	?}B	@�B	@�B	@�B	C�B	H�B	L�B	R�B	ZB	[#B	]/B	_;B	`BB	bNB	ffB	ffB	iyB	l�B	m�B	n�B	p�B	s�B	v�B	z�B	~�B	� B	� B	K�B	��B	�B	�B	�B	�B	�!B	�'B	�9B	�LB	�RB	�^B	�qB	��B	��B	��B	��B	ÖB	ÖB	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�5B	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�TB	�TB	�TB	�TB	�ZB	�mB	�mB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
	7B

=B
DB
PB
PB
VB
PB
DB
DB
JB
JB
DB
DB
JB
PB
JB
JB
VB
\B
VB
VB
PB
VB
\B
bB
bB
bB
bB
hB
hB
bB
bB
bB
�B
�B
-2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190527                              AO  ARCAADJP                                                                    20181005190527    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190527  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190527  QCF$                G�O�G�O�G�O�C000            