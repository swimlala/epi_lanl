CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:51Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005191751  20181005191751  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$z+w1   @��%�mP@5E�����d|z�G�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @333@�  @�  A   AffA@  A`  A�  A�  A�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C�fC�fC  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC_�fCa�fCd  Ce�fCg�fCj  Cl  Cm�fCo�fCq�fCs�fCv  Cx  Cz  C|�C~�C��C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C��3C��3C�  C�  C��C��C�  C��3C�  C�  C�  C�  C��C��C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3D   D �fD  D� D  Dy�D��Dy�D  D� DfD�fDfD� D��D� DfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  Dy�D  D�fDfD�fDfD�fDfD�fD  Dy�D  Dy�D  D� D��Dy�D  D� D��Dy�D��D� D  Dy�D  D�fD  D� D��D� DfD� D   D � D!  D!� D"  D"� D#fD#��D$fD$�fD%fD%�fD&fD&�fD'fD'�fD(�D(� D(��D)� D)��D*� D+fD+� D+��D,� D-  D-�fD.  D.y�D.��D/� D0fD0��D1  D1y�D1��D2� D3  D3y�D4  D4�fD5  D5y�D5��D6� D7  D7� D8  D8� D8��D9y�D:  D:�fD;  D;y�D<  D<�fD=fD=�fD>fD>�fD?  D?y�D?��D@y�D@��DA� DB  DB� DC  DC�fDDfDD�fDE  DEy�DE��DFy�DG  DG� DH  DH�fDIfDI� DJ  DJ�fDK  DKs3DK��DL�fDM�DM� DM��DN� DO  DO� DPfDP� DQfDQ� DR  DR� DS  DS� DT  DTy�DU  DU�fDVfDV�fDW  DW� DX  DX�fDY  DYy�DZ  DZ� D[fD[y�D[��D\y�D]  D]� D^  D^� D_  D_� D`fD`� Da  Da� Da��Db� Db��Dc� Dd  Ddy�De  De� De��Df� Dg  Dgy�Dh  Dhy�Di  Di� DjfDj�fDk  Dky�DlfDl� DmfDm� Dm��Dn� Dn��Do� DpfDp� Dq  Dq� Dr  Dr�fDr��Ds� DtfDt� Dt��Du�fDv  Dv� Dw  Dw� Dw��DxY�D�G�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@G�@�=q@�=qA�A#�AE�Ae�A��\A��\A��\B���B���B���B���B���B���B�p�B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C 8RC8RC8RCQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�Ck�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^8RC`8RCb8RCdQ�Cf8RCh8RCjQ�ClQ�Cn8RCp8RCr8RCt8RCvQ�CxQ�CzQ�C|k�C~k�C�5�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�)C�)C�(�C�(�C�5�C�5�C�(�C�)C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�)C�)D {D ��D{D�{D{D�DD�D{D�{D�D��D�D�{DD�{D�D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�D{D��D�D��D�D��D�D��D{D�D{D�D{D�{DD�D{D�{DD�DD�{D{D�D{D��D{D�{DD�{D�D�{D {D �{D!{D!�{D"{D"�{D#�D#�HD$�D$��D%�D%��D&�D&��D'�D'��D(!HD(�{D)D)�{D*D*�{D+�D+�{D,D,�{D-{D-��D.{D.�D/D/�{D0�D0�HD1{D1�D2D2�{D3{D3�D4{D4��D5{D5�D6D6�{D7{D7�{D8{D8�{D9D9�D:{D:��D;{D;�D<{D<��D=�D=��D>�D>��D?{D?�D@D@�DADA�{DB{DB�{DC{DC��DD�DD��DE{DE�DFDF�DG{DG�{DH{DH��DI�DI�{DJ{DJ��DK{DK��DLDL��DM!HDM�{DNDN�{DO{DO�{DP�DP�{DQ�DQ�{DR{DR�{DS{DS�{DT{DT�DU{DU��DV�DV��DW{DW�{DX{DX��DY{DY�DZ{DZ�{D[�D[�D\D\�D]{D]�{D^{D^�{D_{D_�{D`�D`�{Da{Da�{DbDb�{DcDc�{Dd{Dd�De{De�{DfDf�{Dg{Dg�Dh{Dh�Di{Di�{Dj�Dj��Dk{Dk�Dl�Dl�{Dm�Dm�{DnDn�{DoDo�{Dp�Dp�{Dq{Dq�{Dr{Dr��DsDs�{Dt�Dt�{DuDu��Dv{Dv�{Dw{Dw�{DxDxnD�Q�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA��A��A��A� �A� �A�"�A� �A��A�{A�l�A�VA�I�A�E�A�C�A�;dA�&�A�JA��jA�l�A���A�"�A���A��A�O�A���A���A�Q�A��7A��A�M�A�n�A�ƨA�ZA��;A��uA���A�1'A��mA��hA��A�?}A���A�33A�?}A�;dA�?}A���A��jA��RA�A�oA��TA��A��A�z�A�33A�I�A�JA�VA�dZA��
A�l�A� �A��yA�33A�\)A��/A���A�%A�hsA�{A�Q�A���A�(�A��mA���A��yA���A�I�A�G�A��A�K�A}XAz��Az  AxĜAw�#Av��AsƨAr5?Aql�ApjAnn�Al�HAl5?Ak��Ak?}Aj^5Ai��AiK�Ah��Ae�-Ad��AcS�AaƨAaK�A`ffA_�FA_+A]ƨA\��A\�AZjAW�wAV{AT$�AS`BAR��AQƨAQ+AP��AP�AP��AO"�AL�AK��AJ�AJ�AJ~�AI?}AGx�AE�AD~�AC33AB��AA��AA�A?�A=�FA=%A;��A:=qA9;dA8{A6ȴA6E�A5;dA4(�A2�A2 �A1�PA0r�A/��A.�!A+XA* �A)/A((�A'?}A&{A$�RA#��A"�A!C�A jA�7A9XAS�AI�A�#A�uA�A;dA�uA��AȴA{A��A`BA�/An�AƨA��AI�A�mAt�A��A�A/A
VA
$�A
VA	K�A��A{AS�AAO�AZA7LA{A��AdZA�A z�A J@���@���@�|�@�S�@��@�
=@���@�t�@�33@��@�E�@�@��@홚@�t�@��@��H@���@�\@�M�@�I�@��@�?}@�r�@�K�@��@◍@�$�@�O�@�;d@�@�/@���@��`@�Ĝ@ش9@ؓu@�1@�o@֏\@պ^@�%@�(�@��@�x�@ЋD@�1'@�1@��
@υ@���@�V@�bN@�K�@ʗ�@�~�@��#@�O�@���@�bN@�ƨ@�t�@���@őh@�1@���@�-@���@��@�V@��9@� �@�l�@���@��h@��D@�S�@���@�/@���@��w@���@�l�@�~�@�M�@���@�@��^@�G�@� �@���@���@��@�%@�(�@��@��D@�O�@�A�@��m@�K�@���@�z�@�ƨ@�l�@��@��w@�O�@��j@�`B@�r�@�1@�o@�$�@�x�@��7@�/@�Z@�33@��@���@���@���@�=q@�G�@�&�@��@��/@��j@�j@��@�+@���@��y@��H@�=q@��-@���@��@���@��-@�hs@��@��j@��@��;@���@��@���@�C�@�
=@�~�@�E�@�{@��^@���@�?}@�%@���@�bN@�9X@��;@�;d@��@�ff@�V@�5?@���@��#@��#@���@���@�hs@�7L@��@�I�@�(�@���@�@��@��H@�ȴ@��!@���@�=q@�@���@�p�@��@��u@�Q�@� �@��m@��F@���@���@���@�|�@�l�@�dZ@�\)@�;d@�+@��@�o@�@��H@���@�~�@�~�@�E�@��7@�G�@�7L@��@��@��`@��9@�1'@�b@���@���@��@��w@�33@�E�@�-@�$�@�$�@���@��#@��-@�hs@�X@�G�@�?}@�7L@��@���@���@�r�@�bN@�A�@�b@���@�l�@�;d@��y@��H@��H@��@��R@��+@�=q@���@���@�7L@�Ĝ@���@��/@��`@���@��`@��/@���@��@�1@~�L@i��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA��A��A��A� �A� �A�"�A� �A��A�{A�l�A�VA�I�A�E�A�C�A�;dA�&�A�JA��jA�l�A���A�"�A���A��A�O�A���A���A�Q�A��7A��A�M�A�n�A�ƨA�ZA��;A��uA���A�1'A��mA��hA��A�?}A���A�33A�?}A�;dA�?}A���A��jA��RA�A�oA��TA��A��A�z�A�33A�I�A�JA�VA�dZA��
A�l�A� �A��yA�33A�\)A��/A���A�%A�hsA�{A�Q�A���A�(�A��mA���A��yA���A�I�A�G�A��A�K�A}XAz��Az  AxĜAw�#Av��AsƨAr5?Aql�ApjAnn�Al�HAl5?Ak��Ak?}Aj^5Ai��AiK�Ah��Ae�-Ad��AcS�AaƨAaK�A`ffA_�FA_+A]ƨA\��A\�AZjAW�wAV{AT$�AS`BAR��AQƨAQ+AP��AP�AP��AO"�AL�AK��AJ�AJ�AJ~�AI?}AGx�AE�AD~�AC33AB��AA��AA�A?�A=�FA=%A;��A:=qA9;dA8{A6ȴA6E�A5;dA4(�A2�A2 �A1�PA0r�A/��A.�!A+XA* �A)/A((�A'?}A&{A$�RA#��A"�A!C�A jA�7A9XAS�AI�A�#A�uA�A;dA�uA��AȴA{A��A`BA�/An�AƨA��AI�A�mAt�A��A�A/A
VA
$�A
VA	K�A��A{AS�AAO�AZA7LA{A��AdZA�A z�A J@���@���@�|�@�S�@��@�
=@���@�t�@�33@��@�E�@�@��@홚@�t�@��@��H@���@�\@�M�@�I�@��@�?}@�r�@�K�@��@◍@�$�@�O�@�;d@�@�/@���@��`@�Ĝ@ش9@ؓu@�1@�o@֏\@պ^@�%@�(�@��@�x�@ЋD@�1'@�1@��
@υ@���@�V@�bN@�K�@ʗ�@�~�@��#@�O�@���@�bN@�ƨ@�t�@���@őh@�1@���@�-@���@��@�V@��9@� �@�l�@���@��h@��D@�S�@���@�/@���@��w@���@�l�@�~�@�M�@���@�@��^@�G�@� �@���@���@��@�%@�(�@��@��D@�O�@�A�@��m@�K�@���@�z�@�ƨ@�l�@��@��w@�O�@��j@�`B@�r�@�1@�o@�$�@�x�@��7@�/@�Z@�33@��@���@���@���@�=q@�G�@�&�@��@��/@��j@�j@��@�+@���@��y@��H@�=q@��-@���@��@���@��-@�hs@��@��j@��@��;@���@��@���@�C�@�
=@�~�@�E�@�{@��^@���@�?}@�%@���@�bN@�9X@��;@�;d@��@�ff@�V@�5?@���@��#@��#@���@���@�hs@�7L@��@�I�@�(�@���@�@��@��H@�ȴ@��!@���@�=q@�@���@�p�@��@��u@�Q�@� �@��m@��F@���@���@���@�|�@�l�@�dZ@�\)@�;d@�+@��@�o@�@��H@���@�~�@�~�@�E�@��7@�G�@�7L@��@��@��`@��9@�1'@�b@���@���@��@��w@�33@�E�@�-@�$�@�$�@���@��#@��-@�hs@�X@�G�@�?}@�7L@��@���@���@�r�@�bN@�A�@�b@���@�l�@�;d@��y@��H@��H@��@��R@��+@�=q@���@���@�7L@�Ĝ@���@��/@��`@���@��`@��/@���@��@�1@~�L@i��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B5?B5?B5?B5?B5?B5?B5?B5?B5?B6FBQ�BR�BS�BR�BS�BW
B[#B`BBp�B|�B�=B��B��B�?B�?B�XB�^B�XB�FB�?B�3B�!B�B��B��B��B��B��B��B��B��B�uB�bB�JB�+B� Bx�Bq�B`BBVBD�B<jB8RB5?B+B
=B�B�`B�
BȴB�qB�B��B�B}�Bv�Bm�BhsBe`B^5BVBG�B=qB5?B(�B�BB
��B
�B
�mB
��B
�jB
��B
p�B
]/B
W
B
S�B
J�B
C�B
49B
,B
%�B
�B
hB
1B
B	��B	��B	��B	�B	�B	�fB	��B	��B	��B	�LB	�9B	�-B	�!B	�!B	�B	��B	��B	��B	�JB	�B	x�B	u�B	r�B	m�B	iyB	gmB	ffB	cTB	[#B	O�B	J�B	G�B	E�B	C�B	<jB	1'B	%�B	�B	�B	{B	\B	PB	DB	B��B��B��B�B�B�HB�/B��B��B��B��B��B��BƨB��B�9B�B�B��B��B��B��B��B��B��B�{B�hB�PB�DB�=B�7B�+B�+B�%B�1B�DB�DB�DB�%B|�B{�B}�Bw�By�By�B{�By�By�Bv�Bu�Bo�Br�B{�B|�Bw�Bx�Bx�B{�Bz�Bw�Bq�Bl�Bm�Br�Bt�Bt�Bs�Bs�Bs�Bs�Br�Bt�Bu�Bt�Bs�Bs�Br�Bp�Bm�BgmBcTBdZBdZBdZBdZBcTBbNBaHB`BB`BB`BBbNBcTBdZBdZBdZBhsBk�Bm�Bq�Bu�Bu�Bu�Bv�Bz�B� B�B�B�%B�+B�DB�\B�oB�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�?B�RB�jB�qB�wB�}B��BÖBƨBɺB��B�
B�/B�/B�/B�BB�NB�HB�BB�;B�NB�B��B��B��B��B��B��B��B��B��B��B	B		7B	PB	oB	hB	\B	PB	\B	bB	oB	�B	%�B	%�B	,B	+B	+B	1'B	5?B	:^B	@�B	D�B	F�B	K�B	M�B	O�B	T�B	T�B	VB	W
B	XB	]/B	`BB	`BB	aHB	bNB	e`B	jB	l�B	o�B	p�B	t�B	z�B	� B	�B	�B	�%B	�1B	�7B	�7B	�=B	�JB	�VB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�3B	�?B	�FB	�FB	�FB	�FB	�LB	�XB	�dB	�dB	�qB	�}B	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�)B	�#B	�#B	�#B	�)B	�/B	�5B	�BB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
1B
B
/22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B5?B5?B5?B5?B5?B5?B5?B5?B5?B6FBQ�BR�BS�BR�BS�BW
B[#B`BBp�B|�B�=B��B��B�?B�?B�XB�^B�XB�FB�?B�3B�!B�B��B��B��B��B��B��B��B��B�uB�bB�JB�+B� Bx�Bq�B`BBVBD�B<jB8RB5?B+B
=B�B�`B�
BȴB�qB�B��B�B}�Bv�Bm�BhsBe`B^5BVBG�B=qB5?B(�B�BB
��B
�B
�mB
��B
�jB
��B
p�B
]/B
W
B
S�B
J�B
C�B
49B
,B
%�B
�B
hB
1B
B	��B	��B	��B	�B	�B	�fB	��B	��B	��B	�LB	�9B	�-B	�!B	�!B	�B	��B	��B	��B	�JB	�B	x�B	u�B	r�B	m�B	iyB	gmB	ffB	cTB	[#B	O�B	J�B	G�B	E�B	C�B	<jB	1'B	%�B	�B	�B	{B	\B	PB	DB	B��B��B��B�B�B�HB�/B��B��B��B��B��B��BƨB��B�9B�B�B��B��B��B��B��B��B��B�{B�hB�PB�DB�=B�7B�+B�+B�%B�1B�DB�DB�DB�%B|�B{�B}�Bw�By�By�B{�By�By�Bv�Bu�Bo�Br�B{�B|�Bw�Bx�Bx�B{�Bz�Bw�Bq�Bl�Bm�Br�Bt�Bt�Bs�Bs�Bs�Bs�Br�Bt�Bu�Bt�Bs�Bs�Br�Bp�Bm�BgmBcTBdZBdZBdZBdZBcTBbNBaHB`BB`BB`BBbNBcTBdZBdZBdZBhsBk�Bm�Bq�Bu�Bu�Bu�Bv�Bz�B� B�B�B�%B�+B�DB�\B�oB�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�?B�RB�jB�qB�wB�}B��BÖBƨBɺB��B�
B�/B�/B�/B�BB�NB�HB�BB�;B�NB�B��B��B��B��B��B��B��B��B��B��B	B		7B	PB	oB	hB	\B	PB	\B	bB	oB	�B	%�B	%�B	,B	+B	+B	1'B	5?B	:^B	@�B	D�B	F�B	K�B	M�B	O�B	T�B	T�B	VB	W
B	XB	]/B	`BB	`BB	aHB	bNB	e`B	jB	l�B	o�B	p�B	t�B	z�B	� B	�B	�B	�%B	�1B	�7B	�7B	�=B	�JB	�VB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�3B	�?B	�FB	�FB	�FB	�FB	�LB	�XB	�dB	�dB	�qB	�}B	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�)B	�#B	�#B	�#B	�)B	�/B	�5B	�BB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
1B
B
/22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191751                              AO  ARCAADJP                                                                    20181005191751    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191751  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191751  QCF$                G�O�G�O�G�O�8000            