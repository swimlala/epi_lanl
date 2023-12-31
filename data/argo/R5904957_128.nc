CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:28Z creation      
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
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181024140828  20181024140828  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�Ϥ��d1   @�ϥ/h^2@4���`A��c��E��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BPffBX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C{�fC~  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D �fD!  D!� D!��D"y�D#  D#�fD$fD$� D%  D%� D&  D&� D'  D'�fD(fD(� D)  D)�fD*  D*� D+  D+� D,  D,� D-fD-�fD.fD.�fD/  D/� D0  D0� D1fD1� D1��D2� D3  D3� D4  D4� D4��D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DDy�DD��DE� DF  DF� DG  DG� DH  DHy�DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D]��D^� D_fD_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� DlfDl� Dm  Dm� Dn  Dn� Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv�fDwfDw� Dx  DxS3Dy��D�-�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�Q�@�Q�A (�A (�A@(�A`(�A��HA�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(
=B/��B8
=B@
=BH
=BPp�BX
=B`
=Bh
=Bp
=Bw��B�B�B�B�B�B�B�B�B���B�B�B���B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C)C�C
�C�C�C�C�C��C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C7��C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv)Cx�Cz�C{��C~�C�C�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�C�C�HC�HC�C�HC�HC�HC�HC�C�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC��{C��{C�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D z>D �D��D �D��D �D�
D
D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D
D�
D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D�>D��D  �D �
D! �D!��D!�>D"z>D# �D#�
D$
D$��D% �D%��D& �D&��D' �D'�
D(
D(��D) �D)�
D* �D*��D+ �D+��D, �D,��D-
D-�
D.
D.�
D/ �D/��D0 �D0��D1
D1��D1�>D2��D3 �D3��D4 �D4��D4�>D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DC�>DDz>DD�>DE��DF �DF��DG �DG��DH �DHz>DI �DI��DJ �DJ��DK �DKz>DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU�
DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\
D\��D] �D]��D]�>D^��D_
D_��D` �D`�
Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dgz>Dh �Dh��Di �Di�
Dj �Dj��Dk �Dk��Dl
Dl��Dm �Dm��Dn �Dn��Dn�>Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv�
Dw
Dw��Dx �DxS�Dy��D�.D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA�hsA�hsA�hsA�jA�jA�jA�jA�jA�hsA�jA�jA�l�A�n�A�r�A�p�A�t�A�v�A�v�A�t�A�E�A܇+A�z�A�bNA�^5AҾwA�=qA�n�A·+A��#Aʏ\A�^5A�K�A�\)A��
A�t�A�Q�A�O�A�{A�~�A��A�ƨA�&�A�Q�A�x�A���A�z�A��A�G�A���A�v�A�r�A���A�=qA�?}A�A��#A��A�ffA�ZA��yA�Q�A�`BA��+A�|�A�ȴA��
A�n�A�"�A�ffA��A���A�bA�1'A�\)A��RA��A�dZA���A��+A��A���A�?}A��A��A���AA}��A|n�A{%Ay�mAx9XAv��Av�AtȴArM�Aq�PAnA�Ak�PAh��Ah1'Act�A_;dA\�HAZȴAXVAV��AU
=AQ�7APbAOXAMK�AKt�AJn�AI;dAGG�AE�wAB{A@��A?33A=+A:�/A8v�A6ZA4�DA3�A3�FA3"�A2I�A1�A1�PA0I�A/�hA/;dA.�A.-A-�;A,��A,{A+hsA*�yA)G�A(1'AZA  AK�A�jA��A?}A��A�FA��AK�A��AI�A�PA
��A
�DA	�
A	O�A�yA��AAZA%A�uA�DA��A��A��A�#Ar�A��AK�A ��@�o@��;@��R@��@�Q�@���@�t�@���@���@�v�@�`B@�@�9X@ꗍ@�9@�~�@���@�9@�9X@�  @�@�C�@�?}@߶F@���@ܣ�@�+@�7L@�r�@�9X@�  @��;@���@׮@�\)@�ȴ@և+@�{@�@�&�@��@�I�@ҧ�@��@�S�@ΰ!@Ο�@�E�@��@ͺ^@�`B@��@�K�@���@ʸR@ʇ+@ɡ�@�I�@��H@Ĵ9@�b@��m@Å@�
=@�$�@��-@�O�@��`@��@�bN@�1@�l�@��@�$�@�O�@�V@��D@��@��P@���@�(�@��;@�&�@���@�x�@��
@��P@���@���@���@�ff@��#@�Q�@�1@��@��
@�ƨ@��F@���@�l�@�K�@�K�@�C�@�"�@�o@�@��@��!@��+@�ff@�ff@�^5@�G�@��@�Z@�A�@��m@��@�{@�@���@�O�@��u@�bN@�bN@�A�@�1@��m@���@�@��+@�V@��@��T@���@�X@�&�@�%@���@��`@��@�r�@�9X@�ƨ@��@��H@��\@�n�@��@�J@���@��@��-@�x�@�hs@�&�@�%@�V@���@��@��`@��j@�Q�@��
@���@�\)@�;d@��y@�v�@�@�hs@��@��9@�Q�@���@�S�@��+@��9@�  @��@�  @��@��m@��m@��;@��@�dZ@�K�@�=q@��-@�Ĝ@�1'@��m@���@��w@�|�@�o@��@���@�E�@�J@�hs@�&�@�%@���@���@��u@�I�@�1'@��@��m@�|�@�"�@��\@�v�@��+@��\@���@���@��+@�ff@�E�@�$�@�J@�J@���@�@�J@�J@�@��-@�p�@��D@�r�@���@�Q�@��@���@��@�C�@��@��\@�J@���@��7@�X@�X@��@���@�I�@���@��w@��P@�;d@�
=@��y@�ȴ@��\@��!@���@���@���@���@��\@�v�@��@�p�@�%@�Q�@� �@�1@���@�|�@�\)@�K�@�C�@��y@�J@��#@��-@���@��h@�`B@�&�@��@��9@�z�@� �@�1@��w@�t�@�\)@�33@�ȴ@���@�~�@�V@�$�@���@{�P@h�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ffA�hsA�hsA�hsA�jA�jA�jA�jA�jA�hsA�jA�jA�l�A�n�A�r�A�p�A�t�A�v�A�v�A�t�A�E�A܇+A�z�A�bNA�^5AҾwA�=qA�n�A·+A��#Aʏ\A�^5A�K�A�\)A��
A�t�A�Q�A�O�A�{A�~�A��A�ƨA�&�A�Q�A�x�A���A�z�A��A�G�A���A�v�A�r�A���A�=qA�?}A�A��#A��A�ffA�ZA��yA�Q�A�`BA��+A�|�A�ȴA��
A�n�A�"�A�ffA��A���A�bA�1'A�\)A��RA��A�dZA���A��+A��A���A�?}A��A��A���AA}��A|n�A{%Ay�mAx9XAv��Av�AtȴArM�Aq�PAnA�Ak�PAh��Ah1'Act�A_;dA\�HAZȴAXVAV��AU
=AQ�7APbAOXAMK�AKt�AJn�AI;dAGG�AE�wAB{A@��A?33A=+A:�/A8v�A6ZA4�DA3�A3�FA3"�A2I�A1�A1�PA0I�A/�hA/;dA.�A.-A-�;A,��A,{A+hsA*�yA)G�A(1'AZA  AK�A�jA��A?}A��A�FA��AK�A��AI�A�PA
��A
�DA	�
A	O�A�yA��AAZA%A�uA�DA��A��A��A�#Ar�A��AK�A ��@�o@��;@��R@��@�Q�@���@�t�@���@���@�v�@�`B@�@�9X@ꗍ@�9@�~�@���@�9@�9X@�  @�@�C�@�?}@߶F@���@ܣ�@�+@�7L@�r�@�9X@�  @��;@���@׮@�\)@�ȴ@և+@�{@�@�&�@��@�I�@ҧ�@��@�S�@ΰ!@Ο�@�E�@��@ͺ^@�`B@��@�K�@���@ʸR@ʇ+@ɡ�@�I�@��H@Ĵ9@�b@��m@Å@�
=@�$�@��-@�O�@��`@��@�bN@�1@�l�@��@�$�@�O�@�V@��D@��@��P@���@�(�@��;@�&�@���@�x�@��
@��P@���@���@���@�ff@��#@�Q�@�1@��@��
@�ƨ@��F@���@�l�@�K�@�K�@�C�@�"�@�o@�@��@��!@��+@�ff@�ff@�^5@�G�@��@�Z@�A�@��m@��@�{@�@���@�O�@��u@�bN@�bN@�A�@�1@��m@���@�@��+@�V@��@��T@���@�X@�&�@�%@���@��`@��@�r�@�9X@�ƨ@��@��H@��\@�n�@��@�J@���@��@��-@�x�@�hs@�&�@�%@�V@���@��@��`@��j@�Q�@��
@���@�\)@�;d@��y@�v�@�@�hs@��@��9@�Q�@���@�S�@��+@��9@�  @��@�  @��@��m@��m@��;@��@�dZ@�K�@�=q@��-@�Ĝ@�1'@��m@���@��w@�|�@�o@��@���@�E�@�J@�hs@�&�@�%@���@���@��u@�I�@�1'@��@��m@�|�@�"�@��\@�v�@��+@��\@���@���@��+@�ff@�E�@�$�@�J@�J@���@�@�J@�J@�@��-@�p�@��D@�r�@���@�Q�@��@���@��@�C�@��@��\@�J@���@��7@�X@�X@��@���@�I�@���@��w@��P@�;d@�
=@��y@�ȴ@��\@��!@���@���@���@���@��\@�v�@��@�p�@�%@�Q�@� �@�1@���@�|�@�\)@�K�@�C�@��y@�J@��#@��-@���@��h@�`B@�&�@��@��9@�z�@� �@�1@��w@�t�@�\)@�33@�ȴ@���@�~�@�V@�$�@���@{�P@h�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Br�Bs�Bv�B��B�B�FB�dB�^B�XB��B�/B�B��B%B\BoB�B,B5?B>wBF�BE�BD�BG�BG�BF�BC�BE�BG�BG�BF�BC�B>wB5?B49B.B,B)�B)�B%�B�BbB+B��B�ZB��BŢB�?B�uBw�Bl�BgmB_;BVB?}B%�B�B
��B
�NB
�B
��B
�XB
�-B
��B
`BB
F�B
8RB
$�B
�B
�B
VB
B	��B	�B	�B	�/B	��B	ȴB	�B	��B	�B	|�B	aHB	G�B	;dB	:^B	33B	,B	#�B	 �B	�B	�B	uB	JB		7B	B	  B��B�B�B�`B�5B�
B��BǮBŢBƨBŢBÖBB��B��B�wB�qB�qB�jB�^B�XB�RB�LB�LB�RB�}BĜB�oB�hB�hB�hB�\B�PB�=B�=B�1B�%B�B�B�B~�B}�B�B�B�7B�JB�DB�B|�B|�B�B�=B�PB�uB��B��B�uB�oB�oB�VB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�3B�?B�jBBÖBƨBǮBȴB��B��B�B�/B�/B�5B�HB�sB�sB�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	%B	+B	
=B	PB	bB	�B	�B	�B	"�B	"�B	�B	�B	�B	"�B	#�B	"�B	!�B	�B	�B	�B	�B	 �B	#�B	$�B	%�B	'�B	+B	,B	,B	-B	.B	.B	/B	1'B	33B	5?B	6FB	<jB	?}B	B�B	B�B	C�B	E�B	E�B	H�B	L�B	M�B	R�B	VB	W
B	ZB	[#B	\)B	^5B	aHB	dZB	k�B	n�B	q�B	t�B	u�B	w�B	x�B	y�B	y�B	{�B	|�B	~�B	�B	�B	�1B	�=B	�=B	�PB	�PB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�3B	�3B	�!B	�B	�B	�'B	�3B	�3B	�3B	�3B	�3B	�3B	�3B	�LB	�LB	�FB	�FB	�RB	�RB	�XB	�XB	�^B	�dB	�dB	�jB	�dB	�jB	�dB	�dB	�dB	�dB	�dB	�dB	�dB	�dB	�dB	�jB	�jB	�}B	��B	��B	B	ÖB	ĜB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�BB	�HB	�`B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B
	7B
	7B
	7B
	7B

=B

=B
VB
�B
�B
+�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Br�Bs�Bv�B��B�B�FB�dB�^B�XB��B�/B�B��B%B\BoB�B,B5?B>wBF�BE�BD�BG�BG�BF�BC�BE�BG�BG�BF�BC�B>wB5?B49B.B,B)�B)�B%�B�BbB+B��B�ZB��BŢB�?B�uBw�Bl�BgmB_;BVB?}B%�B�B
��B
�NB
�B
��B
�XB
�-B
��B
`BB
F�B
8RB
$�B
�B
�B
VB
B	��B	�B	�B	�/B	��B	ȴB	�B	��B	�B	|�B	aHB	G�B	;dB	:^B	33B	,B	#�B	 �B	�B	�B	uB	JB		7B	B	  B��B�B�B�`B�5B�
B��BǮBŢBƨBŢBÖBB��B��B�wB�qB�qB�jB�^B�XB�RB�LB�LB�RB�}BĜB�oB�hB�hB�hB�\B�PB�=B�=B�1B�%B�B�B�B~�B}�B�B�B�7B�JB�DB�B|�B|�B�B�=B�PB�uB��B��B�uB�oB�oB�VB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�3B�?B�jBBÖBƨBǮBȴB��B��B�B�/B�/B�5B�HB�sB�sB�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	%B	+B	
=B	PB	bB	�B	�B	�B	"�B	"�B	�B	�B	�B	"�B	#�B	"�B	!�B	�B	�B	�B	�B	 �B	#�B	$�B	%�B	'�B	+B	,B	,B	-B	.B	.B	/B	1'B	33B	5?B	6FB	<jB	?}B	B�B	B�B	C�B	E�B	E�B	H�B	L�B	M�B	R�B	VB	W
B	ZB	[#B	\)B	^5B	aHB	dZB	k�B	n�B	q�B	t�B	u�B	w�B	x�B	y�B	y�B	{�B	|�B	~�B	�B	�B	�1B	�=B	�=B	�PB	�PB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�3B	�3B	�!B	�B	�B	�'B	�3B	�3B	�3B	�3B	�3B	�3B	�3B	�LB	�LB	�FB	�FB	�RB	�RB	�XB	�XB	�^B	�dB	�dB	�jB	�dB	�jB	�dB	�dB	�dB	�dB	�dB	�dB	�dB	�dB	�dB	�jB	�jB	�}B	��B	��B	B	ÖB	ĜB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�BB	�HB	�`B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B
	7B
	7B
	7B
	7B

=B

=B
VB
�B
�B
+�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.01 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140828                              AO  ARCAADJP                                                                    20181024140828    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140828  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140828  QCF$                G�O�G�O�G�O�0               