CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-12T00:37:33Z creation;2019-05-12T00:37:37Z conversion to V3.1;2019-12-23T06:02:28Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        X  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oT   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  s,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �l   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �$   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �$   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �$   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �$   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190512003733  20200120031518  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_145                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ؽ6K�1   @ؽ7��@7��$tS��b�*0U2a1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ D�|�D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�3D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�3D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @h��@�z�@�z�A
=qA*=qAJ=qAj=qA��A��A��A��A��A��A��A��B�\B
�\B�\B�\B"�\B*�\B2�\B:�\BB�\BJ�\BR�\BZ�\Bb�\Bj�\Br�\Bz�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�Q�C�Q�C�Q�C�Q�C�Q�C�^�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�EC�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�D (�D ��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D"�D��D	(�D	��D
(�D
��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D (�D ��D!(�D!��D"(�D"��D#(�D#��D$(�D$��D%(�D%��D&(�D&��D'(�D'��D((�D(��D)(�D)��D*(�D*��D+(�D+��D,(�D,��D-(�D-��D.(�D.��D/(�D/��D0(�D0��D1(�D1��D2(�D2��D3(�D3��D4(�D4��D5(�D5��D6(�D6��D7(�D7��D8(�D8��D9(�D9��D:(�D:��D;(�D;��D<"�D<��D=(�D=��D>(�D>��D?(�D?��D@(�D@��DA(�DA��DB(�DB��DC(�DC��DD(�DD��DE(�DE��DF(�DF��DG(�DG��DH(�DH��DI(�DI��DJ(�DJ��DK(�DK��DL(�DL��DM(�DM��DN(�DN��DO(�DO��DP(�DP��DQ(�DQ��DR(�DR��DS(�DS��DT(�DT��DU(�DU��DV(�DV��DW(�DW��DX(�DX�\DY(�DY��DZ(�DZ��D[(�D[��D\(�D\��D](�D]��D^(�D^��D_(�D_��D`(�D`��Da(�Da��Db(�Db��Dc(�Dc��Dd(�Dd��De(�De��Df(�Df��Dg(�Dg��Dh(�Dh��Di(�Di��Dj(�Dj��Dk(�Dk��Dl(�Dl��Dm(�Dm��Dn(�Dn��Do(�Do��Dp(�Dp��Dq(�Dq��Dr(�Dr��Ds(�Ds��Dt(�Dt��Du(�Du��Dv(�Dv��Dw(�Dw��Dx(�Dx��Dy(�Dy��Dz(�Dz��D{(�D{��D|(�D|��D}(�D}��D~(�D~��D(�D��D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D{D��{D�{D�T{DÔ{D��{D�{D�T{DđHD��{D�{D�T{DŔ{D��{D�{D�T{DƔ{D��{D�{D�T{Dǔ{D��{D�{D�T{DȔ{D��{D��D�T{Dɔ{D��{D�{D�T{Dʔ{D��{D�{D�T{D˔{D��{D�{D�T{D̔{D��{D�{D�T{D͔{D��{D�{D�T{DΔ{D��{D�{D�T{Dϔ{D��{D�{D�T{DД{D��{D��D�T{Dє{D��{D�{D�T{DҔ{D��{D�{D�T{DӔ{D��{D�{D�T{DԔ{D��{D�{D�T{DՔ{D��{D�{D�T{D֔{D��{D�{D�T{Dה{D��{D�{D�T{Dؔ{D��{D�{D�T{Dٔ{D��{D�{D�T{Dڔ{D��{D�{D�T{D۔{D��{D�{D�T{Dܔ{D��{D�{D�T{Dݔ{D��{D�{D�T{Dޔ{D��{D�{D�T{Dߔ{D��{D�{D�T{D��{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D痮D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��HD�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D��{D��{D�{D�W�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aȟ�A�VA���Aƣ�AċDA��AîAËDA�p�A�^5A�7LA���A���Aº^A©�A7A�K�A���A��FA�t�A�n�A�33A��A���A�I�A��
A��A�9XA��!A��A�z�A��hA��-A�7LA��A���A�ffA�S�A�O�A�E�A�;dA��A��A�ĜA��A�C�A��A��TA�C�A��DA��A��9A�|�A��-A�G�A�7LA�1'A��jA���A�hsA��HA��+A���A�A�x�A�$�A�`BA�O�A���A��^A�XA�VA�JA�`BA���A���A�"�A���A�A��A�%A���A��^A�\)A�Q�A���A���A��A��uA��;A� �A�S�A��^A�5?A�VA��A�ĜA�I�A�  A���A��yA��`A���A�ZA�JA�ZA�v�A�A�A�bNA�~�A��+A�/A��RA��A�|�A�jA��`A�r�A��A��7A��FA�bA��A��HA��A�$�A��DA�7LA}��A{oAz�Az  AyC�Axz�AtjAp�Ai�7Ae&�Aa�A`  A\~�AY�AXjAWl�AS��AQ�^ANAM;dAL$�AIƨAGO�AE�
ACoAA�TAA%A@��A@��A@�yA@bNA>E�A<n�A;��A:E�A7��A5��A4�jA3�FA3
=A2��A2�A1A0��A0ffA.=qA*��A*�A(�!A'�A%t�A$��A$ �A#�A!�;A ��A��A�HA�AVA��A\)A^5A%A=qA��A
=A��A��A��A��A��A&�Az�A��A��A�;A7LA��AbNAx�A
�/A
JA�\A�TAK�A^5AO�A1Az�A+A M�@���@�^5@���@��m@��j@���@�n�@�%@�z�@��@�^@���@��/@�@��@�z�@���@�@�=q@��@��@�R@�`B@�33@�=q@��H@ج@�  @� �@�1@�t�@�33@ׅ@�t�@�o@�$�@ӕ�@�Ĝ@�bN@ЋD@�(�@���@��;@�|�@�dZ@�~�@�/@��
@�@�X@��@�I�@Ɵ�@��T@�?}@�Q�@��@¸R@��j@��@�5?@�&�@���@�r�@�b@���@��@��@���@�ȴ@��@�V@�Z@���@�ȴ@���@�$�@��@�O�@�p�@��7@��7@�&�@�j@�ƨ@���@�E�@�X@��9@��D@�b@�\)@��7@�A�@��m@�ƨ@���@��F@�t�@���@��@���@��j@�bN@��@�K�@���@�5?@��T@���@�`B@���@�j@�Z@�  @�(�@�Q�@��@��;@���@�t�@�"�@��\@�-@��@���@���@�7L@���@��D@�I�@���@�|�@�K�@�
=@���@�M�@�@�@�X@�%@��@�1'@���@��F@��@�\)@��@�v�@�ff@�=q@�$�@�J@�`B@�V@�Ĝ@��@�Q�@�A�@��
@��@���@�t�@�C�@�;d@�o@���@�v�@�J@��+@��\@��@�%@��`@��@�O�@��@��h@���@�O�@� �@�l�@�@��!@���@���@�^5@�{@��T@��7@��@��@��/@���@��@���@�(�@�ƨ@��P@�l�@�\)@�K�@�C�@�;d@�+@��@��y@��@���@�M�@�{@��T@���@�7L@��/@���@�Ĝ@���@�1'@� �@��@��P@��@��P@��P@�\)@�;d@�+@�o@��y@��R@��\@�V@�E�@�E�@�-@���@��T@���@��@�X@�/@�%@��j@��D@�I�@�b@�ƨ@��@�dZ@�
=@��@��@�ȴ@���@��@���@�x�@�`B@��/@�j@�1'@�  @��F@���@�dZ@��@��@��@��@��@��@���@�@�`B@�O�@�?}@�7L@�7L@���@��D@�b@��@�1'@� �@�1@�@��@\)@+@�@~v�@}@}�@|�j@|�@{��@{"�@z��@y�@xĜ@x��@x��@x�9@xr�@x1'@w�;@v��@vȴ@vE�@u�@t��@tZ@s��@r�!@r~�@rM�@q�#@p�`@p�9@p  @ol�@n��@nE�@m�@m�h@l�@l�j@l�D@k��@k�m@kƨ@k�F@k33@j��@j^5@i�#@i�@h��@h�9@h�@hr�@hA�@g�;@g;d@f�y@f�y@fȴ@f��@f{@e�-@eO�@e/@e�@eV@d�D@c�m@ct�@b�H@bM�@b�@b�@a�#@a��@`Ĝ@`Q�@` �@_��@_;d@^�@^�+@^5?@]�T@]/@\��@\�@\�@[�m@[S�@Z�\@Y�^@Y�7@Yx�@Y&�@Y�@X��@X��@X�@XbN@XQ�@X1'@W�@W�@V��@V��@Vv�@U��@U�-@U��@UO�@T�@T9X@Sƨ@S��@St�@SdZ@R�H@RM�@R�@Q�^@Qhs@Q%@PĜ@PQ�@P  @O�;@O�w@O�P@OK�@Nȴ@N��@N��@NV@M�@M�h@MO�@M/@M�@L��@L��@L�/@L��@L�@LI�@Kt�@KdZ@KC�@J�\@J^5@J�@I��@I��@I&�@H�@HQ�@HQ�@Hb@G�w@GK�@F�@F�+@FV@FE�@F5?@F@E�h@E?}@D��@D�@D�/@D��@Dz�@Dj@DI�@C�m@C�@C"�@B�@B��@B�!@B��@B�\@Bn�@BM�@A�@A�^@A��@Ax�@A&�@@Ĝ@@�@@Q�@@b@?�w@?K�@>�@>��@>ff@=�@=@=��@=��@=�h@=p�@=O�@=V@<�@<�j@<z�@<I�@<�@;�m@;�@:�@:�!@:n�@:^5@:=q@9��@9�^@9hs@9G�@97L@8�`@8r�@8bN@8bN@8Q�@8Q�@8 �@7��@7|�@7K�@7
=@6��@6�y@6�R@6�+@6E�@5�-@5`B@5�@5V@4��@4Z@4(�@3��@3��@3t�@3C�@3"�@3o@3o@2�H@2��@2M�@2J@1�^@1��@1x�@1X@1%@0�9@0�u@0bN@0A�@01'@0b@/�@/��@/�P@/l�@.�y@.ff@.$�@-�@-��@-p�@-?}@,��@,�j@,��@,�D@,9X@+��@+�
@+��@+C�@+@*��@*�\@*n�@*M�@*-@)��@)�^@)x�@)�@(�u@(A�@'�@'�P@'K�@&��@&�R@&��@&�+@&E�@&@%�-@%�h@%`B@%/@$�@$�j@$��@$Z@$1@#�F@#�@#t�@#t�@#S�@#33@#"�@#"�@#o@"�@"��@"��@"~�@"~�@"=q@!�@!�7@!G�@!&�@ �`@ ��@ �9@ ��@ �@ b@��@l�@;d@
=@�y@�@��@v�@E�@$�@{@@�T@�-@��@��@�h@p�@?}@��@�j@��@I�@1@�m@�m@�m@��@dZ@"�@�H@��@�\@M�@��@�@�@��@G�@�@%@��@r�@Q�@Q�@1'@b@��@�@|�@;d@+@�@��@�y@ȴ@��@E�@{@�@@��@p�@/@��@�@�/@��@�D@j@j@I�@�@��@�
@t�@C�@33@"�@�H@��@�\@^5@=q@�@�@�^@��@X@7L@&�@%@�`@��@�9@�9@��@�@A�@  @�@��@��@|�@l�@K�@;d@+@��@�y@ȴ@ȴ@ȴ@�R@v�@@�-@�h@`B@?}@V@�@�/@�j@j@�@�@�@ƨ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aȟ�A�VA���Aƣ�AċDA��AîAËDA�p�A�^5A�7LA���A���Aº^A©�A7A�K�A���A��FA�t�A�n�A�33A��A���A�I�A��
A��A�9XA��!A��A�z�A��hA��-A�7LA��A���A�ffA�S�A�O�A�E�A�;dA��A��A�ĜA��A�C�A��A��TA�C�A��DA��A��9A�|�A��-A�G�A�7LA�1'A��jA���A�hsA��HA��+A���A�A�x�A�$�A�`BA�O�A���A��^A�XA�VA�JA�`BA���A���A�"�A���A�A��A�%A���A��^A�\)A�Q�A���A���A��A��uA��;A� �A�S�A��^A�5?A�VA��A�ĜA�I�A�  A���A��yA��`A���A�ZA�JA�ZA�v�A�A�A�bNA�~�A��+A�/A��RA��A�|�A�jA��`A�r�A��A��7A��FA�bA��A��HA��A�$�A��DA�7LA}��A{oAz�Az  AyC�Axz�AtjAp�Ai�7Ae&�Aa�A`  A\~�AY�AXjAWl�AS��AQ�^ANAM;dAL$�AIƨAGO�AE�
ACoAA�TAA%A@��A@��A@�yA@bNA>E�A<n�A;��A:E�A7��A5��A4�jA3�FA3
=A2��A2�A1A0��A0ffA.=qA*��A*�A(�!A'�A%t�A$��A$ �A#�A!�;A ��A��A�HA�AVA��A\)A^5A%A=qA��A
=A��A��A��A��A��A&�Az�A��A��A�;A7LA��AbNAx�A
�/A
JA�\A�TAK�A^5AO�A1Az�A+A M�@���@�^5@���@��m@��j@���@�n�@�%@�z�@��@�^@���@��/@�@��@�z�@���@�@�=q@��@��@�R@�`B@�33@�=q@��H@ج@�  @� �@�1@�t�@�33@ׅ@�t�@�o@�$�@ӕ�@�Ĝ@�bN@ЋD@�(�@���@��;@�|�@�dZ@�~�@�/@��
@�@�X@��@�I�@Ɵ�@��T@�?}@�Q�@��@¸R@��j@��@�5?@�&�@���@�r�@�b@���@��@��@���@�ȴ@��@�V@�Z@���@�ȴ@���@�$�@��@�O�@�p�@��7@��7@�&�@�j@�ƨ@���@�E�@�X@��9@��D@�b@�\)@��7@�A�@��m@�ƨ@���@��F@�t�@���@��@���@��j@�bN@��@�K�@���@�5?@��T@���@�`B@���@�j@�Z@�  @�(�@�Q�@��@��;@���@�t�@�"�@��\@�-@��@���@���@�7L@���@��D@�I�@���@�|�@�K�@�
=@���@�M�@�@�@�X@�%@��@�1'@���@��F@��@�\)@��@�v�@�ff@�=q@�$�@�J@�`B@�V@�Ĝ@��@�Q�@�A�@��
@��@���@�t�@�C�@�;d@�o@���@�v�@�J@��+@��\@��@�%@��`@��@�O�@��@��h@���@�O�@� �@�l�@�@��!@���@���@�^5@�{@��T@��7@��@��@��/@���@��@���@�(�@�ƨ@��P@�l�@�\)@�K�@�C�@�;d@�+@��@��y@��@���@�M�@�{@��T@���@�7L@��/@���@�Ĝ@���@�1'@� �@��@��P@��@��P@��P@�\)@�;d@�+@�o@��y@��R@��\@�V@�E�@�E�@�-@���@��T@���@��@�X@�/@�%@��j@��D@�I�@�b@�ƨ@��@�dZ@�
=@��@��@�ȴ@���@��@���@�x�@�`B@��/@�j@�1'@�  @��F@���@�dZ@��@��@��@��@��@��@���@�@�`B@�O�@�?}@�7L@�7L@���@��D@�b@��@�1'@� �@�1@�@��@\)@+@�@~v�@}@}�@|�j@|�@{��@{"�@z��@y�@xĜ@x��@x��@x�9@xr�@x1'@w�;@v��@vȴ@vE�@u�@t��@tZ@s��@r�!@r~�@rM�@q�#@p�`@p�9@p  @ol�@n��@nE�@m�@m�h@l�@l�j@l�D@k��@k�m@kƨ@k�F@k33@j��@j^5@i�#@i�@h��@h�9@h�@hr�@hA�@g�;@g;d@f�y@f�y@fȴ@f��@f{@e�-@eO�@e/@e�@eV@d�D@c�m@ct�@b�H@bM�@b�@b�@a�#@a��@`Ĝ@`Q�@` �@_��@_;d@^�@^�+@^5?@]�T@]/@\��@\�@\�@[�m@[S�@Z�\@Y�^@Y�7@Yx�@Y&�@Y�@X��@X��@X�@XbN@XQ�@X1'@W�@W�@V��@V��@Vv�@U��@U�-@U��@UO�@T�@T9X@Sƨ@S��@St�@SdZ@R�H@RM�@R�@Q�^@Qhs@Q%@PĜ@PQ�@P  @O�;@O�w@O�P@OK�@Nȴ@N��@N��@NV@M�@M�h@MO�@M/@M�@L��@L��@L�/@L��@L�@LI�@Kt�@KdZ@KC�@J�\@J^5@J�@I��@I��@I&�@H�@HQ�@HQ�@Hb@G�w@GK�@F�@F�+@FV@FE�@F5?@F@E�h@E?}@D��@D�@D�/@D��@Dz�@Dj@DI�@C�m@C�@C"�@B�@B��@B�!@B��@B�\@Bn�@BM�@A�@A�^@A��@Ax�@A&�@@Ĝ@@�@@Q�@@b@?�w@?K�@>�@>��@>ff@=�@=@=��@=��@=�h@=p�@=O�@=V@<�@<�j@<z�@<I�@<�@;�m@;�@:�@:�!@:n�@:^5@:=q@9��@9�^@9hs@9G�@97L@8�`@8r�@8bN@8bN@8Q�@8Q�@8 �@7��@7|�@7K�@7
=@6��@6�y@6�R@6�+@6E�@5�-@5`B@5�@5V@4��@4Z@4(�@3��@3��@3t�@3C�@3"�@3o@3o@2�H@2��@2M�@2J@1�^@1��@1x�@1X@1%@0�9@0�u@0bN@0A�@01'@0b@/�@/��@/�P@/l�@.�y@.ff@.$�@-�@-��@-p�@-?}@,��@,�j@,��@,�D@,9X@+��@+�
@+��@+C�@+@*��@*�\@*n�@*M�@*-@)��@)�^@)x�@)�@(�u@(A�@'�@'�P@'K�@&��@&�R@&��@&�+@&E�@&@%�-@%�h@%`B@%/@$�@$�j@$��@$Z@$1@#�F@#�@#t�@#t�@#S�@#33@#"�@#"�@#o@"�@"��@"��@"~�@"~�@"=q@!�@!�7@!G�@!&�@ �`@ ��@ �9@ ��@ �@ b@��@l�@;d@
=@�y@�@��@v�@E�@$�@{@@�T@�-@��@��@�h@p�@?}@��@�j@��@I�@1@�m@�m@�m@��@dZ@"�@�H@��@�\@M�@��@�@�@��@G�@�@%@��@r�@Q�@Q�@1'@b@��@�@|�@;d@+@�@��@�y@ȴ@��@E�@{@�@@��@p�@/@��@�@�/@��@�D@j@j@I�@�@��@�
@t�@C�@33@"�@�H@��@�\@^5@=q@�@�@�^@��@X@7L@&�@%@�`@��@�9@�9@��@�@A�@  @�@��@��@|�@l�@K�@;d@+@��@�y@ȴ@ȴ@ȴ@�R@v�@@�-@�h@`B@?}@V@�@�/@�j@j@�@�@�@ƨ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�#B	�B	�B	�B	�#B	�B	�B	�)B	�/B	�/B	�HB	�B	�B	�B	�B	�B	�B	��B
  B

=B
"�B
@�B
E�B
[#B
^5B
ffB
l�B
o�B
p�B
u�B
s�B
}�B
�oB
��B
��B
�B
��B
��B
�B
�B
�#B
�TB
�B
�BB�B �B+B<jBZBp�B�B�oB�wBÖB��B��B�B�ZB+BE�BcTB�B�DB�\B�bB��B�{B}�B^5BN�BB�B�B�B�#BB�FB�3B�B=qBQ�B^5BgmB��B�B��B_;BC�BC�Br�B�7B�DB�\B�JB�7B�1B�1B�B� B{�Br�B\)BVBR�BQ�BgmBs�Bm�BO�B0!BPB��B�7B��B�yB�B�B)�B
�NB
��B
��B
�B
|�B
t�B
]/B
?}B
0!B
%�B
�B
JB
B
PB
	7B
B	�mB	�qB	o�B	<jB	�B	oB	B�yB�HB�B��B�'B��B��B�{B�bB�+B�B�B�B�1B��B��B�3B�qB�XB�!B�-B��B��B�PB�7B�B�B� B}�B}�B~�By�Bw�Bk�BdZBaHBYBR�BR�BXB\)BT�BL�BL�BP�BO�BK�BI�BH�BG�BE�BB�B@�B?}B=qB<jB<jB<jB9XB49B33B2-B0!B-B+B-B-B0!B/B.B1'B0!B1'B/B,B)�B$�B!�B�B�B!�B#�B$�B�B�B�B�B$�B$�B!�B�B�B�B�B'�B1'B49B49B49B6FB7LB49B2-B&�B�B�B�B"�B'�B-B/B7LB7LB7LB7LB6FB9XB;dB=qB?}B@�B@�BA�B@�BA�BA�BD�BI�BJ�BL�BO�BVBW
BXBYBZBZBZBYB[#B[#B[#B\)B^5BaHBe`BiyBr�Bu�Bu�Bv�Bw�Bx�Bx�Bz�B{�B|�B~�B�B�B�%B�%B�+B�JB�VB�\B�hB��B��B��B��B��B��B��B��B��B��B�B�B�'B�9B�FB�RB�dB�dB�XB�jB�wB�}B��BĜBƨBȴB��B��B�B�B�#B�)B�/B�BB�ZB�sB�B�B�B�B�B�B�B��B	  B	%B	+B		7B	PB	VB	\B	uB	�B	�B	�B	�B	�B	!�B	$�B	)�B	/B	0!B	1'B	33B	49B	9XB	;dB	=qB	A�B	C�B	F�B	I�B	J�B	K�B	N�B	R�B	YB	[#B	\)B	^5B	`BB	ffB	hsB	hsB	ffB	ffB	jB	m�B	n�B	n�B	o�B	o�B	n�B	n�B	o�B	o�B	q�B	u�B	w�B	x�B	x�B	z�B	� B	�B	�B	�+B	�1B	�PB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�9B	�FB	�LB	�RB	�^B	�jB	�jB	�jB	�jB	�}B	��B	��B	B	ĜB	ŢB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B
DB
DB
JB
JB
JB
PB
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
hB
hB
oB
oB
uB
oB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
ZB
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	�]B	��B	��B	��B	��B	��B	��B	�B	�QB	�oB	�vB	�vB	�iB	�vB	��B	��B

	B
"�B
@OB
EmB
Z�B
^B
f2B
lWB
oiB
poB
u�B
s�B
}�B
�:B
�xB
��B
��B
�UB
бB
��B
��B
��B
� B
�QB
�|B�BYB �B*�B<6BY�BpoB��B�:B�BB�aB˒BѷB��B�&B�BEmBc B��B�B�(B�.B�MB�FB}�B^BN�BB[BkB�oB��B�[B�B��B�|B=<BQ�B^BgB��B��B�qB^�BCGBCaBraB�B��B�(B�B��B��B��B��B�B{�Br|B[�BU�BR�BQ�Bg8BshBm]BO�B/�BB�jB�BңB�*B��B��B)�B
��B
˒B
��B
��B
|�B
t�B
\�B
?.B
/�B
%�B
_B
B
�B
B
�B
�B	�8B	�"B	oOB	<6B	QB	:B	�B�DB��B��B�;B��B�jB�_B�,B�B��B��B��B��B��B�2B�vB��B�<B�	B��B��B��B�KB�B�B��B��B�B}�B}�B~�By�Bw�Bk6BdB`�BX�BR�BR�BW�B[�BT�BL~BL~BP�BO�BKxBIlBHfBG_BESBB[B@4B?HB="B<6B<6B<B9	B3�B2�B1�B/�B,�B*�B,�B,�B/�B.�B-�B0�B/�B0�B.�B+�B)�B$�B!|BpBpB!|B#�B$�BjBKBWBdB$�B$�B!|B7B9B?BdB'�B0�B3�B3�B3�B5�B6�B3�B1�B&�BjB7BQB"�B'�B,�B.�B6�B6�B6�B6�B5�B9	B;B="B?.B@4B@4BA;B@BA;BA;BDMBIlBJrBL~BOvBU�BV�BW�BX�BY�BY�BY�BX�BZ�BZ�BZ�B[�B]�B`�BeBiBrGButButBvzBw�Bx�Bx�Bz�B{�B|�B~�B��B��B��B��B��B��B�B�B�B�?B�WB�]B�dB�pB�|B��B�hB��B��B��B��B��B��B��B�B�B�B�	B�B�(B�B�;B�3B�YB�fB̈́BөB��B��BںB��B��B��B�B�$B�6B�"B�CB�AB�[B�[B�aB��B��B	�B	�B	�B	B	B	B	&B	$B	EB	QB	]B	VB	!|B	$tB	)�B	.�B	/�B	0�B	2�B	3�B	9	B	;B	="B	A B	CGB	FYB	IlB	JrB	KxB	N�B	R�B	X�B	Z�B	[�B	]�B	_�B	fB	h$B	h
B	e�B	fB	j0B	mCB	nIB	nIB	oOB	oOB	nIB	nIB	o5B	o5B	q[B	uZB	wfB	x�B	xlB	z�B	�B	��B	��B	��B	��B	�B	�B	��B	�B	�?B	�EB	�KB	�7B	�QB	�WB	�dB	�vB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�4B	�;B	�AB	�MB	�9B	�_B	�fB	�fB	�lB	�lB	�rB	�rB	�~B	�~B	̈́B	�pB	�vB	ЗB	ңB	ӏB	ԕB	յB	֡B	خB	��B	ںB	ںB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�$B	�B	�0B	�"B	�/B	�IB	�OB	�UB	�UB	�OB	�UB	�aB	�nB	�nB	�ZB	�tB	�`B	�`B	�zB	��B	�lB	�xB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
B
B
�B
�B
B
B
B
B
B
B
�B
B
B
B
 B
 B
 B
 B
B
B
&B
B
,B
B
,B
,B
2B
2B
9B
B
?B
EB
EB
EB
EB
1B
1B
1B
EB
EB
KB
QB
7B
7B
WB
CB
]B
]B
]B
]B
]B
dB
dB
jB
pB
 vB
 vB
 vB
!|B
"hB
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$tB
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
3�B
4�B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
8B
8B
7�B
8B
7�B
8B
9	B
8�B
8�B
8�B
:B
:B
:B
:B
;B
<B
<B
<B
<B
<B
<B
=B
="B
="B
=B
="B
>(B
>(B
>(B
>B
>(B
>(B
>B
?.B
?.B
?B
?.B
?B
?B
?B
?.B
@B
@4B
@4B
@4B
A B
A B
A B
A B
BAB
BAB
BAB
BAB
B'B
BAB
CGB
C-B
CGB
CGB
DMB
C-B
CGB
DMB
DMB
DMB
DMB
ESB
ESB
E9B
ESB
ESB
ESB
ESB
ESB
F?B
FYB
FYB
G_B
G_B
G_B
G_B
G_B
G_B
HfB
HfB
HfB
HKB
HfB
HfB
IlB
IRB
IRB
IlB
IRB
IlB
IRB
JrB
JrB
JrB
JrB
KxB
KxB
KxB
KxB
KxB
L~B
L~B
L~B
LdB
M�B
M�B
M�B
MjB
MjB
N�B
N�B
N�B
N�B
N�B
O�B
OvB
OvB
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Z�B
Z�B
Y�B
Z�B
Z�B
Z�B
[�B
[�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
cB
a�B
cB
cB
b�B
cB
dB
dB
dB
c�B
dB
dB
eB
eB
eB
eB
d�B
eB
fB
fB
e�B
fB
fB
fB
fB
gB
gB
gB
gB
gB
h$B
h$B
h$B
h$B
h$B
iB
i*B
iB
i*B
i*B
i*B
i*B
i*B
iB
i*B
j0B
j0B
j0B
jB
j0B
j0B
jB
j0B
jB
j0B
j0B
j0B
kB
kB
k6B
l=B
l=B
l=B
l=B
l=B
l=B
l=B
m)B
mCB
mCB
m)B
m)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.64(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201905170032592019051700325920190517003259201905180025102019051800251020190518002510JA  ARFMdecpA19c                                                                20190512093657  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190512003733  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190512003735  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190512003735  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190512003736  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190512003736  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190512003736  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190512003736  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190512003737  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190512003737                      G�O�G�O�G�O�                JA  ARUP                                                                        20190512070031                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190512153542  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190516153259  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190516153259  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190517152510  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120031518                      G�O�G�O�G�O�                