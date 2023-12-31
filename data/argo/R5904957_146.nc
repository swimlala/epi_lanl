CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:32Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I`   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R<   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  YP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       [   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       k   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  r   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140832  20181024140832  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��%i�|1   @��%�i�@5A���o�c����F1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @9��@y��@�  A��A!��AA��Aa��A�  A�  A�33A�  A�  A�  A�33A�  B ffBffB  B  B   B(  B/��B8  B@  BH  BP  BX  B`ffBg��Bp  Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C�C  C  C�fC   C"  C$  C&  C'�fC)�fC+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCs�fCv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��C��C��C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3D � D�fD  D� D  D� D  D� D  Dy�D��Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D�fD   D � D!fD!�fD"fD"�fD#fD#�fD$fD$� D%  D%�fD&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,y�D,��D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D2��D3y�D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D9��D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DG��DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DL��DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR��DSy�DS��DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZfDZ� D[  Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDlfDl� Dm  Dmy�Dm��Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dt� DufDu� Dv  Dv� Dw  Dwy�Dw�fDy\D�VD��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @I��@���@�  A��A%��AE��Ae��A�  A�  A�33A�  A�  A�  A�33A�  BffB	ffB  B  B!  B)  B0��B9  BA  BI  BQ  BY  BaffBh��Bq  Bx��B�L�B�� B�� B�� B�� B�� B�� B�� B�� B�� B�L�B�� B�� B�� B�� B�� B��3BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ C@ C@ C&fC@ C@ C@ CY�C@ C@ C&fC @ C"@ C$@ C&@ C(&fC*&fC,&fC.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF&fCH@ CJ@ CL@ CN@ CP@ CRY�CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr&fCt&fCv@ Cx@ Cz@ C|@ C~@ C�,�C�  C�  C�  C�  C�,�C�  C�  C�  C�,�C�,�C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�3C�  C�3C�  C�,�C�  C�  C�3C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�3C�3C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�3C�  C�  C�  C�  C�3C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�,�C�  C�  C�  C�,�C�,�C�,�C�  C�,�C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�,�C�  D 	�D � D�fD D� D D� D D� D D��D	�D��D	�D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D	�D� D D�fD  D � D!fD!�fD"fD"�fD#fD#�fD$fD$� D% D%�fD& D&� D'fD'� D( D(� D) D)� D* D*� D+ D+��D, D,��D-	�D-� D. D.� D/ D/�fD0 D0� D1 D1� D2 D2� D3	�D3��D4 D4� D5 D5� D6 D6� D7 D7� D8 D8� D9 D9� D:	�D:� D; D;� D< D<� D= D=� D> D>� D? D?� D@ D@� DA DA� DB DB� DC DC� DDfDD� DE DE� DF DF� DG DG� DH	�DH��DI DI� DJ DJ� DK DK� DL DL� DM	�DM� DN DN� DO DO� DP DP� DQ DQ� DR DR� DS	�DS��DT	�DT� DU DU� DV DV� DW DW� DX DX� DY DY�fDZfDZ� D[ Dg Dg� Dh Dh� Di Di� Dj Dj� Dk Dk�fDlfDl� Dm Dm��Dn	�Dn� Do Do�fDp Dp� Dq Dq� Dr Dr� Ds Ds� Dt	�Dt� DufDu� Dv Dv� Dw Dw��Dw�fDy�\D�^D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�1A�
=A�JA�VA�VA�bA�bA�oA�oA�VA�bA�bA�oA�{A��A��A��A��A��A��A��A��A��A��A��A�{A�bA��
A֮A��`A�%A���A�?}AŰ!A�%A��\A�  A�v�A���A�=qA�M�A���A��DA���A�oA�E�A�VA�A�(�A��mA�\)A��hA�C�A��A�$�A��PA�\)A�oA���A�?}A���A�C�A��\A��RA�;dA�5?A�1A�|�A��FA��mA�;dA�1'A�-A��A�A�A�XA��A�"�A�\)A��A�x�A�  A� �A�~�A���A��\A�7LA�r�A��A�33A~r�A|�yA{7LAx��AvffAr�!Aq�Ao�7An(�Am�^Al��AihsAcVAaG�A`(�A]O�AZ�\AX�AV��AU�AT�uARĜAP�`AP1'AM|�AKAJ��AI�AH �AH  AGC�AFJAD�HACK�AA��A@{A?�wA?�A=��A;�wA;G�A:�DA7�PA4M�A3;dA2�yA2�jA2v�A1��A1p�A0�9A01A-l�A,Q�A,  A+��A+33A)A(5?A'VA%ƨA#�^A"I�A!��A ��A 5?A 1A��A��A�\A�AQ�AK�A�A�9Az�AI�AK�AffA�A�-A��AjAE�A-AJA��AAE�At�AbNAx�A1'A
Q�A	��A	&�A�HAA�A��A��A�AĜA�^A33A��AZA��A+A Q�@��w@��j@�z�@�9X@���@���@��@��@��R@�^5@��@��@�bN@�33@�hs@���@�r�@�|�@��-@�j@�1@�"�@�~�@�{@�@��@�|�@���@�\)@��@���@���@���@�\@�n�@�^5@�{@�-@�p�@��@�Ĝ@��@�5?@�V@��@��y@�?}@��`@�1'@���@��@���@�;d@�=q@��@�/@���@��-@��/@��@��u@��D@�Z@��@�  @�A�@��u@��@�S�@��\@��T@���@�E�@�(�@��`@���@�J@��@��^@�x�@�G�@�/@�Ĝ@���@�z�@�I�@�b@���@�"�@���@��@�(�@�  @�  @�l�@��@���@�O�@�Ĝ@�z�@�Q�@��m@���@�;d@���@��@�hs@��/@�Q�@�  @��
@��
@���@���@��@�\)@��@���@�V@�E�@�$�@���@��@��@�%@�r�@��@���@�S�@��@�hs@�/@�`B@���@���@��@��@��@���@��@��@��@�K�@�o@��y@�$�@�@��T@��T@��T@��@��@��T@��-@�?}@�?}@��^@�$�@�ff@���@��@���@��@�v�@�E�@��@�@���@�p�@�`B@�V@���@�I�@�(�@�1@���@�C�@�
=@���@��+@�n�@�{@���@�O�@��@��j@�9X@�b@��@��@�33@��\@�n�@�=q@�{@���@�`B@���@�j@�A�@�(�@��@���@��P@�|�@�t�@�S�@��@�~�@�^5@�=q@�-@��@��@��@���@��-@�p�@�G�@��@��@��@���@��/@���@��9@���@��D@�z�@�r�@�j@�bN@�Q�@�Q�@� �@�ƨ@�ƨ@��w@��F@��@���@���@��P@��@�l�@�dZ@�dZ@�dZ@�dZ@�"�@��Y@�w�@k��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�1A�
=A�JA�VA�VA�bA�bA�oA�oA�VA�bA�bA�oA�{A��A��A��A��A��A��A��A��A��A��A��A�{A�bA��
A֮A��`A�%A���A�?}AŰ!A�%A��\A�  A�v�A���A�=qA�M�A���A��DA���A�oA�E�A�VA�A�(�A��mA�\)A��hA�C�A��A�$�A��PA�\)A�oA���A�?}A���A�C�A��\A��RA�;dA�5?A�1A�|�A��FA��mA�;dA�1'A�-A��A�A�A�XA��A�"�A�\)A��A�x�A�  A� �A�~�A���A��\A�7LA�r�A��A�33A~r�A|�yA{7LAx��AvffAr�!Aq�Ao�7An(�Am�^Al��AihsAcVAaG�A`(�A]O�AZ�\AX�AV��AU�AT�uARĜAP�`AP1'AM|�AKAJ��AI�AH �AH  AGC�AFJAD�HACK�AA��A@{A?�wA?�A=��A;�wA;G�A:�DA7�PA4M�A3;dA2�yA2�jA2v�A1��A1p�A0�9A01A-l�A,Q�A,  A+��A+33A)A(5?A'VA%ƨA#�^A"I�A!��A ��A 5?A 1A��A��A�\A�AQ�AK�A�A�9Az�AI�AK�AffA�A�-A��AjAE�A-AJA��AAE�At�AbNAx�A1'A
Q�A	��A	&�A�HAA�A��A��A�AĜA�^A33A��AZA��A+A Q�@��w@��j@�z�@�9X@���@���@��@��@��R@�^5@��@��@�bN@�33@�hs@���@�r�@�|�@��-@�j@�1@�"�@�~�@�{@�@��@�|�@���@�\)@��@���@���@���@�\@�n�@�^5@�{@�-@�p�@��@�Ĝ@��@�5?@�V@��@��y@�?}@��`@�1'@���@��@���@�;d@�=q@��@�/@���@��-@��/@��@��u@��D@�Z@��@�  @�A�@��u@��@�S�@��\@��T@���@�E�@�(�@��`@���@�J@��@��^@�x�@�G�@�/@�Ĝ@���@�z�@�I�@�b@���@�"�@���@��@�(�@�  @�  @�l�@��@���@�O�@�Ĝ@�z�@�Q�@��m@���@�;d@���@��@�hs@��/@�Q�@�  @��
@��
@���@���@��@�\)@��@���@�V@�E�@�$�@���@��@��@�%@�r�@��@���@�S�@��@�hs@�/@�`B@���@���@��@��@��@���@��@��@��@�K�@�o@��y@�$�@�@��T@��T@��T@��@��@��T@��-@�?}@�?}@��^@�$�@�ff@���@��@���@��@�v�@�E�@��@�@���@�p�@�`B@�V@���@�I�@�(�@�1@���@�C�@�
=@���@��+@�n�@�{@���@�O�@��@��j@�9X@�b@��@��@�33@��\@�n�@�=q@�{@���@�`B@���@�j@�A�@�(�@��@���@��P@�|�@�t�@�S�@��@�~�@�^5@�=q@�-@��@��@��@���@��-@�p�@�G�@��@��@��@���@��/@���@��9@���@��D@�z�@�r�@�j@�bN@�Q�@�Q�@� �@�ƨ@�ƨ@��w@��F@��@���@���@��P@��@�l�@�dZ@�dZ@�dZ@�dZ@�"�@��Y@�w�@k��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B
=BoB!�B)�B7LB=qBE�BP�BP�BP�BYBXBT�BR�BO�BM�BL�BH�BE�BA�B6FB/B)�B&�B �B�B�B
=BB�B�/B�
BŢB�-B��Bz�BaHBI�B�B
�mB
�HB
�BB
�B
��B
�RB
�B
��B
��B
�uB
�uB
�PB
u�B
^5B
XB
P�B
I�B
=qB
6FB
+B
�B
{B
	7B	��B	�B	�#B	��B	ȴB	�}B	�^B	�'B	��B	p�B	cTB	YB	I�B	>wB	5?B	2-B	1'B	,B	!�B	�B	bB	B��B��B�B�B�B�B�`B�;B�#B�B�
B�B��B��B��BɺBĜB��B�}B�}B�wB�qB�qB�jB�^B�XB�?B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�FB�LB�FB�FB�9B�3B�'B�'B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�JB�DB�DB�DB�=B�DB�JB�DB�VB�oB�oB�oB�oB�oB�uB�uB�uB�{B�{B�{B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�LB�LB�LB�^B�qB��BBĜBŢBǮBȴB��B�B�
B�B�B�
B�
B�B�#B�NB�sB�B�B�B�B��B	DB	bB	�B	�B	�B	!�B	$�B	%�B	'�B	)�B	)�B	+B	,B	.B	2-B	5?B	5?B	7LB	@�B	B�B	B�B	E�B	H�B	H�B	I�B	K�B	L�B	M�B	O�B	P�B	Q�B	S�B	T�B	W
B	XB	ZB	[#B	\)B	\)B	\)B	]/B	^5B	_;B	aHB	cTB	e`B	gmB	gmB	iyB	jB	o�B	q�B	r�B	s�B	u�B	y�B	�B	�B	�+B	�1B	�DB	�\B	�oB	�uB	�uB	�uB	�oB	�bB	�bB	�bB	�uB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�9B	�?B	�FB	�FB	�FB	�FB	�dB	��B	ÖB	ŢB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�TB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B<jB
  B
B
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
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
oB
	RB
yB
%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B
=BoB!�B)�B7LB=qBE�BP�BP�BP�BYBXBT�BR�BO�BM�BL�BH�BE�BA�B6FB/B)�B&�B �B�B�B
=BB�B�/B�
BŢB�-B��Bz�BaHBI�B�B
�mB
�HB
�BB
�B
��B
�RB
�B
��B
��B
�uB
�uB
�PB
u�B
^5B
XB
P�B
I�B
=qB
6FB
+B
�B
{B
	7B	��B	�B	�#B	��B	ȴB	�}B	�^B	�'B	��B	p�B	cTB	YB	I�B	>wB	5?B	2-B	1'B	,B	!�B	�B	bB	B��B��B�B�B�B�B�`B�;B�#B�B�
B�B��B��B��BɺBĜB��B�}B�}B�wB�qB�qB�jB�^B�XB�?B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�FB�LB�FB�FB�9B�3B�'B�'B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�JB�DB�DB�DB�=B�DB�JB�DB�VB�oB�oB�oB�oB�oB�uB�uB�uB�{B�{B�{B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�LB�LB�LB�^B�qB��BBĜBŢBǮBȴB��B�B�
B�B�B�
B�
B�B�#B�NB�sB�B�B�B�B��B	DB	bB	�B	�B	�B	!�B	$�B	%�B	'�B	)�B	)�B	+B	,B	.B	2-B	5?B	5?B	7LB	@�B	B�B	B�B	E�B	H�B	H�B	I�B	K�B	L�B	M�B	O�B	P�B	Q�B	S�B	T�B	W
B	XB	ZB	[#B	\)B	\)B	\)B	]/B	^5B	_;B	aHB	cTB	e`B	gmB	gmB	iyB	jB	o�B	q�B	r�B	s�B	u�B	y�B	�B	�B	�+B	�1B	�DB	�\B	�oB	�uB	�uB	�uB	�oB	�bB	�bB	�bB	�uB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�9B	�?B	�FB	�FB	�FB	�FB	�dB	��B	ÖB	ŢB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�TB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B<jB
  B
B
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
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
oB
	RB
yB
%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140832                              AO  ARCAADJP                                                                    20181024140832    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140832  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140832  QCF$                G�O�G�O�G�O�0               