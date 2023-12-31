CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2018-07-23T07:13:13Z creation;2018-07-23T07:13:17Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z           9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  D�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        G@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  R@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        U    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        b�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  m�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  {�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        ~@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180723071313  20180723072029  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            7906                            051216                          846 @�n���	1   @�nƒ��@2]p��
=�e�\)1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @���A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:�C<  C=�fC@  CB  CC�fCE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  Dy�D��D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DRfDR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|�fD}fD}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�fD�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @���A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:�C<  C=�fC@  CB  CC�fCE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  Dy�D��D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DRfDR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|�fD}fD}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�fD�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��#A�=qA��A�5?A�7LA�+A�-A�&�A� �A��A�oA�oA�VA�
=A�A�  A���A���A���A��A��A��/A���Aغ^AؓuA��A��Aև+A�r�A�`BA��mA�G�A�A�A�`BA��A�&�A��/AΟ�A·+A�p�A�33A��
A�~�Aɏ\A��AȁA���A�1A�JAċDA�A�p�A���A�A��A�K�A�|�A�M�A��A�|�A��
A��A���A��!A���A�7LA��+A�^5A�Q�A���A��/A��A�K�A�n�A�ĜA�G�A��A�7LA�I�A�{A�A�A�A�-A��FA�l�A�JA�I�A�z�A�A�v�A���A�A��A�r�A�9XA��A��A��
A�"�A�"�A��^A�O�A��A�  A��
A��A��\A��RA��jA�A��!A��RA���A�bNA�$�A��A��A��;A��^A�=qA���A�VA���A��A��;A��A��A�^A{��Aw��Au"�Ar�uAq"�Ap��Ao��AlE�Aj$�Ad�yAa�FA[�mA[�AZz�AX{AVA�AS�FAQ�wAPA�AN^5AL��AJM�AJ�AI;dAH �AC�AB��AAO�A?dZA=O�A:��A6M�A4�uA3XA1�;A/�A.n�A,��A+�wA+��A+33A*��A*=qA)��A)33A(�9A(��A)�A(^5A'�A&�/A%��A$=qA#�mA#�-A"��A"(�A ��A��A
=A��Ap�A&�A5?A��A�
A�A;dAO�A��AQ�AS�AjA��A��A�An�A��Al�A-A�^A`BA
��A
��A
��A
�uA
ffA
Q�A	��A�A�7A~�A"�A�uAjA&�A�^A|�A`BA;dAoA ��A �!A �A Q�A 5?@��
@�Q�@�=q@�{@���@��7@�7L@�A�@�x�@�ff@���@�K�@�!@�&�@�  @�l�@�;d@�+@��@�h@�&�@�u@��;@�t�@�%@��H@�ff@�{@ݲ-@�V@���@܃@�I�@ۅ@��@�A�@׍P@��@���@֧�@�^5@�$�@�@�7L@Ԭ@Դ9@�z�@�j@�Z@ӍP@�V@�@�@ѩ�@��`@��H@�/@˥�@ʰ!@ə�@��`@ț�@ȓu@ȃ@�Q�@� �@� �@�b@�1@ƸR@Ƨ�@�~�@�E�@���@��@���@�-@�\)@�  @�t�@��H@ƸR@Ƈ+@�v�@��@�/@ă@��;@��@�~�@���@��j@� �@�|�@��!@��!@�S�@�S�@���@��@��7@��@�~�@�{@��@�$�@��@���@�x�@�?}@�V@��@�Z@���@�33@���@��+@�5?@�-@���@�7L@��`@�r�@���@�ƨ@�dZ@��!@�=q@��-@�X@�7L@���@�j@�  @�ƨ@���@�S�@��@��@�o@���@��+@�V@��@��^@�x�@�X@�7L@�&�@���@�Ĝ@��j@���@��D@��@�z�@�9X@�b@���@��@��;@���@�|�@���@��H@�v�@�M�@�V@�$�@���@�7L@�&�@�V@��j@���@���@��D@�j@�bN@�bN@��m@��P@�S�@�ȴ@���@�=q@��@��T@��#@���@���@��h@�hs@�?}@��`@��u@�z�@�I�@�b@��w@�;d@�"�@��@�ȴ@�=q@���@���@���@�33@��!@�=q@���@�`B@��@��9@� �@��m@�ƨ@�t�@�\)@�K�@���@��!@���@�E�@���@��7@�&�@��/@���@���@���@��@��D@��D@��D@��D@��D@��u@�Z@�b@��m@���@��w@�S�@�"�@�@��H@��R@���@�~�@�n�@�=q@��@���@�X@��@���@��/@���@� �@�1@���@��P@�K�@�+@�
=@���@�~�@�-@�x�@�?}@��@��`@���@���@�(�@�l�@�+@�o@�@�ȴ@�M�@��@���@��@�/@���@��D@�Z@�  @���@�K�@��H@��!@��+@�^5@��@��-@�p�@�O�@�G�@���@�Q�@���@��@�|�@�o@�n�@�5?@�@��#@��7@�/@���@���@���@��D@�Z@���@���@�|�@�33@�ȴ@��R@��!@��\@�=q@�@�x�@�`B@���@�r�@�Z@��@�  @��@~ȴ@~5?@}�@}�@}��@}�@|��@|�@|I�@{��@zJ@y&�@xĜ@x  @w��@vv�@u�T@u@u�-@u�@t�j@s��@sS�@r�@q��@q&�@p��@pbN@pb@o�@o��@o�P@oK�@n��@m�@mO�@l�/@l�D@k��@k@jn�@jM�@j�@i�@i��@iG�@i&�@h�@hbN@hA�@h1'@hb@g��@g�w@g��@gl�@f�y@fV@e�-@e`B@e?}@d��@d1@c�@c33@c@b��@b��@b=q@a�^@aX@`Ĝ@_�w@_l�@^��@^�+@]�@]O�@\�@\9X@[�
@[�F@[��@[dZ@["�@Z��@Z^5@Z=q@ZJ@Y��@Yx�@YX@Y%@X��@X�@XA�@W�;@W+@Vȴ@Vff@U��@U�@T��@Tz�@T1@Sƨ@S��@SS�@R�H@R��@R~�@RM�@R�@Q��@Qx�@QG�@Q%@P��@P1'@O�w@Ol�@O�@N�y@N��@Nv�@NV@N@M�h@MO�@L�@Lz�@L1@L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��#A�=qA��A�5?A�7LA�+A�-A�&�A� �A��A�oA�oA�VA�
=A�A�  A���A���A���A��A��A��/A���Aغ^AؓuA��A��Aև+A�r�A�`BA��mA�G�A�A�A�`BA��A�&�A��/AΟ�A·+A�p�A�33A��
A�~�Aɏ\A��AȁA���A�1A�JAċDA�A�p�A���A�A��A�K�A�|�A�M�A��A�|�A��
A��A���A��!A���A�7LA��+A�^5A�Q�A���A��/A��A�K�A�n�A�ĜA�G�A��A�7LA�I�A�{A�A�A�A�-A��FA�l�A�JA�I�A�z�A�A�v�A���A�A��A�r�A�9XA��A��A��
A�"�A�"�A��^A�O�A��A�  A��
A��A��\A��RA��jA�A��!A��RA���A�bNA�$�A��A��A��;A��^A�=qA���A�VA���A��A��;A��A��A�^A{��Aw��Au"�Ar�uAq"�Ap��Ao��AlE�Aj$�Ad�yAa�FA[�mA[�AZz�AX{AVA�AS�FAQ�wAPA�AN^5AL��AJM�AJ�AI;dAH �AC�AB��AAO�A?dZA=O�A:��A6M�A4�uA3XA1�;A/�A.n�A,��A+�wA+��A+33A*��A*=qA)��A)33A(�9A(��A)�A(^5A'�A&�/A%��A$=qA#�mA#�-A"��A"(�A ��A��A
=A��Ap�A&�A5?A��A�
A�A;dAO�A��AQ�AS�AjA��A��A�An�A��Al�A-A�^A`BA
��A
��A
��A
�uA
ffA
Q�A	��A�A�7A~�A"�A�uAjA&�A�^A|�A`BA;dAoA ��A �!A �A Q�A 5?@��
@�Q�@�=q@�{@���@��7@�7L@�A�@�x�@�ff@���@�K�@�!@�&�@�  @�l�@�;d@�+@��@�h@�&�@�u@��;@�t�@�%@��H@�ff@�{@ݲ-@�V@���@܃@�I�@ۅ@��@�A�@׍P@��@���@֧�@�^5@�$�@�@�7L@Ԭ@Դ9@�z�@�j@�Z@ӍP@�V@�@�@ѩ�@��`@��H@�/@˥�@ʰ!@ə�@��`@ț�@ȓu@ȃ@�Q�@� �@� �@�b@�1@ƸR@Ƨ�@�~�@�E�@���@��@���@�-@�\)@�  @�t�@��H@ƸR@Ƈ+@�v�@��@�/@ă@��;@��@�~�@���@��j@� �@�|�@��!@��!@�S�@�S�@���@��@��7@��@�~�@�{@��@�$�@��@���@�x�@�?}@�V@��@�Z@���@�33@���@��+@�5?@�-@���@�7L@��`@�r�@���@�ƨ@�dZ@��!@�=q@��-@�X@�7L@���@�j@�  @�ƨ@���@�S�@��@��@�o@���@��+@�V@��@��^@�x�@�X@�7L@�&�@���@�Ĝ@��j@���@��D@��@�z�@�9X@�b@���@��@��;@���@�|�@���@��H@�v�@�M�@�V@�$�@���@�7L@�&�@�V@��j@���@���@��D@�j@�bN@�bN@��m@��P@�S�@�ȴ@���@�=q@��@��T@��#@���@���@��h@�hs@�?}@��`@��u@�z�@�I�@�b@��w@�;d@�"�@��@�ȴ@�=q@���@���@���@�33@��!@�=q@���@�`B@��@��9@� �@��m@�ƨ@�t�@�\)@�K�@���@��!@���@�E�@���@��7@�&�@��/@���@���@���@��@��D@��D@��D@��D@��D@��u@�Z@�b@��m@���@��w@�S�@�"�@�@��H@��R@���@�~�@�n�@�=q@��@���@�X@��@���@��/@���@� �@�1@���@��P@�K�@�+@�
=@���@�~�@�-@�x�@�?}@��@��`@���@���@�(�@�l�@�+@�o@�@�ȴ@�M�@��@���@��@�/@���@��D@�Z@�  @���@�K�@��H@��!@��+@�^5@��@��-@�p�@�O�@�G�@���@�Q�@���@��@�|�@�o@�n�@�5?@�@��#@��7@�/@���@���@���@��D@�Z@���@���@�|�@�33@�ȴ@��R@��!@��\@�=q@�@�x�@�`B@���@�r�@�Z@��@�  @��@~ȴ@~5?@}�@}�@}��@}�@|��@|�@|I�@{��@zJ@y&�@xĜ@x  @w��@vv�@u�T@u@u�-@u�@t�j@s��@sS�@r�@q��@q&�@p��@pbN@pb@o�@o��@o�P@oK�@n��@m�@mO�@l�/@l�D@k��@k@jn�@jM�@j�@i�@i��@iG�@i&�@h�@hbN@hA�@h1'@hb@g��@g�w@g��@gl�@f�y@fV@e�-@e`B@e?}@d��@d1@c�@c33@c@b��@b��@b=q@a�^@aX@`Ĝ@_�w@_l�@^��@^�+@]�@]O�@\�@\9X@[�
@[�F@[��@[dZ@["�@Z��@Z^5@Z=q@ZJ@Y��@Yx�@YX@Y%@X��@X�@XA�@W�;@W+@Vȴ@Vff@U��@U�@T��@Tz�@T1@Sƨ@S��@SS�@R�H@R��@R~�@RM�@R�@Q��@Qx�@QG�@Q%@P��@P1'@O�w@Ol�@O�@N�y@N��@Nv�@NV@N@M�h@MO�@L�@Lz�@L1@L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�
B	�
B	��B	��B	ŢB	�-B	�B	��B	�-B	�dB	�HB
!�B
]/B
�
B�B&�B+B5?BO�BYBQ�BT�B�PB��B�-BƨBǮB��B��B(�B33BB�BH�BH�BL�BP�BdZBl�Bp�Bs�Bm�B{�B�B�B�B�JB�JB�DB�hB�\Bu�Bu�Bk�BbNBYBk�Be`BVB9XB�BJBJBJB1B1B�B'�B�BhB	7B��B�B�5B�JBk�BF�B1'B#�B�B\B	7BBB
��B
�B
�`B-B0!B%�B�BJB  B
�B
�TB
��B
��B
��B
ǮB
�LB
��B
��B
�7B
~�B
z�B
w�B
m�B
_;B
G�B
-B
�B

=B	��B	��B	�B	�BB	��B	�'B	��B	o�B	dZB	hsB	^5B	R�B	C�B	8RB	/B	!�B	�B	\B	\B	
=B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B	  B��B	B	DB	bB	{B	#�B	,B	1'B	1'B	2-B	49B	5?B	9XB	8RB	9XB	8RB	8RB	8RB	:^B	=qB	9XB	0!B	"�B	�B	�B	{B	hB	bB	bB	bB	PB	1B	+B	  B��B��B��B��B��B	B	  B	  B	  B	B��B��B��B��B��B��B��B	  B	B	B	B	B	B	B	B	B	B	  B��B�B��B��B��B��B�B�B�B��B��B	B	DB	bB	uB	�B	�B	'�B	+B	/B	33B	6FB	7LB	<jB	<jB	<jB	<jB	<jB	=qB	<jB	<jB	<jB	=qB	<jB	@�B	?}B	?}B	>wB	>wB	@�B	B�B	F�B	G�B	G�B	J�B	K�B	K�B	M�B	Q�B	Q�B	Q�B	P�B	S�B	VB	T�B	P�B	J�B	I�B	H�B	H�B	H�B	I�B	I�B	I�B	J�B	O�B	Q�B	W
B	YB	\)B	^5B	`BB	aHB	cTB	e`B	k�B	t�B	� B	�B	�B	�+B	�+B	�+B	�1B	�1B	�1B	�1B	�1B	�+B	�1B	�7B	�7B	�DB	�DB	�VB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�-B	�3B	�9B	�?B	�RB	�dB	�jB	�wB	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
VB
\B
\B
\B
bB
bB
\B
bB
hB
oB
oB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
%�B
&�B
&�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
(�B
(�B
+B
+B
+B
+B
+B
+B
+B
,B
-B
-B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
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
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
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
=qB
=qB
>wB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�
B	�
B	��B	��B	ŢB	�-B	�B	��B	�-B	�dB	�HB
!�B
]/B
�
B�B&�B+B5?BO�BYBQ�BT�B�PB��B�-BƨBǮB��B��B(�B33BB�BH�BH�BL�BP�BdZBl�Bp�Bs�Bm�B{�B�B�B�B�JB�JB�DB�hB�\Bu�Bu�Bk�BbNBYBk�Be`BVB9XB�BJBJBJB1B1B�B'�B�BhB	7B��B�B�5B�JBk�BF�B1'B#�B�B\B	7BBB
��B
�B
�`B-B0!B%�B�BJB  B
�B
�TB
��B
��B
��B
ǮB
�LB
��B
��B
�7B
~�B
z�B
w�B
m�B
_;B
G�B
-B
�B

=B	��B	��B	�B	�BB	��B	�'B	��B	o�B	dZB	hsB	^5B	R�B	C�B	8RB	/B	!�B	�B	\B	\B	
=B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B	  B��B	B	DB	bB	{B	#�B	,B	1'B	1'B	2-B	49B	5?B	9XB	8RB	9XB	8RB	8RB	8RB	:^B	=qB	9XB	0!B	"�B	�B	�B	{B	hB	bB	bB	bB	PB	1B	+B	  B��B��B��B��B��B	B	  B	  B	  B	B��B��B��B��B��B��B��B	  B	B	B	B	B	B	B	B	B	B	  B��B�B��B��B��B��B�B�B�B��B��B	B	DB	bB	uB	�B	�B	'�B	+B	/B	33B	6FB	7LB	<jB	<jB	<jB	<jB	<jB	=qB	<jB	<jB	<jB	=qB	<jB	@�B	?}B	?}B	>wB	>wB	@�B	B�B	F�B	G�B	G�B	J�B	K�B	K�B	M�B	Q�B	Q�B	Q�B	P�B	S�B	VB	T�B	P�B	J�B	I�B	H�B	H�B	H�B	I�B	I�B	I�B	J�B	O�B	Q�B	W
B	YB	\)B	^5B	`BB	aHB	cTB	e`B	k�B	t�B	� B	�B	�B	�+B	�+B	�+B	�1B	�1B	�1B	�1B	�1B	�+B	�1B	�7B	�7B	�DB	�DB	�VB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�-B	�3B	�9B	�?B	�RB	�dB	�jB	�wB	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
VB
\B
\B
\B
bB
bB
\B
bB
hB
oB
oB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
%�B
&�B
&�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
(�B
(�B
+B
+B
+B
+B
+B
+B
+B
,B
-B
-B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
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
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
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
=qB
=qB
>wB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20180723153741  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180723071313  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180723071314  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180723071314  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180723071315  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180723071315  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180723071315  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180723071315  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180723071317  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180723071317                      G�O�G�O�G�O�                JA  ARUP                                                                        20180723072029                      G�O�G�O�G�O�                