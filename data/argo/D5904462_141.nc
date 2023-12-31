CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:46Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125946  20190405100754  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @���[h�1   @���$	�@/߾vȴ9�d�x���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT33CU�fCW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dys3D��D�P D�vfD���D�fD�S3D�� D��fD��3D�I�D���D�� D�	�D�9�DږfD��3D�	�D�I�D�i�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @�  A  A(  AH  Ah  A�  A�  A�  A�  A�  A�  A�  A�  B  B
  BffB  B"  B*  B2  B:  BB  BJ  BR  BZ  Bb  Bj  Br  Bz  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  C � C� C� C� C� C
��C� C� C� C� C� C� C� C� C� C� C � C"� C$� C&� C(� C*� C,� C.� C0� C2� C4� C6� C8� C:� C<� C>� C@� CB� CD� CF� CH� CJ� CL� CN� CP� CR��CT�3CVffCXffCZ� C\� C^� C`� Cb� Cd� Cf� Ch� Cj� Cl� Cn� Cp� Cr� Ct� Cv� Cx� Cz� C|� C~� C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt��Dy�3D�)�D�` D��fD�ɚD�&fD�c3D�� D��fD�3D�Y�D���D�� D��D�I�DڦfD��3D��D�Y�D�y�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��/A��A�jA�"�A晚A�r�A�I�A�hA���A�+A�S�A�K�A�-A��A�\)A�-A��A���A�l�A�A���A�jA�"�A�^A��
A�M�A���A�p�A���Aޥ�A�
=A�r�A��A��;AܾwA�^5A��A��mA���Aۉ7A�jA��Aٗ�AؑhA�-A�p�A�$�A�r�A��mAҙ�A�-A��A��`A�bNA͡�A̍PA�+AɼjA�9XAȃAǥ�A�O�A��AŋDA���A�&�A�;dA�ƨA��A�Q�A��hA���A��A���A��A���A�r�A�"�A���A�JA��A��wA�;dA�1'A�t�A�G�A�A�A��hA�(�A�ZA�%A��A���A�K�A��^A�~�A�~�A��A��-A�ĜA�ĜA��`A~��Az1Ax  Au�#Ap  Ah5?Ac&�A`-A^�AZz�AX-AU��AT�DAS`BAR5?AQl�AO��AM/AK%AH��AG7LAE��AD��AD$�AB�!A?��A=�A<�A:�A7ƨA6n�A3�A3+A1&�A.�9A-C�A+��A+/A+�A(��A&=qA%?}A$�A$�\A#��A!�FA�PA%AI�A�uA��A/A+A�A��A7LAVA�yAM�A��AG�A��A��A�AI�A�A��A
=A��A�9A�A��A�7A`BA%AJAƨA+A
ĜA	�FA	VAĜA��A�AƨA�7A|�AS�A%AjA?}AoAoA
=A��AAz�A(�A��A ��A ��A E�@���@��@��P@�$�@��h@�%@�
=@�E�@�^5@�-@���@�1'@��
@�dZ@�=q@�@���@�A�@���@�Z@��@�&�@�j@�  @�K�@�+@�M�@�h@�(�@�ff@�9X@���@��@�j@�O�@��@�V@���@�h@�x�@�hs@��@�u@�Q�@睲@旍@�$�@���@�7@���@�p�@�A�@�R@�v�@��@�@�h@��@�(�@��;@�K�@ާ�@�hs@ۅ@�$�@؃@� �@�  @���@�1@��@�dZ@�
=@��@�X@�&�@�1'@��@���@�n�@���@�`B@�O�@���@�|�@���@�v�@Ώ\@ΰ!@���@���@Χ�@�n�@͙�@��/@Η�@�E�@��#@͡�@�V@�r�@ˮ@ʟ�@���@�G�@�j@��@���@�+@��@�"�@�
=@��@�J@őh@�?}@�%@�Q�@�S�@��@+@���@��9@���@�t�@�33@�o@���@�n�@�M�@�{@�J@���@��#@��T@��T@���@��h@�O�@���@���@��j@�z�@�(�@�  @��F@�|�@�t�@�t�@�|�@�l�@�|�@�@��+@�J@���@��@���@�hs@�?}@��@�%@��`@�j@�b@��@���@�o@��T@��^@���@��@�r�@�1@��@�+@���@��+@�@��/@�A�@��@��@�9X@�1'@��@�|�@�o@���@���@��^@��h@��@���@��@�  @���@�dZ@�;d@��y@�^5@�$�@��@���@�X@��`@�bN@���@�;d@�ȴ@��!@�~�@�5?@��@��T@���@��h@�X@���@�1'@���@�t�@�dZ@�K�@�33@��@��\@��^@��h@��h@��h@��7@�hs@��@���@��`@�r�@��
@��@�C�@�
=@�$�@���@�@�@��-@���@�G�@��@���@�Z@�9X@� �@��@��w@�\)@���@�~�@�=q@���@���@�1'@�b@�b@�b@�1@�1@��m@��F@�dZ@�+@�@�ȴ@�~�@�$�@���@���@�X@�G�@�&�@�z�@��m@��@�C�@���@���@��H@��T@|�/@tI�@g��@^�R@T�/@L�@E�-@?�@7\)@0�@'\)@�y@��@V@��@@
�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��/A��A�jA�"�A晚A�r�A�I�A�hA���A�+A�S�A�K�A�-A��A�\)A�-A��A���A�l�A�A���A�jA�"�A�^A��
A�M�A���A�p�A���Aޥ�A�
=A�r�A��A��;AܾwA�^5A��A��mA���Aۉ7A�jA��Aٗ�AؑhA�-A�p�A�$�A�r�A��mAҙ�A�-A��A��`A�bNA͡�A̍PA�+AɼjA�9XAȃAǥ�A�O�A��AŋDA���A�&�A�;dA�ƨA��A�Q�A��hA���A��A���A��A���A�r�A�"�A���A�JA��A��wA�;dA�1'A�t�A�G�A�A�A��hA�(�A�ZA�%A��A���A�K�A��^A�~�A�~�A��A��-A�ĜA�ĜA��`A~��Az1Ax  Au�#Ap  Ah5?Ac&�A`-A^�AZz�AX-AU��AT�DAS`BAR5?AQl�AO��AM/AK%AH��AG7LAE��AD��AD$�AB�!A?��A=�A<�A:�A7ƨA6n�A3�A3+A1&�A.�9A-C�A+��A+/A+�A(��A&=qA%?}A$�A$�\A#��A!�FA�PA%AI�A�uA��A/A+A�A��A7LAVA�yAM�A��AG�A��A��A�AI�A�A��A
=A��A�9A�A��A�7A`BA%AJAƨA+A
ĜA	�FA	VAĜA��A�AƨA�7A|�AS�A%AjA?}AoAoA
=A��AAz�A(�A��A ��A ��A E�@���@��@��P@�$�@��h@�%@�
=@�E�@�^5@�-@���@�1'@��
@�dZ@�=q@�@���@�A�@���@�Z@��@�&�@�j@�  @�K�@�+@�M�@�h@�(�@�ff@�9X@���@��@�j@�O�@��@�V@���@�h@�x�@�hs@��@�u@�Q�@睲@旍@�$�@���@�7@���@�p�@�A�@�R@�v�@��@�@�h@��@�(�@��;@�K�@ާ�@�hs@ۅ@�$�@؃@� �@�  @���@�1@��@�dZ@�
=@��@�X@�&�@�1'@��@���@�n�@���@�`B@�O�@���@�|�@���@�v�@Ώ\@ΰ!@���@���@Χ�@�n�@͙�@��/@Η�@�E�@��#@͡�@�V@�r�@ˮ@ʟ�@���@�G�@�j@��@���@�+@��@�"�@�
=@��@�J@őh@�?}@�%@�Q�@�S�@��@+@���@��9@���@�t�@�33@�o@���@�n�@�M�@�{@�J@���@��#@��T@��T@���@��h@�O�@���@���@��j@�z�@�(�@�  @��F@�|�@�t�@�t�@�|�@�l�@�|�@�@��+@�J@���@��@���@�hs@�?}@��@�%@��`@�j@�b@��@���@�o@��T@��^@���@��@�r�@�1@��@�+@���@��+@�@��/@�A�@��@��@�9X@�1'@��@�|�@�o@���@���@��^@��h@��@���@��@�  @���@�dZ@�;d@��y@�^5@�$�@��@���@�X@��`@�bN@���@�;d@�ȴ@��!@�~�@�5?@��@��T@���@��h@�X@���@�1'@���@�t�@�dZ@�K�@�33@��@��\@��^@��h@��h@��h@��7@�hs@��@���@��`@�r�@��
@��@�C�@�
=@�$�@���@�@�@��-@���@�G�@��@���@�Z@�9X@� �@��@��w@�\)@���@�~�@�=q@���@���@�1'@�b@�b@�b@�1@�1@��m@��F@�dZ@�+@�@�ȴ@�~�@�$�@���@���@�X@�G�@�&�@�z�@��m@��@�C�@���@���@��H@��T@|�/@tI�@g��@^�R@T�/@L�@E�-@?�@7\)@0�@'\)@�y@��@V@��@@
�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�\B�\B�\B�\B�VB�DB�7B~�B�7B��BB�B��B	JB	DB	hB	�B	"�B	8RB	E�B	iyB	�PB	�B
�B
K�B
|�B
�\B
�VB
�JB
�7B
�B
�B
� B
~�B
|�B
u�B
t�B
p�B
m�B
iyB
^5B
ZB
YB
K�B
L�B
ZB
J�B
>wB
$�B
VB
PB
�B
@�B
L�B
YB
cTB
{�B
�RB
��B
�/B
��BhB�B)�B>wBP�B<jB+B33Bp�Bt�B�\B��B��B��B��B�BƨB��B��B�B�B�B�B�#B��BǮB�'B�uB}�Bk�B]/BJ�B�B
�B
��B
�VB
p�B
K�B
7LB
�B	��B	�wB	��B	��B	�1B	dZB	=qB	%�B	�B		7B	1B	
=B	hB		7B	B��B��B��B��B�B�B�TB�HB�HB�HB�;B�;B�BB�HB�;B�B��B��B��B��B��BǮBŢBŢBŢB�wB�RB�LB�RB�jB�}B�LB�dB�wB��B��B��B��BB��B��BBƨB��B��B�
B�B�
B�
B�B�HB�fB�B��B��B	B	1B	1B	+B	%B	B	B	B	JB	VB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	'�B	-B	0!B	2-B	<jB	?}B	B�B	B�B	E�B	K�B	O�B	Q�B	R�B	L�B	E�B	@�B	>wB	=qB	A�B	D�B	K�B	Q�B	W
B	XB	\)B	_;B	aHB	`BB	aHB	aHB	`BB	`BB	\)B	\)B	^5B	_;B	ffB	o�B	n�B	n�B	m�B	�B	��B	��B	��B	��B	��B	�XB	�jB	�qB	�}B	�}B	�wB	��B	��B	ÖB	ŢB	ƨB	ŢB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	��B	��B	��B	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�#B	�B	�
B	�B	�TB	�fB	�fB	�B	�B	�B	�yB	�mB	�fB	�`B	�ZB	�TB	�TB	�NB	�TB	�TB	�ZB	�mB	�mB	�fB	�`B	�ZB	�ZB	�TB	�ZB	�ZB	�NB	�;B	�;B	�BB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B	��B	��B	��B
B
%B
%B
%B
+B
1B
	7B
	7B
1B
	7B

=B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B
	7B

=B

=B

=B
	7B
	7B
	7B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
DB
JB
\B
bB
hB
oB
oB
oB
oB
oB
oB
oB
hB
bB
bB
VB
VB
VB
VB
VB
VB
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
\B
hB
oB
oB
uB
oB
�B
%�B
/B
33B
9XB
>wB
D�B
H�B
N�B
S�B
W
B
^5B
bNB
iyB
n�B
r�B
v�B
z�B
}�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�4B�8B�7B�:B�1B� B�B~�B�B��B�iB��B��B	$B	B	EB	vB	"�B	8*B	E}B	iUB	�)B	��B
�B
K�B
|�B
�6B
�1B
�%B
�B
��B
��B
�B
~�B
|�B
u�B
t�B
p|B
mgB
iSB
^B
Y�B
X�B
K�B
L�B
Y�B
J�B
>MB
$�B
0B
)B
�B
@\B
L�B
X�B
c*B
{�B
�+B
ʘB
�B
��B<BoB)�B>PBP�B<@B*�B3BpyBt�B�3B��B��B��B��B��B�~BʔBͪB��B��B��B��B��BзBǁB��B�HB}�BkVB]BJ�BfB
��B
��B
�#B
psB
K�B
7B
~B	�B	�DB	��B	�TB	��B	d&B	=>B	%�B	KB		B	�B	
B	3B		 B	�B��B��B��B��B�iB�HB�B�B�B�B�B�B�B�B�B��BѲBЭBѲBЫBˌB�tB�iB�hB�iB�=B�B�B�B�.B�BB�B�)B�<B�QB�HB�NB�MB�SB�MB�MB�WB�kBѳB��B��B��B��B��B��B�B�)B�VB��B��B	�B	�B	�B	�B	�B	�B	 �B	�B	B	B	@B	LB	QB	SB	^B	iB	vB	uB	B	 �B	!�B	'�B	,�B	/�B	1�B	<)B	?@B	BRB	BRB	EbB	K�B	O�B	Q�B	R�B	L�B	EgB	@FB	>;B	=4B	AJB	D^B	K�B	Q�B	V�B	W�B	[�B	^�B	a
B	`B	a	B	a	B	`B	`B	[�B	[�B	]�B	^�B	f'B	o_B	nZB	nYB	mSB	��B	�AB	�iB	�nB	�zB	��B	�B	�,B	�3B	�?B	�?B	�7B	�DB	�KB	�XB	�cB	�kB	�cB	�bB	ʂB	ҴB	ҶB	ѯB	ΛB	ΛB	͖B	ϠB	ӻB	��B	��B	��B	��B	��B	��B	ҵB	ΙB	ʂB	�uB	�tB	�{B	͒B	ΚB	ЦB	ѬB	ЦB	ѭB	ѮB	ϞB	͕B	̎B	˅B	ʂB	ΛB	ϞB	ϠB	ΙB	͕B	ϡB	ҳB	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�$B	�@B	�DB	�AB	�:B	�.B	�&B	�!B	�B	�B	�B	�B	�B	�B	�B	�,B	�,B	�%B	�!B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�)B	�7B	�?B	�@B	�GB	�GB	�EB	�GB	�EB	�GB	�FB	�EB	�FB	�KB	�NB	�JB	�RB	�XB	�XB	�WB	�]B	�dB	�kB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
�B
	�B
	�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
B
B
B
B
B
	B
B
!B
&B
/B
.B
0B
1B
/B
1B
.B
'B
 B
 B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
B
(B
0B
0B
3B
1B
XB
%�B
.�B
2�B
9B
>6B
DYB
HrB
N�B
S�B
V�B
]�B
bB
i8B
nWB
rmB
v�B
z�B
}�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.5 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007542019040510075420190405100754  AO  ARCAADJP                                                                    20181121125946    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125946  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125946  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100754  IP                  G�O�G�O�G�O�                