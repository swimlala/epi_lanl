CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-06T07:01:05Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20171006070105  20190405100808  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�+�/7�;1   @�+�ffi@-I7KƧ��d�I�^1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   B   @@  @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���C   C  C  C  C  C
  C  C  C  C�C33C�fC�fC�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD�	�D�L�D�y�D��fD�fD�33D��3D�ɚD��D�C3D��3D��fD��D�S3D�|�D��fD��D�S3D�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @fff@�ff@�33A	��A)��AI��Ai��A���A���A���A���A���A���A���A���BffB
ffBffBffB"ffB*ffB2ffB:ffBBffBJffBRffBZffBbffBjffBrffBzffB�33B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�33B�  C ��C��C��C��C��C
��C��C��C��C�4C��C� C� C� C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB�4CD�4CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx�4Cz��C|��C~��C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�D &fD �fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD	&fD	�fD
&fD
�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD &fD �fD!&fD!�fD"&fD"�fD#&fD#�fD$&fD$�fD%&fD%�fD&&fD&�fD'&fD'�fD(&fD(�fD)&fD)�fD*&fD*�fD+&fD+�fD,&fD,�fD-&fD-�fD.&fD.�fD/&fD/�fD0&fD0�fD1&fD1�fD2&fD2�fD3&fD3�fD4&fD4�fD5&fD5�fD6&fD6�fD7&fD7�fD8&fD8�fD9&fD9�fD:&fD:�fD;&fD;�fD<&fD<�fD=&fD=�fD>&fD>�fD?&fD?�fD@&fD@�fDA&fDA�fDB&fDB�fDC&fDC�fDD&fDD�fDE&fDE�fDF&fDF�fDG&fDG�fDH&fDH�fDI&fDI�fDJ&fDJ�fDK&fDK�fDL&fDL�fDM&fDM�fDN&fDN�fDO&fDO�fDP&fDP�fDQ&fDQ�fDR&fDR�fDS&fDS�fDT&fDT�fDU&fDU�fDV&fDV�fDW&fDW�fDX&fDX�fDY&fDY�fDZ&fDZ�fD[&fD[�fD\&fD\�fD]&fD]�fD^&fD^�fD_&fD_�fD`&fD`�fDa&fDa�fDb&fDb�fDc&fDc�fDd&fDd�fDe&fDe�fDf&fDf�fDg&fDg�fDh&fDh�fDi&fDi�fDj&fDj�fDk&fDk�fDl&fDl�fDm&fDm�fDn&fDn�fDo&fDo�fDp&fDp�fDq&fDq�fDr&fDr�fDs&fDs�fDt&fDt� Dy��D��D�` D���D��D��D�FfD��fD���D�0 D�VfD��fD�ٙD�  D�ffDڐ D��D�,�D�ffD��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�FA�9A�FA�RA���A�wA�A�A⟾A♚A�uA�+A�A�v�A�K�A�$�A��A�A���A��A�-A�A�`BA�S�A�C�A�9XA�-A�$�A��A�
=A���A��A��A��TA���A�!Aߴ9Aޟ�A�JAڃA���A�VAՁA�VA�-Aԥ�A�A��A�A�A���A�9XA���Aʡ�Aɧ�A�ƨA�E�A��HA�dZA�K�A�33A�33A�/A�{A�Q�A�A�v�A�bA�VA�x�A�A�A�VA��+A��9A�t�A���A��7A�E�A�XA�hsA�"�A�-A���A�&�A�~�A���A�`BA��9A�~�A��uA�=qA��PA��
A��mA�O�A���A���A�  A���A�A���A�^5A� �A�p�A��9A��A���A��A�=qA�ĜA�9XA�?}A�^5A��A�JA�1'A���A�/A�1A��jA���A�/A~1'Av��AnȴAfQ�A_?}A]�FAY�PAQ�FAMC�AJ�`AIVAF9XABE�A@~�A?ƨA@ȴA@�!A>^5A=�A<z�A;��A;�A:��A9
=A5��A3+A0�A.�\A.$�A.A�A.ZA.��A/�7A.z�A-��A-�A-dZA,I�A*�\A)33A'�7A&��A&�A&9XA%�PA%G�A$�A$�uA${A#
=A"�DA"M�A!��A!p�A!�A �jA v�A Q�A (�A��A�9AA�A��A+A��A�A�TA�7A?}AAM�A|�A��A1'A�A��Ar�A^5A  A��A|�AC�A�DA��A�mA�A+AĜA�9A�\A5?A�;AXA�A�wAt�A�AA�AdZA/AA�9AI�AA�A1'A�A�AoAVA
�9A
I�A	�;A	�A�jAA�hAO�A�`A�+A �AƨA�A/A~�A(�A�^AA�AbNA�^A;dA ��A ��A ��A r�A 1'@�l�@�n�@�%@�Z@��@���@�C�@��@�E�@��T@��@��`@�A�@�1@���@�@���@�v�@��^@�V@��j@�j@�w@�o@�@�
=@��y@�@�5?@�$�@��@��@���@�\)@�n�@�J@�?}@��@�C�@�n�@�@���@�@��`@�@� �@�ƨ@�
=@�\@�v�@�5?@�7@��/@�  @�
=@�+@ᙚ@��@�A�@�ƨ@߮@�K�@�E�@���@��T@���@݁@��`@�bN@�
=@�{@ٲ-@�X@��@ؓu@�33@�v�@��@��@���@�ƨ@ӝ�@Ӆ@�l�@�dZ@�S�@�+@�+@��@��@Ұ!@�^5@�M�@�$�@��#@ёh@��`@��;@Ο�@�p�@��@���@�bN@�(�@��;@�t�@ʇ+@�&�@�(�@ǥ�@�l�@Ƈ+@��@��@�G�@���@�j@��m@öF@ÍP@��@���@���@�`B@�`B@�X@�7L@��@��D@�b@��w@��@��y@�=q@���@��-@��7@�p�@��;@�@���@�x�@���@��u@�I�@�S�@�E�@��`@���@��
@���@�|�@�t�@�S�@�C�@���@���@�~�@�M�@�=q@��@�/@�Z@���@��@�dZ@�"�@���@���@�^5@�=q@��@���@�p�@��j@��
@�l�@�C�@���@���@��@�p�@��D@�bN@�I�@�1@��m@�ƨ@�ƨ@��@��P@�t�@�l�@�K�@�K�@�S�@�33@��@���@���@�r�@��@�|�@�K�@�+@��@���@�V@�J@���@��/@�b@��@�l�@�C�@�@�v�@���@��@�V@���@�r�@�Q�@�1@��;@��w@��w@��F@��@���@���@�l�@���@��T@��`@�1@K�@vE�@pĜ@f��@]`B@R^5@L1@B=q@>ff@3ƨ@.�@'�@ b@I�@��@t�@�;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�FA�9A�FA�RA���A�wA�A�A⟾A♚A�uA�+A�A�v�A�K�A�$�A��A�A���A��A�-A�A�`BA�S�A�C�A�9XA�-A�$�A��A�
=A���A��A��A��TA���A�!Aߴ9Aޟ�A�JAڃA���A�VAՁA�VA�-Aԥ�A�A��A�A�A���A�9XA���Aʡ�Aɧ�A�ƨA�E�A��HA�dZA�K�A�33A�33A�/A�{A�Q�A�A�v�A�bA�VA�x�A�A�A�VA��+A��9A�t�A���A��7A�E�A�XA�hsA�"�A�-A���A�&�A�~�A���A�`BA��9A�~�A��uA�=qA��PA��
A��mA�O�A���A���A�  A���A�A���A�^5A� �A�p�A��9A��A���A��A�=qA�ĜA�9XA�?}A�^5A��A�JA�1'A���A�/A�1A��jA���A�/A~1'Av��AnȴAfQ�A_?}A]�FAY�PAQ�FAMC�AJ�`AIVAF9XABE�A@~�A?ƨA@ȴA@�!A>^5A=�A<z�A;��A;�A:��A9
=A5��A3+A0�A.�\A.$�A.A�A.ZA.��A/�7A.z�A-��A-�A-dZA,I�A*�\A)33A'�7A&��A&�A&9XA%�PA%G�A$�A$�uA${A#
=A"�DA"M�A!��A!p�A!�A �jA v�A Q�A (�A��A�9AA�A��A+A��A�A�TA�7A?}AAM�A|�A��A1'A�A��Ar�A^5A  A��A|�AC�A�DA��A�mA�A+AĜA�9A�\A5?A�;AXA�A�wAt�A�AA�AdZA/AA�9AI�AA�A1'A�A�AoAVA
�9A
I�A	�;A	�A�jAA�hAO�A�`A�+A �AƨA�A/A~�A(�A�^AA�AbNA�^A;dA ��A ��A ��A r�A 1'@�l�@�n�@�%@�Z@��@���@�C�@��@�E�@��T@��@��`@�A�@�1@���@�@���@�v�@��^@�V@��j@�j@�w@�o@�@�
=@��y@�@�5?@�$�@��@��@���@�\)@�n�@�J@�?}@��@�C�@�n�@�@���@�@��`@�@� �@�ƨ@�
=@�\@�v�@�5?@�7@��/@�  @�
=@�+@ᙚ@��@�A�@�ƨ@߮@�K�@�E�@���@��T@���@݁@��`@�bN@�
=@�{@ٲ-@�X@��@ؓu@�33@�v�@��@��@���@�ƨ@ӝ�@Ӆ@�l�@�dZ@�S�@�+@�+@��@��@Ұ!@�^5@�M�@�$�@��#@ёh@��`@��;@Ο�@�p�@��@���@�bN@�(�@��;@�t�@ʇ+@�&�@�(�@ǥ�@�l�@Ƈ+@��@��@�G�@���@�j@��m@öF@ÍP@��@���@���@�`B@�`B@�X@�7L@��@��D@�b@��w@��@��y@�=q@���@��-@��7@�p�@��;@�@���@�x�@���@��u@�I�@�S�@�E�@��`@���@��
@���@�|�@�t�@�S�@�C�@���@���@�~�@�M�@�=q@��@�/@�Z@���@��@�dZ@�"�@���@���@�^5@�=q@��@���@�p�@��j@��
@�l�@�C�@���@���@��@�p�@��D@�bN@�I�@�1@��m@�ƨ@�ƨ@��@��P@�t�@�l�@�K�@�K�@�S�@�33@��@���@���@�r�@��@�|�@�K�@�+@��@���@�V@�J@���@��/@�b@��@�l�@�C�@�@�v�@���@��@�V@���@�r�@�Q�@�1@��;@��w@��w@��F@��@���@���@�l�@���@��T@��`@�1@K�@vE�@pĜ@f��@]`B@R^5@L1@B=q@>ff@3ƨ@.�@'�@ b@I�@��@t�@�;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
I�B
I�B
J�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
I�B
H�B
H�B
H�B
H�B
I�B
J�B
K�B
K�B
K�B
K�B
J�B
I�B
H�B
H�B
H�B
H�B
H�B
G�B
G�B
E�B
E�B
E�B
D�B
B�B
>wB
)�B
{B	�B	��B	ÖB	ÖB	ƨB	ǮB	ǮB	��B	��B	�B
�B
C�B
u�B
��B
�qB
�5B
��B�B>wB[#Br�Bt�Bt�Bt�Bu�B�B��B��B�dBƨB��B��B�`B�B��B1B!�B.B.B1'B49B49B33B33B33B2-B0!B-B(�B"�B�BVB1BB��B��B�B�yB�5B��B�'B��B�VB~�Bt�B`BBI�B49B�B�BPBB
�B
�jB
��B
�B
q�B
R�B
1'B
�B
�B
�B
VB	�yB	��B	t�B	M�B	@�B	<jB	&�B	B��B��B��B��B��B��B		7B	'�B	@�B	ZB	cTB	ffB	hsB	n�B	o�B	x�B	�hB	��B	��B	�^B	��B	�B	�`B	��B
�B
=qB
D�B
E�B
D�B
>wB
5?B
-B
0!B
33B
49B
8RB
9XB
<jB
<jB
<jB
:^B
:^B
:^B
9XB
9XB
:^B
:^B
:^B
:^B
:^B
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
=qB
<jB
=qB
=qB
<jB
<jB
;dB
;dB
:^B
9XB
9XB
:^B
9XB
9XB
9XB
9XB
9XB
8RB
7LB
5?B
7LB
7LB
6FB
6FB
5?B
33B
33B
2-B
33B
33B
49B
5?B
5?B
33B
2-B
33B
1'B
/B
-B
+B
(�B
'�B
&�B
&�B
%�B
%�B
%�B
%�B
%�B
$�B
$�B
#�B
"�B
%�B
$�B
$�B
"�B
!�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
oB
hB
bB
VB
PB
PB
VB
\B
hB
hB
hB
hB
bB
\B
\B
VB
PB
JB
DB
DB
JB
JB
PB
PB
JB
JB
PB
JB
JB
JB
JB
JB
DB

=B
	7B
1B
+B
%B
B
B
B
B
B
B
B
B
  B
  B	��B	��B	��B	��B	��B	��B
  B
B
1B
1B
1B
1B
1B
+B
+B
+B
+B
+B
+B
+B
+B
%B
%B
%B
%B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
1B
	7B
DB

=B

=B

=B
	7B
1B
+B
1B
	7B

=B
	7B
	7B

=B

=B

=B
DB
DB
DB
PB
PB
PB
VB
\B
hB
hB
oB
oB
oB
oB
oB
oB
uB
oB
oB
oB
oB
oB
oB
oB
hB
hB
hB
hB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
.B
0!B
7LB
<jB
?}B
C�B
G�B
N�B
P�B
W
B
YB
`BB
cTB
jB
s�B
w�B
}�B
~�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
I�B
I�B
J�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
I�B
H�B
H�B
H�B
H�B
I�B
J�B
K�B
K�B
K�B
K�B
J�B
I�B
H�B
H�B
H�B
H�B
H�B
GB
G�B
EtB
EqB
EsB
DpB
BdB
>HB
)�B
LB	�[B	��B	�hB	�hB	�wB	ǀB	ǀB	̝B	��B	�mB
�B
CfB
u�B
��B
�@B
�B
��BiB>DBZ�Br~Bt�Bt�Bt�Bu�B��B�gB��B�0B�uB��B��B�,B�YB��B�B!�B-�B-�B0�B4B4B2�B2�B2�B1�B/�B,�B(�B"�BgBB�B �B��B�B�kB�DB��B΢B��B�yB�B~�Bt�B`	BI�B4B�BGBB�B
�bB
�1B
�gB
��B
qkB
R�B
0�B
�B
rB
IB
B	�;B	��B	tB	M�B	@FB	<)B	&�B	�B��B��B��B�zB��B��B	�B	'�B	@CB	Y�B	cB	f!B	h0B	nRB	o\B	x�B	�$B	�gB	��B	�B	̉B	��B	�B	��B
sB
=,B
DVB
E]B
DXB
>2B
4�B
,�B
/�B
2�B
3�B
8B
9B
<'B
<'B
<%B
:B
:B
:B
9B
9B
:B
:B
:B
:B
:B
9B
9B
:B
:B
;B
;B
;B
<#B
=,B
=)B
=+B
='B
>/B
>0B
>1B
=(B
< B
=,B
=+B
<"B
<$B
;B
;B
:B
9B
9B
:B
9B
9B
9B
9B
9B
8	B
7B
4�B
7B
7B
5�B
5�B
4�B
2�B
2�B
1�B
2�B
2�B
3�B
4�B
4�B
2�B
1�B
2�B
0�B
.�B
,�B
*�B
(�B
'�B
&�B
&�B
%�B
%�B
%�B
%�B
%�B
$�B
$�B
#�B
"�B
%�B
$�B
$�B
"�B
!�B
!�B
 {B
 yB
vB
tB
oB
hB
kB
eB
cB
aB
[B
\B
XB
WB
PB
OB
JB
LB
BB
=B
?B
6B
+B
(B
B
B
B
B
	B
B
B
B
 B
B
B
B
B
B
B
B
 B

�B

�B
 B
 B
B
B
�B
�B
B
 B
�B
B
 B
�B

�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	�}B	�jB	�bB	�^B	�]B	�dB	�]B	�XB	�OB	�NB	�JB	�LB	�GB	�GB	�CB	�SB	�WB	�WB	�VB	�WB	�XB	�^B	�_B	�bB	�eB	�eB	�dB	�dB	�cB	�dB	�bB	�_B	�bB	�nB	�pB	�nB	�wB	�uB	�qB	�pB	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B

�B
	�B
	�B
	�B
�B
�B
�B
�B
�B
	�B
�B
�B
	�B
	�B
	�B

�B

�B

�B
B
B
B
B
B
B
B
!B
#B
"B
#B
&B
!B
)B
#B
"B
 B
"B
!B
#B
"B
B
B
B
B
!B
"B
#B
*B
-B
1B
:B
>B
>B
@B
>B
@B
?G�O�B
FB
lB
-�B
/�B
6�B
<B
?/B
CGB
G^B
N�B
P�B
V�B
X�B
_�B
cB
j/B
sfB
w}B
}�B
~�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.6 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008082019040510080820190405100808  AO  ARCAADJP                                                                    20171006070105    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171006070105  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171006070105  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100808  IP                  G�O�G�O�G�O�                