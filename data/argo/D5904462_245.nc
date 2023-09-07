CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-02-25T18:02:46Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20180225180246  20190405100813  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�O�σ�1   @�O����@-MO�;dZ�d�^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy� D�3D�C3D�y�D��fD�fD�VfD�l�D���D� D�<�D��fD�� D��fD�I�Dډ�D��3D�	�D�I�D�y�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�33A	��A)��AI��Ai��A���A���A���A���A���A���A���A���BffB
ffBffBffB"ffB*ffB2ffB:ffBBffBJffBRffBZffBbffBjffBrffBzffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�  B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C �4C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<�4C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�D &fD �fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD	&fD	�fD
&fD
�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD��D&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD &fD �fD!&fD!�fD"&fD"�fD#&fD#�fD$&fD$�fD%&fD%�fD&&fD&�fD'&fD'�fD(&fD(�fD)  D)�fD*&fD*�fD+&fD+�fD,&fD,�fD-&fD-�fD.&fD.�fD/&fD/�fD0&fD0�fD1&fD1�fD2&fD2�fD3&fD3�fD4&fD4�fD5&fD5�fD6&fD6�fD7&fD7�fD8&fD8�fD9&fD9�fD:&fD:�fD;&fD;�fD<&fD<�fD=&fD=�fD>&fD>�fD?&fD?�fD@&fD@�fDA&fDA�fDB&fDB�fDC&fDC�fDD&fDD�fDE&fDE�fDF&fDF�fDG&fDG�fDH&fDH�fDI&fDI�fDJ&fDJ�fDK&fDK�fDL&fDL�fDM&fDM�fDN&fDN�fDO&fDO�fDP&fDP�fDQ&fDQ�fDR&fDR�fDS&fDS�fDT&fDT�fDU&fDU�fDV&fDV�fDW&fDW�fDX&fDX�fDY&fDY�fDZ&fDZ�fD[&fD[�fD\&fD\�fD]&fD]�fD^&fD^�fD_&fD_�fD`&fD`�fDa&fDa�fDb&fDb�fDc&fDc�fDd&fDd�fDe&fDe�fDf&fDf�fDg&fDg�fDh&fDh�fDi&fDi�fDj&fDj�fDk&fDk�fDl&fDl�fDm&fDm�fDn&fDn�fDo&fDo�fDp&fDp�fDq&fDq�fDr&fDr�fDs&fDs�fDt&fDt��Dy�fD�fD�VfD���D�əD�)�D�i�D�� D�� D�#3D�P D���D��3D�	�D�\�Dڜ�D��fD��D�\�D��D�ٙ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bNA�\)A�C�A�A���A�"�AϺ^A�%A��`AΥ�A�E�A�&�A� �A��A�oA��A��
A���A�ȴA�ĜA�AͶFAͬA͙�A͍PA͋DA͋DA͋DA�|�A�hsA�ffA�dZA�dZA�^5A�ZA�ZA�\)A�C�A��mA��A̾wA��A�`BA�{Aʛ�A�ĜAǕ�AƁA�dZA��A�ƨA¶FA�oA�oA���A�{A�"�A��A�x�A��A��A���A��-A�E�A��9A��/A�^5A��A�^5A�&�A�dZA��!A��
A���A�p�A��
A���A�M�A���A�(�A�|�A���A��7A�
=A��7A�ƨA�l�A�E�A��wA��FA�l�A���A���A�I�A��TA�VA�VA��FA��7A�bAxE�Am��Ah��AfbAd1Aa�;A^^5A\��A\ �A[�AW�AP1'AN5?AKp�AJbNAI��AHbNAG7LAE�hAC33A@Q�A=��A<$�A;"�A9l�A6��A4jA1l�A/�wA-�mA-�TA-��A)�A$$�A!�PA ȴA ^5A�TA�PAG�A�uA��AO�A"�A��AdZAl�AhsA+A�RA��A�DAS�An�Av�A��A/AG�A��AVAĜA�A�hA �A�#AC�Ap�AO�A	dZA
�`A��A��Ax�AdZA
��A
bNA	��A	A	�FA	�A�HA�RAr�A�;A+A�HA(�A�A��A��A�PAp�A��A�+A�AK�A ��A ��A ĜA �A �uA ff@�\)@�n�@���@�G�@��9@�j@��@�33@��R@�/@�33@�M�@�X@���@�A�@�@�33@�ȴ@�~�@�n�@�E�@��@�@�@�@���@��@��@�h@���@�(�@�ƨ@�@��@�P@�dZ@�\)@�\)@�C�@�33@�o@��H@�\@�^5@��#@��@�\)@��@��@�7@�/@��@���@��@�M�@��T@�@ᙚ@���@�bN@�A�@��@�33@���@�G�@�/@��@�r�@ە�@ڸR@�=q@�?}@�Z@���@ו�@��@�v�@�=q@��@ՙ�@�?}@��@��/@ӥ�@���@�^5@��@�{@�J@��@��T@��#@с@�X@�&�@�Z@��;@�l�@�
=@���@Η�@�-@���@�?}@̛�@��@˅@�^5@�-@ɲ-@ȋD@ǝ�@Ɨ�@��@�X@�%@ě�@þw@å�@Õ�@��@�@�{@��^@��@�%@���@���@�A�@�l�@��@���@���@�x�@��@��@��@�Q�@��@�l�@�"�@���@���@��@�V@�I�@��@���@�ff@�V@��@�@�hs@�%@�bN@�A�@� �@���@��@�+@�o@�ȴ@�n�@�E�@��h@��@�z�@��@��w@�|�@�@�V@���@���@��@��9@�r�@��m@���@�S�@��y@��+@�@�?}@�V@�%@��@��`@�Ĝ@���@��@�|�@�S�@�"�@���@���@��\@�V@��@�hs@��`@�I�@�  @���@�t�@�S�@��@��H@��R@��+@�ff@�=q@�@��^@�O�@�Ĝ@�r�@�  @�|�@�l�@�S�@�K�@��@���@�v�@�=q@��#@�x�@�7L@���@�9X@�  @��
@���@��@�\)@�@��y@��\@��@�@��^@�X@�7L@�&�@�&�@��@��@��`@�z�@�I�@��@���@���@��@�l�@�ȴ@�E�@�x�@���@���@��u@� �@���@�+@��@���@���@�ff@��@��@���@�hs@�`B@�G�@��@��`@���@�(�@��m@�ƨ@�t�@�"�@���@���@�v�@���@���@�V@~ff@t�@i&�@_l�@V��@Lz�@D�D@;dZ@5�h@.��@+t�@%?}@ ��@ƨ@�@�@�@�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�bNA�\)A�C�A�A���A�"�AϺ^A�%A��`AΥ�A�E�A�&�A� �A��A�oA��A��
A���A�ȴA�ĜA�AͶFAͬA͙�A͍PA͋DA͋DA͋DA�|�A�hsA�ffA�dZA�dZA�^5A�ZA�ZA�\)A�C�A��mA��A̾wA��A�`BA�{Aʛ�A�ĜAǕ�AƁA�dZA��A�ƨA¶FA�oA�oA���A�{A�"�A��A�x�A��A��A���A��-A�E�A��9A��/A�^5A��A�^5A�&�A�dZA��!A��
A���A�p�A��
A���A�M�A���A�(�A�|�A���A��7A�
=A��7A�ƨA�l�A�E�A��wA��FA�l�A���A���A�I�A��TA�VA�VA��FA��7A�bAxE�Am��Ah��AfbAd1Aa�;A^^5A\��A\ �A[�AW�AP1'AN5?AKp�AJbNAI��AHbNAG7LAE�hAC33A@Q�A=��A<$�A;"�A9l�A6��A4jA1l�A/�wA-�mA-�TA-��A)�A$$�A!�PA ȴA ^5A�TA�PAG�A�uA��AO�A"�A��AdZAl�AhsA+A�RA��A�DAS�An�Av�A��A/AG�A��AVAĜA�A�hA �A�#AC�Ap�AO�A	dZA
�`A��A��Ax�AdZA
��A
bNA	��A	A	�FA	�A�HA�RAr�A�;A+A�HA(�A�A��A��A�PAp�A��A�+A�AK�A ��A ��A ĜA �A �uA ff@�\)@�n�@���@�G�@��9@�j@��@�33@��R@�/@�33@�M�@�X@���@�A�@�@�33@�ȴ@�~�@�n�@�E�@��@�@�@�@���@��@��@�h@���@�(�@�ƨ@�@��@�P@�dZ@�\)@�\)@�C�@�33@�o@��H@�\@�^5@��#@��@�\)@��@��@�7@�/@��@���@��@�M�@��T@�@ᙚ@���@�bN@�A�@��@�33@���@�G�@�/@��@�r�@ە�@ڸR@�=q@�?}@�Z@���@ו�@��@�v�@�=q@��@ՙ�@�?}@��@��/@ӥ�@���@�^5@��@�{@�J@��@��T@��#@с@�X@�&�@�Z@��;@�l�@�
=@���@Η�@�-@���@�?}@̛�@��@˅@�^5@�-@ɲ-@ȋD@ǝ�@Ɨ�@��@�X@�%@ě�@þw@å�@Õ�@��@�@�{@��^@��@�%@���@���@�A�@�l�@��@���@���@�x�@��@��@��@�Q�@��@�l�@�"�@���@���@��@�V@�I�@��@���@�ff@�V@��@�@�hs@�%@�bN@�A�@� �@���@��@�+@�o@�ȴ@�n�@�E�@��h@��@�z�@��@��w@�|�@�@�V@���@���@��@��9@�r�@��m@���@�S�@��y@��+@�@�?}@�V@�%@��@��`@�Ĝ@���@��@�|�@�S�@�"�@���@���@��\@�V@��@�hs@��`@�I�@�  @���@�t�@�S�@��@��H@��R@��+@�ff@�=q@�@��^@�O�@�Ĝ@�r�@�  @�|�@�l�@�S�@�K�@��@���@�v�@�=q@��#@�x�@�7L@���@�9X@�  @��
@���@��@�\)@�@��y@��\@��@�@��^@�X@�7L@�&�@�&�@��@��@��`@�z�@�I�@��@���@���@��@�l�@�ȴ@�E�@�x�@���@���@��u@� �@���@�+@��@���@���@�ff@��@��@���@�hs@�`B@�G�@��@��`@���@�(�@��m@�ƨ@�t�@�"�@���@���@�v�@���@���@�V@~ff@t�@i&�@_l�@V��@Lz�@D�D@;dZ@5�h@.��@+t�@%?}@ ��@ƨ@�@�@�@�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
%�B
%�B
&�B
%�B
$�B
&�B
&�B
'�B
+B
7LB
:^B
<jB
=qB
=qB
>wB
B�B
D�B
E�B
E�B
E�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
H�B
G�B
I�B
J�B
J�B
I�B
I�B
K�B
YB
e`B
�B
��B
��B
��B
��B
��B
��B
��B
��BB
��B
=B�B@�B�B|�B�B�7B��B�LB�XB�XB�-B��B��B��B��BhsB9XB8RBE�BbNBE�B{BB
��B
��B
�B
�sB
�BB
�
B
��B
�qB
�!B
��B
�oB
|�B
m�B
iyB
ffB
`BB
XB
M�B
?}B
6FB
0!B
'�B
DB	�yB	�BB	�#B	��B	��B	x�B	ffB	]/B	VB	N�B	J�B	K�B	L�B	K�B	I�B	.B	 �B	�B	�B	�B	�B	�B	PB	+B	B��B��B��B�B�B�B�fB�NB�NB�HB�5B�)B�B�5B�BB�BB�BB�BB�BB�;B�BB�HB�)B�/B�5B�HB�`B�`B�BB�)B�/B��B	1B		7B	+B	B	PB	'�B	YB	`BB	hsB	m�B	l�B	jB	W
B	K�B	Q�B	VB	p�B	��B	��B	��B	��B	�B	�B	�-B	�?B	�FB	�LB	�XB	�dB	�jB	�}B	��B	�}B	��B	ÖB	ÖB	ƨB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�
B	�
B	�
B	�
B	�
B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�/B	�/B	�5B	�BB	�NB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�sB	�sB	�yB	�B	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
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
+B
+B
+B
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

=B

=B

=B
DB
DB
DB

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
VB
VB
VB
VB
VB
\B
\B
bB
bB
bB
bB
hB
oB
oB
oB
uB
{B
{B
{B
{B
uB
uB
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
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
#�B
#�B
#�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
&�B
'�B
.B
1'B
6FB
<jB
C�B
H�B
N�B
T�B
YB
_;B
dZB
hsB
k�B
o�B
r�B
u�B
|�B
�B
�B
�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
%�B
%�B
&�B
%�B
$�B
&�B
&�B
'�B
*�B
7B
:+B
<;B
=>B
=?B
>EB
B]B
DlB
ErB
ErB
EsB
FyB
G�B
H�B
H�B
I�B
I�B
I�B
J�B
H�B
G~B
I�B
J�B
J�B
I�B
I�B
K�B
X�B
e0B
��B
�PB
�nB
�iB
�YB
�sB
��B
�UB
��B�B
��B
BXB@RB��B|�B��B�B��B�B�&B�$B��B��B��B�zB�KBh?B9#B8BEmBbBElBEB�B
��B
��B
�nB
�:B
�
B
��B
ϨB
�9B
��B
��B
�5B
|�B
mWB
i<B
f+B
`B
W�B
M�B
?BB
6
B
/�B
'�B
B	�<B	�B	��B	͕B	��B	x�B	f'B	\�B	U�B	N�B	J�B	K�B	L�B	K�B	IxB	-�B	 �B	^B	KB	EB	EB	>B	B	�B	�B��B��B��B�gB�FB�VB�"B�	B�B�B��B��B��B��B��B��B��B��B��B��B��B� B��B��B��B� B�B�B��B��B��B�yB	�B	�B	�B	�B	B	'�B	X�B	_�B	h,B	mHB	lBB	j7B	V�B	K}B	Q�B	U�B	p]B	�]B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�5B	�9B	�3B	�:B	�LB	�OB	�^B	�lB	�dB	�xB	̃B	̃B	ΏB	ϗB	НB	НB	МB	ЛB	КB	ѣB	ѢB	ѢB	ѡB	ҩB	ѣB	ѡB	ѠB	ѠB	ҧB	ԳB	ԴB	ռB	ջB	պB	��B	��B	ֿB	��B	��B	־B	��B	ֿB	ֿB	ֿB	��B	־B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�(B	�+B	�2B	�-B	�-B	�-B	�.B	�0B	�4B	�3B	�2B	�:B	�:B	�@B	�AB	�DB	�EB	�FB	�EB	�LB	�DB	�DB	�LB	�SB	�QB	�UB	�SB	�RB	�XB	�YB	�YB	�XB	�\B	�VB	�WB	�]B	�_B	�]B	�^B	�_B	�cB	�cB	�^B	�]B	�`B	�^B	�cB	�dB	�fB	�iB	�qB	�vB	�nB	�vB	�uB	�tB	�{B	�{B	�}B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
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
B
B
�B
 B
	B
B
B
	B
B
B
B
B
B
B
B
B
"B
"B
B
'B
.B
,B
+B
+B
(B
'B
-B
.B
,B
/B
.B
.B
,B
3B
3B
3B
8B
7B
7B
:B
AB
?B
?B
AB
?B
@B
?B
BB
?B
CB
CB
FB
FB
CB
FB
LB
QB
RB
XB
iB
mB
pB
 vB
 vB
 zB
!{B
"�B
"�B
"�B
#�B
#�B
#�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
&�B
'�B
-�B
0�B
5�B
<B
CHB
HgB
N�B
T�B
X�B
^�B
dB
h%B
k4B
oQB
r`B
uwB
|�B
��B
��B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.6 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008132019040510081320190405100813  AO  ARCAADJP                                                                    20180225180246    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180225180246  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180225180246  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100813  IP                  G�O�G�O�G�O�                