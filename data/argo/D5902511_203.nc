CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  x   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-03-07T00:21:07Z creation; 2023-02-10T23:09:44Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  X�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  _�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  {x   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �h   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ܘ   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �H   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � !�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � =�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � D�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` `h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   `�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   f�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   l�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T r�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   s   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   s$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   s,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   s4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � s<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   s�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   s�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    s�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        t    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    tArgo profile    3.1 1.2 19500101000000  20220307002107  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_203                 6810_008521_203                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @پ�R~R@پ�R~R11  @پ��{J#@پ��{J#@0X<K	�@0X<K	��d��:э&�d��:э&11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�@@  @�  @�G�@�  @޸R@�p�A\)A   A,(�A@  A`  A\)A��A�  A���A�Q�AϮA�  A�  B   B  B  B(�B   B'�
B0(�B8Q�B@  BG�BO�BX  B_�
Bg�
Bp(�Bx(�B�
B�  B�{B��
B��B�  B��B�  B�{B�{B�(�B�  B��
B�  B�  B�  B�  B�{B�  B��B�  B�{B�{B�  B�  B�{B��B�  B�(�B�  B�{B�  B��C  C��C�C�C	��C  C��C
=C
=C
=C
=C  C  C
=C  C   C"
=C$  C&
=C(
=C*  C,
=C-��C/��C2  C3��C6
=C8
=C:  C<  C>  C@  CA�CD  CF
=CH
=CI��CL  CM��CO��CR  CT
=CV  CX
=CZ  C\  C^  C_��Cb  Cd  Ce��Cg��Ci��Cl  Cn
=Co��Cq��Ct  Cu��Cw��Cz
=C|
=C~{C��C���C�  C�  C�  C�
=C�C�C�C�C�
=C�C�C�
=C�  C�C�C�  C�  C�C�  C�  C�C�C�  C�  C�C�C�  C�C���C�  C�  C�  C�C�  C���C���C�C�  C�  C���C�  C�C�  C�  C�  C���C�  C�C���C�  C�C�  C�  C�C�C�C���C�  C���C�C�C�C�  C���C�  C�  C�  C�  C�  C�  C�C�  C���C���C���C���C���C�C�
=C�  C���C�C�C���C���C���C���C���C���C�  C�C�C�  C�  C�  C���C���C�  C���C�C�C���C�  C�C�C�
=C�  C�  C�C�
=C�C���C���C���C�C�  C���C�  C�C�  C�  C���C�  C�  C���C���C���D z�D �qD� D�D��D�D��D  D��D�D��DD� D�qD� D  D}qD�qD	� D	�qD
}qD  D��D  D� D  D� D�qD}qD��D� D�D��D  D� D�D}qD�D��D�D� D  D� D�D��D�qDz�D�qD� D�D� D�qD}qD  D}qD  D� D�qDz�D�qD}qD  D��D   D ��D!�D!� D"  D"��D"�qD#}qD$  D$� D%�D%��D&  D&�D'D'��D'�qD(z�D(�qD)}qD)�qD*��D+D+�D,  D,� D,�qD-}qD.  D.��D/�D/��D0�D0��D1�D1� D2  D2}qD3  D3��D4�D4��D5  D5��D6�D6��D7�D7� D7��D8� D9�D9}qD9��D:z�D:�qD;�D<D<�D=�D=� D>  D>� D?  D?� D@  D@}qD@�qDA� DA�qDB}qDB�qDC}qDD  DD}qDD��DE}qDE��DF}qDF�qDG� DG�qDH}qDI�DI�DJ  DJz�DJ�qDK�DL  DLz�DL�qDM��DN�DN� DO  DO��DP�DP��DP�qDQ}qDQ�qDR� DR�qDSxRDS�qDT��DU  DU��DV�DV��DW�DW� DW�qDX}qDY�DY��DZ  DZ��D[�D[��D\�D\� D]�D]��D^  D^}qD^�qD_}qD`  D`��DaDa� Db  Db��Dc�Dc}qDc��Ddz�De  Dez�De��Df}qDg  Dg}qDg�qDh}qDh�qDi� Dj�Dj� Dk�Dk� Dk�qDl��DmDm��Dn  Dn��Do�Do��Dp  Dp� Dq�Dq� Dq�qDr� Ds�Ds� Dt�Dt��Dt�qDu}qDv�Dv}qDv��Dw}qDw�qDx� Dy�Dy�Dz  Dz� D{  D{��D|D|��D|�qD}� D~  D~� D  D}qD�  D�AHD�~�D���D�  D�@ D��HD�� D���D�=qD�}qD��qD���D�>�D�� D�� D���D�AHD�� D�� D�HD�@ D�}qD���D���D�@ D���D���D���D�>�D�� D��HD�  D�@ D��HD�� D�  D�AHD��HD��HD�HD�AHD�� D�� D�  D�=qD�~�D���D���D�@ D��HD�� D��)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�Q�?�?aG�?���?�Q�?��@��@��@0��@G�@W
=@p��@�G�@���@�@��R@��@�z�@�p�@�ff@�33@��H@��@��@�(�A�AQ�Ap�A��A
=A��A ��A%A,(�A0��A4z�A;�A@��AC�
AI��AO\)AS33AXQ�A^{AaG�Ag
=Al��Ap��Au�Az�HA�  A��A���A��A���A�(�A�\)A���A��A��RA�G�A�33A�{A���A�33A��A��A��HA�p�A�
=A��A���A�
=A���A�z�A��RA�Q�AÅA�ffA�Q�Aʏ\A��A�Q�Aҏ\A�z�A�
=A�=qA�(�A�ffA�G�A��
A�{A�Q�A�33A�p�A�\)A��HA���A��RA�=qA�(�A�ffB ��B�B�HBQ�BB�\B�
B	p�B
�\B�B��B=qB33B��B=qB
=B(�BB
=B  Bp�B�HB  B��B=qB�
B ��B!�B#�B$��B%B'
=B(��B)p�B*�HB,z�B-��B.�\B0(�B1��B2�RB3�
B5p�B6�HB7�
B9�B:�RB<  B<��B>ffB?�
B@��BB{BC�BD��BE�BG�BH��BIBK\)BLz�BMp�BO
=BPQ�BQp�BR�RBTQ�BUG�BVffBX(�BYG�BZ=qB[�
B]�B^{B_�B`��BaBc\)Bd��BeBf�HBhz�Bi�Bj�HBl(�BmBn�RBp(�Bq��Br�HBs�
Bu�Bv�RBw�
Bx��Bz�\B{�
B|��B~{B�B�ffB�
=B��
B�ffB���B�B�ffB��HB���B�ffB���B�p�B�(�B���B��B�  B��RB�p�B�{B��\B�\)B�{B���B�33B�  B��RB�G�B�B��\B�G�B�B�ffB�33B��
B�Q�B��HB��B�=qB��RB���B�=qB��RB�G�B�{B��RB�G�B��
B���B�\)B��B�z�B�33B��B�z�B���B�B��\B�33B���B�ffB��B���B�(�B���B���B�(�B��HB���B�(�B���B��B�=qB���B�\)B�(�B��RB�G�B�{B���B�G�B��B���B�\)B��B�z�B��B��B�z�B�
=B��
B�z�B���B��B�ffB���B�\)B�(�B���B��B�B�ffB���B�33B�B�Q�B���B�
=BÙ�B�  B�=qBĸRB��B�\)BŮB�(�B�Q�BƏ\B�
=B�\)BǅB��
B�=qBȣ�B���B�
=B�p�B��B�{B�Q�B���B�33B�\)BˮB�  B�z�B̸RB�
=B�G�B�B�(�B�Q�BΣ�B�33B�p�Bϙ�B�{BЏ\B���B���BхB��
B�  B�ffB��HB�
=B�\)B��
B�=qB�z�BԸRB�33Bՙ�B�B�{B�z�B��HB�
=B�p�B��B�=qB�ffB���B�G�B�p�B�B�=qBڣ�B���B��Bۙ�B��
B�{B܏\B���B�33B�p�B��
B�Q�Bޏ\B���B�G�B߮B��B�(�B��\B�
=B�G�B�B�  B�ffB��B���B�p�B��
B�  B�Q�B���B�33B�\)B�B�{B��B��HB��B�B�  B�ffB�RB���B�\)B��
B�=qB�z�B���B�33B�B�  B�(�B�\B���B�p�B�B�  B�Q�B���B��B�G�B�B�(�B�Q�B��B�
=B�B�B�  B�ffB���B�
=B�33B�B�  B�=qB�z�B���B�33B�\)B��
B�Q�B�z�B���B�
=B�p�B��B��
B�Q�B��\B��RB��B�p�B��B��
B�{B��\B��HB�
=B�G�B���B�  B�{B�ffB���B���B��B�p�B��B�(�B�Q�B��\B�
=B��B�\)B��
C 
=C �C G�C �C �\C �RC �C  C{C=qCp�C�C��C��C�HC
=C33C=qCQ�C�C��C�C��C  C{C�CG�Cp�Cz�C�\CC�C�C{C=qCG�CffC��C��C�RC�C  C
=C(�C\)Cz�Cz�C��C��C�HC�C{C=qC\)CffC�\C�RCC��C{C�CG�Cp�C�C��C�
C
=C{C=qCffC�C��C��C	  C	�C	(�C	\)C	�\C	��C	�RC	��C
�C
33C
Q�C
z�C
�C
��C
�HC
=C=qCffC�C��C�
C  C{CG�Cz�C��C�RC��C
=C=qCQ�Cp�C�C��C�
C
=C=qCffC�C��C�
C  C(�C=qCz�C��C�RC�
C{CG�CQ�C�C�RC�
C�C�CQ�Cp�C�C�C�HC
=C{CG�C�C��CC�HC{CG�C�C�C��C�C(�CffC�\C��C�
C{CG�CffC�C�RC��C�C33C\)C��C��C�C
=C33Cp�C��CC�HC{CQ�Cz�C��C�RC��C(�CQ�Cp�C��C�HC
=C33CffC��C�
C��C�CQ�C��CC�HC{CQ�C�\C�RC�
C
=CG�C�C�C��C
=CG�Cp�C��C��C
=C=qC\)C�\C�
C   C �C \)C ��C ��C �C!�C!\)C!��C!��C!�C"�C"\)C"��C"�
C#  C#�C#Q�C#��C#��C#�C$�C$Q�C$�\C$��C$��C%�C%G�C%�\C%��C&
=C&33C&Q�C&��C&�
C&�C'(�C'ffC'��C'�
C(  C((�C(p�C(�C(�HC)
=C)=qC)z�C)C*
=C*=qC*\)C*��C*�
C+{C+\)C+�\C+C+�C,33C,z�C,�RC,�HC-{C-Q�C-��C-�HC.
=C.=qC.p�C.�RC/  C/33C/\)C/�\C/��C0{C0Q�C0z�C0�C0�
C1{C1\)C1�\C1�RC1�HC2�C2ffC2�C2�
C3
=C3=qC3z�C3�RC3�C433C4p�C4C4��C5(�C5\)C5�\C5�
C6�C6ffC6�\C6C6��C733C7p�C7�C7��C833C8p�C8��C8��C9{C9\)C9�C9�RC9��C:=qC:�C:C;  C;33C;ffC;��C;�
C<�C<\)C<��C<�
C={C=\)C=�\C=C=��C>(�C>p�C>�C>�C?(�C?ffC?��C?��C@
=C@=qC@z�C@�C@�CA�CAQ�CA�\CA�RCA��CB33CBp�CB��CB�HCC�CC\)CC��CC�
CD{CDG�CD�CDCD��CE33CEp�CE�CE��CF(�CFffCF��CF�HCG�CG\)CG��CG�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                              111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @�@@  @�  @�G�@�  @޸R@�p�A\)A   A,(�A@  A`  A\)A��A�  A���A�Q�AϮA�  A�  B   B  B  B(�B   B'�
B0(�B8Q�B@  BG�BO�BX  B_�
Bg�
Bp(�Bx(�B�
B�  B�{B��
B��B�  B��B�  B�{B�{B�(�B�  B��
B�  B�  B�  B�  B�{B�  B��B�  B�{B�{B�  B�  B�{B��B�  B�(�B�  B�{B�  B��C  C��C�C�C	��C  C��C
=C
=C
=C
=C  C  C
=C  C   C"
=C$  C&
=C(
=C*  C,
=C-��C/��C2  C3��C6
=C8
=C:  C<  C>  C@  CA�CD  CF
=CH
=CI��CL  CM��CO��CR  CT
=CV  CX
=CZ  C\  C^  C_��Cb  Cd  Ce��Cg��Ci��Cl  Cn
=Co��Cq��Ct  Cu��Cw��Cz
=C|
=C~{C��C���C�  C�  C�  C�
=C�C�C�C�C�
=C�C�C�
=C�  C�C�C�  C�  C�C�  C�  C�C�C�  C�  C�C�C�  C�C���C�  C�  C�  C�C�  C���C���C�C�  C�  C���C�  C�C�  C�  C�  C���C�  C�C���C�  C�C�  C�  C�C�C�C���C�  C���C�C�C�C�  C���C�  C�  C�  C�  C�  C�  C�C�  C���C���C���C���C���C�C�
=C�  C���C�C�C���C���C���C���C���C���C�  C�C�C�  C�  C�  C���C���C�  C���C�C�C���C�  C�C�C�
=C�  C�  C�C�
=C�C���C���C���C�C�  C���C�  C�C�  C�  C���C�  C�  C���C���C���D z�D �qD� D�D��D�D��D  D��D�D��DD� D�qD� D  D}qD�qD	� D	�qD
}qD  D��D  D� D  D� D�qD}qD��D� D�D��D  D� D�D}qD�D��D�D� D  D� D�D��D�qDz�D�qD� D�D� D�qD}qD  D}qD  D� D�qDz�D�qD}qD  D��D   D ��D!�D!� D"  D"��D"�qD#}qD$  D$� D%�D%��D&  D&�D'D'��D'�qD(z�D(�qD)}qD)�qD*��D+D+�D,  D,� D,�qD-}qD.  D.��D/�D/��D0�D0��D1�D1� D2  D2}qD3  D3��D4�D4��D5  D5��D6�D6��D7�D7� D7��D8� D9�D9}qD9��D:z�D:�qD;�D<D<�D=�D=� D>  D>� D?  D?� D@  D@}qD@�qDA� DA�qDB}qDB�qDC}qDD  DD}qDD��DE}qDE��DF}qDF�qDG� DG�qDH}qDI�DI�DJ  DJz�DJ�qDK�DL  DLz�DL�qDM��DN�DN� DO  DO��DP�DP��DP�qDQ}qDQ�qDR� DR�qDSxRDS�qDT��DU  DU��DV�DV��DW�DW� DW�qDX}qDY�DY��DZ  DZ��D[�D[��D\�D\� D]�D]��D^  D^}qD^�qD_}qD`  D`��DaDa� Db  Db��Dc�Dc}qDc��Ddz�De  Dez�De��Df}qDg  Dg}qDg�qDh}qDh�qDi� Dj�Dj� Dk�Dk� Dk�qDl��DmDm��Dn  Dn��Do�Do��Dp  Dp� Dq�Dq� Dq�qDr� Ds�Ds� Dt�Dt��Dt�qDu}qDv�Dv}qDv��Dw}qDw�qDx� Dy�Dy�Dz  Dz� D{  D{��D|D|��D|�qD}� D~  D~� D  D}qD�  D�AHD�~�D���D�  D�@ D��HD�� D���D�=qD�}qD��qD���D�>�D�� D�� D���D�AHD�� D�� D�HD�@ D�}qD���D���D�@ D���D���D���D�>�D�� D��HD�  D�@ D��HD�� D�  D�AHD��HD��HD�HD�AHD�� D�� D�  D�=qD�~�D���D���D�@ D��HD�� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�Q�?�?aG�?���?�Q�?��@��@��@0��@G�@W
=@p��@�G�@���@�@��R@��@�z�@�p�@�ff@�33@��H@��@��@�(�A�AQ�Ap�A��A
=A��A ��A%A,(�A0��A4z�A;�A@��AC�
AI��AO\)AS33AXQ�A^{AaG�Ag
=Al��Ap��Au�Az�HA�  A��A���A��A���A�(�A�\)A���A��A��RA�G�A�33A�{A���A�33A��A��A��HA�p�A�
=A��A���A�
=A���A�z�A��RA�Q�AÅA�ffA�Q�Aʏ\A��A�Q�Aҏ\A�z�A�
=A�=qA�(�A�ffA�G�A��
A�{A�Q�A�33A�p�A�\)A��HA���A��RA�=qA�(�A�ffB ��B�B�HBQ�BB�\B�
B	p�B
�\B�B��B=qB33B��B=qB
=B(�BB
=B  Bp�B�HB  B��B=qB�
B ��B!�B#�B$��B%B'
=B(��B)p�B*�HB,z�B-��B.�\B0(�B1��B2�RB3�
B5p�B6�HB7�
B9�B:�RB<  B<��B>ffB?�
B@��BB{BC�BD��BE�BG�BH��BIBK\)BLz�BMp�BO
=BPQ�BQp�BR�RBTQ�BUG�BVffBX(�BYG�BZ=qB[�
B]�B^{B_�B`��BaBc\)Bd��BeBf�HBhz�Bi�Bj�HBl(�BmBn�RBp(�Bq��Br�HBs�
Bu�Bv�RBw�
Bx��Bz�\B{�
B|��B~{B�B�ffB�
=B��
B�ffB���B�B�ffB��HB���B�ffB���B�p�B�(�B���B��B�  B��RB�p�B�{B��\B�\)B�{B���B�33B�  B��RB�G�B�B��\B�G�B�B�ffB�33B��
B�Q�B��HB��B�=qB��RB���B�=qB��RB�G�B�{B��RB�G�B��
B���B�\)B��B�z�B�33B��B�z�B���B�B��\B�33B���B�ffB��B���B�(�B���B���B�(�B��HB���B�(�B���B��B�=qB���B�\)B�(�B��RB�G�B�{B���B�G�B��B���B�\)B��B�z�B��B��B�z�B�
=B��
B�z�B���B��B�ffB���B�\)B�(�B���B��B�B�ffB���B�33B�B�Q�B���B�
=BÙ�B�  B�=qBĸRB��B�\)BŮB�(�B�Q�BƏ\B�
=B�\)BǅB��
B�=qBȣ�B���B�
=B�p�B��B�{B�Q�B���B�33B�\)BˮB�  B�z�B̸RB�
=B�G�B�B�(�B�Q�BΣ�B�33B�p�Bϙ�B�{BЏ\B���B���BхB��
B�  B�ffB��HB�
=B�\)B��
B�=qB�z�BԸRB�33Bՙ�B�B�{B�z�B��HB�
=B�p�B��B�=qB�ffB���B�G�B�p�B�B�=qBڣ�B���B��Bۙ�B��
B�{B܏\B���B�33B�p�B��
B�Q�Bޏ\B���B�G�B߮B��B�(�B��\B�
=B�G�B�B�  B�ffB��B���B�p�B��
B�  B�Q�B���B�33B�\)B�B�{B��B��HB��B�B�  B�ffB�RB���B�\)B��
B�=qB�z�B���B�33B�B�  B�(�B�\B���B�p�B�B�  B�Q�B���B��B�G�B�B�(�B�Q�B��B�
=B�B�B�  B�ffB���B�
=B�33B�B�  B�=qB�z�B���B�33B�\)B��
B�Q�B�z�B���B�
=B�p�B��B��
B�Q�B��\B��RB��B�p�B��B��
B�{B��\B��HB�
=B�G�B���B�  B�{B�ffB���B���B��B�p�B��B�(�B�Q�B��\B�
=B��B�\)B��
C 
=C �C G�C �C �\C �RC �C  C{C=qCp�C�C��C��C�HC
=C33C=qCQ�C�C��C�C��C  C{C�CG�Cp�Cz�C�\CC�C�C{C=qCG�CffC��C��C�RC�C  C
=C(�C\)Cz�Cz�C��C��C�HC�C{C=qC\)CffC�\C�RCC��C{C�CG�Cp�C�C��C�
C
=C{C=qCffC�C��C��C	  C	�C	(�C	\)C	�\C	��C	�RC	��C
�C
33C
Q�C
z�C
�C
��C
�HC
=C=qCffC�C��C�
C  C{CG�Cz�C��C�RC��C
=C=qCQ�Cp�C�C��C�
C
=C=qCffC�C��C�
C  C(�C=qCz�C��C�RC�
C{CG�CQ�C�C�RC�
C�C�CQ�Cp�C�C�C�HC
=C{CG�C�C��CC�HC{CG�C�C�C��C�C(�CffC�\C��C�
C{CG�CffC�C�RC��C�C33C\)C��C��C�C
=C33Cp�C��CC�HC{CQ�Cz�C��C�RC��C(�CQ�Cp�C��C�HC
=C33CffC��C�
C��C�CQ�C��CC�HC{CQ�C�\C�RC�
C
=CG�C�C�C��C
=CG�Cp�C��C��C
=C=qC\)C�\C�
C   C �C \)C ��C ��C �C!�C!\)C!��C!��C!�C"�C"\)C"��C"�
C#  C#�C#Q�C#��C#��C#�C$�C$Q�C$�\C$��C$��C%�C%G�C%�\C%��C&
=C&33C&Q�C&��C&�
C&�C'(�C'ffC'��C'�
C(  C((�C(p�C(�C(�HC)
=C)=qC)z�C)C*
=C*=qC*\)C*��C*�
C+{C+\)C+�\C+C+�C,33C,z�C,�RC,�HC-{C-Q�C-��C-�HC.
=C.=qC.p�C.�RC/  C/33C/\)C/�\C/��C0{C0Q�C0z�C0�C0�
C1{C1\)C1�\C1�RC1�HC2�C2ffC2�C2�
C3
=C3=qC3z�C3�RC3�C433C4p�C4C4��C5(�C5\)C5�\C5�
C6�C6ffC6�\C6C6��C733C7p�C7�C7��C833C8p�C8��C8��C9{C9\)C9�C9�RC9��C:=qC:�C:C;  C;33C;ffC;��C;�
C<�C<\)C<��C<�
C={C=\)C=�\C=C=��C>(�C>p�C>�C>�C?(�C?ffC?��C?��C@
=C@=qC@z�C@�C@�CA�CAQ�CA�\CA�RCA��CB33CBp�CB��CB�HCC�CC\)CC��CC�
CD{CDG�CD�CDCD��CE33CEp�CE�CE��CF(�CFffCF��CF�HCG�CG\)CG��CG�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                              111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A͛�AͅÁA�z�A�XA�-A��A�oA�JA�A���A��A��A��TA��HA��;A��A���A���A���A���A���A̡�A�"�A�+A�bA�n�A�G�A�VA�^5A�\)A�I�A��A��HAʶFA�l�A�bNA�M�A�
=A��mA���A���Aɩ�A�hsA�ZA�?}A�-A�"�A���AȺ^Aȇ+A�x�A�G�A�v�A�M�A�r�A�JA���A�t�A�A�A���AĬAã�A�ĜA�ĜA�I�A�K�A�VA�JA�33A��yA���A��
A�`BA�VA��A�bNA�ZA��A�ȴA��A��A�dZA��`A�ĜA��HA�&�A�&�A��A���A���A���A�VA��#A��HA��A� �A��;A���A�dZA�-A�=qA��A���A�G�A��`A�&�A���A�ĜA�PA|�RAz�\Ay?}AwG�As��An�Ah �Ae�hAb~�A^bA[�AW;dAS��AQ�AO�AL �AF��AD�AC�
AB��AA�^A>�A=K�A;�wA:E�A9�A6ffA5p�A5/A4n�A3��A1��A0JA-��A+�A++A*jA(�HA(A�A'��A'A%��A%"�A$�jA#�
A#;dA"��A"��A"Q�A!��A �9A ^5A/AQ�A�-AffAG�AE�A�9A�
A
=A~�A{A��AVA�/A��AQ�A�A�A�wA �A5?A�AoA{A�TAVA$�A�hA/A��A�#AVA�A�A
�!A
��A
�+A	�AbNAQ�Ap�AffA�^Ap�A;dA��A5?A�;AXA�A��A�RAjAJA�A7LA ��A ^5A E�A $�A �A bA J@��F@�@���@�ƨ@�K�@�@���@��#@�r�@�?}@�Q�@�b@���@�ƨ@�F@�P@�@�P@�ff@�V@�j@�1'@���@�;d@�x�@��@�w@�l�@�"�@��@��@�ȴ@�5?@�V@�(�@畁@�C�@柾@��#@�7@�%@��@�F@�|�@��@�+@�=q@�O�@��;@�+@�M�@ݑh@�G�@�/@�%@��`@ܬ@�1@�ff@�p�@��@�bN@��
@�+@��@�ff@���@�X@���@Դ9@�ƨ@�o@�M�@��#@���@�r�@Ѓ@�Z@� �@�dZ@�"�@�
=@�n�@��#@͉7@�z�@�b@�ƨ@���@��@��m@ˮ@�M�@�n�@�^5@���@��@��m@ǅ@�33@ư!@��@��@��T@ũ�@�?}@��`@ě�@�1'@��;@Ý�@�|�@�t�@�K�@�33@�o@�v�@�-@��T@�7L@�Ĝ@��9@�Z@���@�V@�M�@�$�@�@��#@���@�x�@�hs@�?}@���@��@�bN@��
@�v�@�p�@��9@� �@���@�|�@�"�@��@�ȴ@�^5@��@�@�hs@�7L@���@��u@�1'@�ƨ@�l�@���@�-@��@���@��@�O�@�1'@�C�@��@��R@��+@�{@�@�&�@��@��`@���@��9@���@�9X@���@�dZ@�C�@�+@��@���@�ff@���@�%@�Q�@��@��@���@�C�@��@���@�-@��-@��@�%@�A�@��m@���@��@�dZ@�;d@��+@�v�@�=q@��@��T@���@���@�`B@�V@��9@���@��/@��j@��9@�z�@�Z@�1@��m@��;@��F@�t�@�33@��@��!@�M�@�-@��@��@�hs@�?}@�/@��@��/@���@�j@�A�@�b@��;@�t�@��H@�=q@��@�O�@��/@��j@��j@���@�bN@� �@���@�t�@�"�@�
=@�ȴ@��+@���@��\@�~�@�-@��7@�&�@��/@��@�I�@�  @��w@��@�S�@�;d@�+@�"�@���@��R@�v�@�5?@��@���@���@��h@�x�@�O�@��/@��@�A�@�b@��;@���@�l�@�33@�@���@�n�@��@���@��7@�`B@�X@�/@�V@��@�Ĝ@�z�@�1'@��w@���@���@��@�C�@�33@��y@���@���@���@��-@�hs@���@��j@�r�@��@��m@��;@��
@���@�ƨ@�C�@���@���@�5?@�{@��T@�O�@�%@��/@��u@�I�@��@��@��w@�C�@��H@��\@�M�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A͡�A͝�Aͣ�A͡�AͅA̓AͅA̓AͅÁA�z�A̓A�x�A�x�A�t�A�VA�I�A�C�A�1'A� �A� �A� �A�{A�oA�{A�oA�bA�JA�
=A�JA�1A�  A�A���A���A���A���A��A��A��A��A��A��A��A��yA��mA��yA��TA��TA��`A��`A��HA��HA��mA��TA��HA��TA��TA��;A��HA��HA��#A��;A��HA��;A��/A��/A��;A��/A��
A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A���A�ȴA���A���A���A���A���A���A���A���A���A�ȴA���A���A�ƨA�ȴA�ȴA���A̧�A̍PA�z�A�n�A�M�A�9XA�VA�
=A�bA�oA��A��A�$�A�&�A�1'A�9XA�;dA�9XA�-A��A���A��A��A���AˋDA�ffA�Q�A�?}A�=qA�E�A�E�A�C�A�I�A�K�A�K�A�K�A�Q�A�S�A�O�A�S�A�bNA�dZA�dZA�bNA�`BA�ZA�VA�\)A�dZA�XA�XA�\)A�XA�^5A�XA�VA�S�A�Q�A�=qA�+A��A���A��`A��yA��mA��`A��`A��`A��HA��`A��;A��/A��TA��TA��
A�ĜAʶFAʝ�AʁA�v�A�v�A�n�A�jA�jA�ffA�bNA�dZA�hsA�dZA�`BA�`BA�^5A�VA�S�A�S�A�I�A�C�A�;dA�&�A�{A�JA�A���A��A��A��A��mA��`A��TA��;A��#A��
A��A���A���A���A���A���A���A���A���A���A���A���A�ĜAɰ!Aɛ�AɋDA�z�A�r�A�jA�ffA�dZA�dZA�hsA�ffA�bNA�^5A�^5A�XA�O�A�G�A�C�A�C�A�A�A�?}A�9XA�7LA�33A�-A�+A�-A�(�A�$�A�&�A�&�A�$�A��A� �A��A��A�{A�{A���A��;A��/A��
A���A�ƨAȺ^AȬAț�Aȕ�AȍPAȃAȁAȅAȇ+Aȉ7Aȇ+A�~�A�x�A�r�A�l�A�n�A�ffA�\)A�VA�I�A�5?A��A���A���Aǉ7A�jA�VA�I�A�=qA�33A�5?A�33A�/A�33A�A�A�M�A�\)A�l�A�n�A�t�AǅAǍPAǑhAǙ�AǓuAǇ+A�~�A�|�A�p�A�VA�K�A�E�A�9XA�(�A� �A��A��A�bA�
=A�1A�
=A�A���A���A��A��A��A��mA��;A���A�ĜAƼjAƲ-AƩ�AƝ�Aƛ�Aƕ�AƍPAƋDAƉ7A�|�A�t�AƃA�~�A�z�A�x�A�ZA�ZA�K�A�K�A�M�A�O�A�I�A�G�A�I�A�C�A�;dA�7LA�?}A�33A�+A�(�A�&�A��A��A�oA�A��AŸRA�t�A�XA�G�A�9XA�$�A�1A��A��;A�ƨAġ�AčPAāA�v�A�\)A�E�A�;dA�"�A�oA�  AüjAÛ�AÏ\A�z�A�jA�^5A�VA�I�A�E�A�33A��A��A��`A���A¬ADA�jA�^5A�G�A�+A�JA��A��A�ĜA��RA���A���A���A���A��A�v�A�r�A�r�A�n�A�bNA�\)A�S�A�E�A�9XA�33A�(�A�VA���A��yA���A��A��7A�p�A�`BA�O�A�C�A�"�A��HA��A�E�A��A��RA���A��DA�v�A�bNA�G�A�9XA�-A��A��A�VA�%A�  A���A��TA�A��-A��\A�XA�+A���A��!A��uA�hsA�K�A�+A��A��A���A��^A���A�~�A�dZA�Q�A�&�A���A��`A�ƨA��A��+A�n�A�^5A�A�A�1'A�(�A� �A��A�JA�  A���A��A��/A��wA��A���A��hA�x�A�\)A�JA���A���A�l�A�9XA�%A��A���A��hA�z�A�I�A�"�A�  A��/A��A�^5A�O�A��A��yA��-A�ffA�ȴA��^A���A�Q�A�
=A��!A�A�A��A��^A��+A�O�A�{A�VA�  A��A��A��RA���A�z�A�XA�1'A��yA�S�A��A��A�v�A�n�A�hsA�dZA�^5A�VA�Q�A�M�A�9XA�/A�{A�%A��A��`A���A�ƨA��jA���A���A��uA��7A�|�A�ffA�Q�A�A�A�oA�  A��HA���A�p�A�bNA�`BA�E�A�VA��
A��+A�ffA�7LA��A�VA���A��A��A�ƨA���A�ffA�oA��
A���A�p�A�5?A���A��A��HA��#A��A���A���A���A�ĜA��jA��PA�n�A�+A��HA���A��^A�|�A�O�A�A���A��\A�n�A�=qA�1A��
A���A���A��+A�v�A�\)A�ZA�VA�E�A�I�A�9XA�7LA�5?A�(�A���A���A��A��HA��TA��#A�ȴA��RA�ZA�VA�ZA�C�A��/A���A�dZA�VA�7LA� �A���A��yA��jA��hA�;dA�{A���A��A��wA���A��hA�x�A�?}A�VA��A���A���A��\A�^5A�?}A��A�1A��yA��TA��
A���A��FA��hA��A�v�A�hsA�^5A�?}A�&�A��A�%A��!A��hA��7A�hsA��A��/A��7A�XA�1'A��A�1A��TA���A��jA���A��PA��7A�~�A�n�A�M�A��A���A�A��uA��A�$�A��wA��A�`BA�E�A�;dA��A��A���A�`BA�$�A��A�z�A���A��RA�O�A��A��A�A��A�\)A�A��FA��A�^5A�E�A��A��yA���A���A��FA��-A��A��A���A��uA�|�A�n�A�ffA�`BA�ZA�Q�A�K�A�G�A�E�A�A�A�7LA�&�A��A��A���A��A���A��\A��A�jA�VA�C�A�&�A���A���A���A�hsA�K�A�5?A�{A���A��A���A��A���A��7A�t�A�dZA�S�A�?}A��A�
=A���A���A���A���A���A���A���A���A��A���A��RA���A��DA�l�A�(�A���A��9A�S�A��A���A���A���A��hA�v�A�jA�ZA�K�A�9XA� �A�A���A�z�A�7LA��A��A���A��FA���A��7A�r�A�^5A�I�A�=qA�&�A�VA���A��mA��^A���A��A�dZA�VA���A�G�A���A�l�A��jA�;dA��yA��TA���A�ĜA��-A���A��PA��A�v�A�hsA�S�A�9XA�&�A��A�VA�1A���A���A��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                              111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A͛�AͅÁA�z�A�XA�-A��A�oA�JA�A���A��A��A��TA��HA��;A��A���A���A���A���A���A̡�A�"�A�+A�bA�n�A�G�A�VA�^5A�\)A�I�A��A��HAʶFA�l�A�bNA�M�A�
=A��mA���A���Aɩ�A�hsA�ZA�?}A�-A�"�A���AȺ^Aȇ+A�x�A�G�A�v�A�M�A�r�A�JA���A�t�A�A�A���AĬAã�A�ĜA�ĜA�I�A�K�A�VA�JA�33A��yA���A��
A�`BA�VA��A�bNA�ZA��A�ȴA��A��A�dZA��`A�ĜA��HA�&�A�&�A��A���A���A���A�VA��#A��HA��A� �A��;A���A�dZA�-A�=qA��A���A�G�A��`A�&�A���A�ĜA�PA|�RAz�\Ay?}AwG�As��An�Ah �Ae�hAb~�A^bA[�AW;dAS��AQ�AO�AL �AF��AD�AC�
AB��AA�^A>�A=K�A;�wA:E�A9�A6ffA5p�A5/A4n�A3��A1��A0JA-��A+�A++A*jA(�HA(A�A'��A'A%��A%"�A$�jA#�
A#;dA"��A"��A"Q�A!��A �9A ^5A/AQ�A�-AffAG�AE�A�9A�
A
=A~�A{A��AVA�/A��AQ�A�A�A�wA �A5?A�AoA{A�TAVA$�A�hA/A��A�#AVA�A�A
�!A
��A
�+A	�AbNAQ�Ap�AffA�^Ap�A;dA��A5?A�;AXA�A��A�RAjAJA�A7LA ��A ^5A E�A $�A �A bA J@��F@�@���@�ƨ@�K�@�@���@��#@�r�@�?}@�Q�@�b@���@�ƨ@�F@�P@�@�P@�ff@�V@�j@�1'@���@�;d@�x�@��@�w@�l�@�"�@��@��@�ȴ@�5?@�V@�(�@畁@�C�@柾@��#@�7@�%@��@�F@�|�@��@�+@�=q@�O�@��;@�+@�M�@ݑh@�G�@�/@�%@��`@ܬ@�1@�ff@�p�@��@�bN@��
@�+@��@�ff@���@�X@���@Դ9@�ƨ@�o@�M�@��#@���@�r�@Ѓ@�Z@� �@�dZ@�"�@�
=@�n�@��#@͉7@�z�@�b@�ƨ@���@��@��m@ˮ@�M�@�n�@�^5@���@��@��m@ǅ@�33@ư!@��@��@��T@ũ�@�?}@��`@ě�@�1'@��;@Ý�@�|�@�t�@�K�@�33@�o@�v�@�-@��T@�7L@�Ĝ@��9@�Z@���@�V@�M�@�$�@�@��#@���@�x�@�hs@�?}@���@��@�bN@��
@�v�@�p�@��9@� �@���@�|�@�"�@��@�ȴ@�^5@��@�@�hs@�7L@���@��u@�1'@�ƨ@�l�@���@�-@��@���@��@�O�@�1'@�C�@��@��R@��+@�{@�@�&�@��@��`@���@��9@���@�9X@���@�dZ@�C�@�+@��@���@�ff@���@�%@�Q�@��@��@���@�C�@��@���@�-@��-@��@�%@�A�@��m@���@��@�dZ@�;d@��+@�v�@�=q@��@��T@���@���@�`B@�V@��9@���@��/@��j@��9@�z�@�Z@�1@��m@��;@��F@�t�@�33@��@��!@�M�@�-@��@��@�hs@�?}@�/@��@��/@���@�j@�A�@�b@��;@�t�@��H@�=q@��@�O�@��/@��j@��j@���@�bN@� �@���@�t�@�"�@�
=@�ȴ@��+@���@��\@�~�@�-@��7@�&�@��/@��@�I�@�  @��w@��@�S�@�;d@�+@�"�@���@��R@�v�@�5?@��@���@���@��h@�x�@�O�@��/@��@�A�@�b@��;@���@�l�@�33@�@���@�n�@��@���@��7@�`B@�X@�/@�V@��@�Ĝ@�z�@�1'@��w@���@���@��@�C�@�33@��y@���@���@���@��-@�hs@���@��j@�r�@��@��m@��;@��
@���@�ƨ@�C�@���@���@�5?@�{@��T@�O�@�%@��/@��u@�I�@��@��@��w@�C�@��H@��\@�M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A͡�A͝�Aͣ�A͡�AͅA̓AͅA̓AͅÁA�z�A̓A�x�A�x�A�t�A�VA�I�A�C�A�1'A� �A� �A� �A�{A�oA�{A�oA�bA�JA�
=A�JA�1A�  A�A���A���A���A���A��A��A��A��A��A��A��A��yA��mA��yA��TA��TA��`A��`A��HA��HA��mA��TA��HA��TA��TA��;A��HA��HA��#A��;A��HA��;A��/A��/A��;A��/A��
A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A���A�ȴA���A���A���A���A���A���A���A���A���A�ȴA���A���A�ƨA�ȴA�ȴA���A̧�A̍PA�z�A�n�A�M�A�9XA�VA�
=A�bA�oA��A��A�$�A�&�A�1'A�9XA�;dA�9XA�-A��A���A��A��A���AˋDA�ffA�Q�A�?}A�=qA�E�A�E�A�C�A�I�A�K�A�K�A�K�A�Q�A�S�A�O�A�S�A�bNA�dZA�dZA�bNA�`BA�ZA�VA�\)A�dZA�XA�XA�\)A�XA�^5A�XA�VA�S�A�Q�A�=qA�+A��A���A��`A��yA��mA��`A��`A��`A��HA��`A��;A��/A��TA��TA��
A�ĜAʶFAʝ�AʁA�v�A�v�A�n�A�jA�jA�ffA�bNA�dZA�hsA�dZA�`BA�`BA�^5A�VA�S�A�S�A�I�A�C�A�;dA�&�A�{A�JA�A���A��A��A��A��mA��`A��TA��;A��#A��
A��A���A���A���A���A���A���A���A���A���A���A���A�ĜAɰ!Aɛ�AɋDA�z�A�r�A�jA�ffA�dZA�dZA�hsA�ffA�bNA�^5A�^5A�XA�O�A�G�A�C�A�C�A�A�A�?}A�9XA�7LA�33A�-A�+A�-A�(�A�$�A�&�A�&�A�$�A��A� �A��A��A�{A�{A���A��;A��/A��
A���A�ƨAȺ^AȬAț�Aȕ�AȍPAȃAȁAȅAȇ+Aȉ7Aȇ+A�~�A�x�A�r�A�l�A�n�A�ffA�\)A�VA�I�A�5?A��A���A���Aǉ7A�jA�VA�I�A�=qA�33A�5?A�33A�/A�33A�A�A�M�A�\)A�l�A�n�A�t�AǅAǍPAǑhAǙ�AǓuAǇ+A�~�A�|�A�p�A�VA�K�A�E�A�9XA�(�A� �A��A��A�bA�
=A�1A�
=A�A���A���A��A��A��A��mA��;A���A�ĜAƼjAƲ-AƩ�AƝ�Aƛ�Aƕ�AƍPAƋDAƉ7A�|�A�t�AƃA�~�A�z�A�x�A�ZA�ZA�K�A�K�A�M�A�O�A�I�A�G�A�I�A�C�A�;dA�7LA�?}A�33A�+A�(�A�&�A��A��A�oA�A��AŸRA�t�A�XA�G�A�9XA�$�A�1A��A��;A�ƨAġ�AčPAāA�v�A�\)A�E�A�;dA�"�A�oA�  AüjAÛ�AÏ\A�z�A�jA�^5A�VA�I�A�E�A�33A��A��A��`A���A¬ADA�jA�^5A�G�A�+A�JA��A��A�ĜA��RA���A���A���A���A��A�v�A�r�A�r�A�n�A�bNA�\)A�S�A�E�A�9XA�33A�(�A�VA���A��yA���A��A��7A�p�A�`BA�O�A�C�A�"�A��HA��A�E�A��A��RA���A��DA�v�A�bNA�G�A�9XA�-A��A��A�VA�%A�  A���A��TA�A��-A��\A�XA�+A���A��!A��uA�hsA�K�A�+A��A��A���A��^A���A�~�A�dZA�Q�A�&�A���A��`A�ƨA��A��+A�n�A�^5A�A�A�1'A�(�A� �A��A�JA�  A���A��A��/A��wA��A���A��hA�x�A�\)A�JA���A���A�l�A�9XA�%A��A���A��hA�z�A�I�A�"�A�  A��/A��A�^5A�O�A��A��yA��-A�ffA�ȴA��^A���A�Q�A�
=A��!A�A�A��A��^A��+A�O�A�{A�VA�  A��A��A��RA���A�z�A�XA�1'A��yA�S�A��A��A�v�A�n�A�hsA�dZA�^5A�VA�Q�A�M�A�9XA�/A�{A�%A��A��`A���A�ƨA��jA���A���A��uA��7A�|�A�ffA�Q�A�A�A�oA�  A��HA���A�p�A�bNA�`BA�E�A�VA��
A��+A�ffA�7LA��A�VA���A��A��A�ƨA���A�ffA�oA��
A���A�p�A�5?A���A��A��HA��#A��A���A���A���A�ĜA��jA��PA�n�A�+A��HA���A��^A�|�A�O�A�A���A��\A�n�A�=qA�1A��
A���A���A��+A�v�A�\)A�ZA�VA�E�A�I�A�9XA�7LA�5?A�(�A���A���A��A��HA��TA��#A�ȴA��RA�ZA�VA�ZA�C�A��/A���A�dZA�VA�7LA� �A���A��yA��jA��hA�;dA�{A���A��A��wA���A��hA�x�A�?}A�VA��A���A���A��\A�^5A�?}A��A�1A��yA��TA��
A���A��FA��hA��A�v�A�hsA�^5A�?}A�&�A��A�%A��!A��hA��7A�hsA��A��/A��7A�XA�1'A��A�1A��TA���A��jA���A��PA��7A�~�A�n�A�M�A��A���A�A��uA��A�$�A��wA��A�`BA�E�A�;dA��A��A���A�`BA�$�A��A�z�A���A��RA�O�A��A��A�A��A�\)A�A��FA��A�^5A�E�A��A��yA���A���A��FA��-A��A��A���A��uA�|�A�n�A�ffA�`BA�ZA�Q�A�K�A�G�A�E�A�A�A�7LA�&�A��A��A���A��A���A��\A��A�jA�VA�C�A�&�A���A���A���A�hsA�K�A�5?A�{A���A��A���A��A���A��7A�t�A�dZA�S�A�?}A��A�
=A���A���A���A���A���A���A���A���A��A���A��RA���A��DA�l�A�(�A���A��9A�S�A��A���A���A���A��hA�v�A�jA�ZA�K�A�9XA� �A�A���A�z�A�7LA��A��A���A��FA���A��7A�r�A�^5A�I�A�=qA�&�A�VA���A��mA��^A���A��A�dZA�VA���A�G�A���A�l�A��jA�;dA��yA��TA���A�ĜA��-A���A��PA��A�v�A�hsA�S�A�9XA�&�A��A�VA�1A���A���A��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                              111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B	�LB	�zB	��B	�zB	�B	�B	��B	�B	��B	��B	��B	��B	�B	�B	�?B	�B	�B	��B	�?B	�B	�B	��B	�B
QNB
��B
ѷB-�B`vBr�B��B��B��B��B�$B�=B��B�RB��B��B��B��B�B��B��B��B��B��B�gBĜB��B�B��B�BB��BʌB�KB�|B��BB�B"B�B �B($B+�B*eB$�B,B3hB;�B?HBJXBYBa�B]�BXyBVmBT�BMjB>B5tB1�B$tBBPBMB��B�pB��B�B��Bz�BuZBo�BcTBP}B>B6�B+B�B�B
�GB
�B
�B
�B
��B
��B
��B
{B
n/B
\]B
K�B
@�B
2�B
�B	�cB	ӏB	�HB	�-B	��B	�YB	rGB	Z�B	J�B	@�B	.B	=B	JB	+B	�B�(B�AB��B�BB�]B	CB	uB	�B	{B	B	MB	VB	�B	�B	oB	�B	�B	bB	4B	qB	)*B	4B	8RB	<�B	A B	B'B	B'B	A�B	B'B	A�B	?B	>�B	B'B	A�B	@�B	L�B	Q�B	S�B	PB	OvB	R�B	TaB	[�B	d�B	jB	j�B	l�B	y�B	z�B	}�B	��B	��B	�*B	�tB	��B	�~B	��B	��B	�hB	�B	��B	��B	�3B	�[B	��B	�B	�FB	�B	��B	�RB	�*B	�dB	��B	ʌB	��B	�0B	�B	�<B	�<B	�pB	бB	�vB	��B	ϫB	ΥB	��B	ϫB	�B	�HB	ϫB	�B	�HB	�B	��B	�B	�B	�}B	�TB	��B	ԕB	՛B	�?B	�KB	یB	چB	�B	�mB	�mB	��B	�
B	�sB	�KB	��B	��B	֡B	�
B	�B	�dB	ܒB	�5B	��B	��B	�jB	ߤB	�vB	�vB	�vB	�B	��B	�BB	ߤB	��B	�B	�mB	�mB	�sB	�B	�B	�cB	� B	�B	�B	�;B	��B	��B	�cB	��B	�B	��B	�B	�B	�B	�AB	�MB	�AB	�vB	�GB	��B	�MB	��B	�B	��B	�2B	�`B	��B	�>B	�8B	�+B	��B	�+B	��B	�B	�	B	�B	��B	�]B	��B
�B	��B	�cB	�.B	��B	�.B
B
uB
uB
B
 iB
�B
SB
�B
+B
+B
�B
�B
�B
�B
_B
�B
+B
�B
�B
1B
	�B

rB

�B

=B

�B

�B

=B

	B

	B
DB
�B
�B

	B

=B
�B
	�B
�B
fB
�B
�B
�B
1B
�B
�B
_B
�B
�B
�B
�B

rB
fB
+B
%B
+B
	�B
�B
�B
�B
�B
�B
�B
oB
B
:B
oB
:B
B
oB
�B
{B
B
�B
�B
�B
�B
B
$B
$B
�B
_B
+B
B
�B
�B
�B
�B
�B
	B
qB
�B
qB
=B
�B
=B
B
xB
�B
�B
�B
B
�B
�B
!�B
!bB
"hB
!bB
 'B
 �B
!�B
!�B
!�B
"4B
#nB
$�B
%�B
%B
%zB
%�B
'�B
($B
'�B
'�B
&LB
&�B
&�B
)*B
+B
+B
+6B
+kB
+�B
+�B
+�B
,qB
-B
,�B
,qB
-�B
-wB
-wB
-CB
.IB
/�B
/OB
/�B
/�B
0�B
0!B
/�B
/�B
/OB
/�B
1�B
0�B
1�B
0UB
0�B
0UB
0UB
0�B
0�B
0�B
2aB
2aB
1�B
1�B
3�B
3hB
3�B
4�B
4�B
5B
5B
4�B
3�B
4�B
5tB
6B
7LB
7LB
6�B
7B
7�B
8�B
8�B
9$B
9XB
8�B
9$B
9�B
;dB
;0B
;�B
<�B
=B
>B
>BB
>wB
>�B
?HB
?}B
?�B
@OB
@�B
@�B
@�B
@�B
A B
@�B
@�B
@�B
@�B
A�B
B[B
B�B
CaB
CaB
C�B
C�B
C�B
C�B
DgB
D�B
E�B
E�B
E9B
EB
D�B
D�B
FB
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
G�B
F�B
G�B
G�B
H�B
G�B
HKB
HKB
IB
I�B
J#B
K)B
K^B
K)B
K�B
LdB
M6B
NpG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	�zB	�FB	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�zB	�UB	�RB	��B	��B	��B	�FB	��B	�nB	�B	�FB	�nB	�9B	�B	�B	��B	�hB	��B	�B	�?B	�B	�nB	�B	��B	�9B	��B	��B	�9B	��B	��B	�B	��B	�tB	��B	��B	�tB	�B	�B	�B	��B	�hB	��B	�B	��B	��B	��B	�nB	�9B	�FB	�nB	�B	��B	�B	��B	��B	�9B	��B	�tB	�9B	�tB	�B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�?B	�tB	�B	��B	��B	�B	��B	�nB	��B	��B	��B	�B	��B	��B	�$B	��B	��B	�^B	�0B	�XB	��B	��B	��B	�OB	ÖB	�mB	�B
�B
B
'B
6zB
OBB
`vB
lWB
u%B
.B
��B
��B
�B
��B
��B
��B
��B
�HB
ѷB
�[B
��B
�NB
��B�B*�B8�BJ�BP�BT�B[�B^�Ba|Bc BdZBh>Bh
BjKBq�Bu�B|PB�AB�YB��B��B�	B��B��B�lB�B��B�xB��B�xB�'B��B��B��B�kB�XB�FB�FB��B�\B��B��B��B�zB��B�B��B��B��B�=B�}B��B��B�kB��B��B�tB�B�LB�FB�zB�B��B��B��B��B��B��B��B�0B�_B��B�eB�_B�0B�*B�zB�B�LB��B�4B��B��B��B��B�hB�B��B�:B��B�tB��B�nB��B��B�B��B�B��B��B�kB��B��B��B�FB��B��B�:B��B��B�6B�=B��B��B�IB�OB��B�?B�B�nB�B��B�[B��BB�gB��B�aB�3B�9B��BÖB��B�mB�3B�3B�B��B��B�#B�tB��B��B��B��B�wB�B�B��B�dB�B�jB�^B��B�B��B��B�6B��B�wB�<B�}B��B�B�qB�B�<B�6B��B�B�?B��B��B��B��B�wBB��B�aBB�RB��B� B�B�WBںB�B�B��B�B��B��B�B��B�B�]B��B��B�B�]B��B�B��B��B�GB�ZB�B��B�B�>B��B�DB�B��B��B�]B��B�"B��B��B��B��B��B��B�]B iB{B��B��BBAB%BB	7B�B�B�B	B�B_B�BB	B�B
	B�B	�B	�BBxB	�B"B.BYBoB"B(B�B"B"B�B�B�B.B"BPB�BhB�B�B7B	BB&�B!�B �B#:B!-B �B!-B$�B#:B#nB$�B&�B&�B(�B&�B+B*eB)*B+�B*0B,=B-�B,B.B+B,qB,B+kB+kB,�B,qB+�B*0B*�B,=B*�B*0B)�B*�B(XB(�B*�B)*B(�B'RB'�B&�B"�B�B \B!B \B$@B,qB(�B+kB+kB,B+B*�B+B.�B,=B+kB-�B,qB+�B+kB+�B,=B/B,�B,�B/�B3�B3�B<�B6�B4�B7B7�B6�B4nB<B7�B>BB5tB=B;�B9�B;dB=B=�B<B?B@�B@B=�B@�B>wB<�B>BB>BB>�B?HB=�B=�B@BB'B>�B=<BA B?�B@�BM�BFtBH�BH�BI�BK^BFBK^BK�BH�BN<BI�BK^BNpBP}BK�BJ�BQ�BNpBR BOBp;BO�BS[BV�B\�BW�BbB\�B[�B[�Ba�BZ�BYBY�BXB[�B[�BZ�BY�B\�BYKBb�BoiB��BffB_�B_;B^�B]dB]�B^�B^5B]dB^jB^jBbB`BB[�B[�BaBZ�B[�B\�B\]BYBY�BZQB[�BY�BYKBV�BUgBY�B]�BS�BPHBPBU2BV9B]�BYBWsB[�BW�BT�BS�BT�BTaBT�BT,B\�BZBXBU�BU�BZ�BT�BQ�BRTBP�BO�BPHBPHBO�BNBNpBU�BOBR�BS&BK�BH�BJ�BJ#BF?BK�BK^B>BBI�B9�BC-B>B<B:^B9�B8�B8B7�B5�B:*B9$B7�B4nB1'B=�B4B5B2�B.�B2-B7�B8�B7�B-B*�B-BN�B7�B(�B($B)�B)*B'�B&�B.�B'B+�B!bB!bB�B~BB�BB"�B+BBeBkB�B�B{B�BhB�B~B�B�B"B�B	B1B�B�B	�B�B�B�B�B��B�"BBB��B�]B�B�/B�QB�WB��B�B�sB�,B��B�B�B�BB�B�TB�5B�B�mB��B�B��B�}B��B�3B��B�9B�pB�6B�dB��B��B��B�@B��B��B�xB�_B��B��B�B�SB�(B�lB��B�MB��B��B~]B|�BzxBzBzxBy>By�BzxBzDByrBw2Bu�Bu%Bt�Bt�Bt�BsMBrGBsBr�BsMBv+Bp�Bm]Bj�Bi�BjKBk�BffBd�Bg�Bg8Bd�Bd&B]�BYKBT,BZBbNB[WBOBBG�BF�BD3BB[BB�BB�BA�BB�B=B:*B8�B8�B8�B9XB7�B6�B5�B7�B6FB6�B5?B2aB2aB0�B-�B/OB.�B*0B�BCB�B�B	B�BB4B�B(B"B\B:B�B�B
��B
��B
��B
�	B
��B
��B
��B
�B
�B
�oB
�B
�B
�B
�B
�yB
�sB
�TB
�iB
�B
�B
��B
��B
�HB
�sB
��B
�wB
��B
��B
��B
�jB
��B
�dB
�RB
��B
�*B
��B
�tB
�?B
�aB
��B
��B
��B
��444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                              444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                              444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022030700210720220307002107IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022031619005820220316190058QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022031619005820220316190058QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194420230210131944IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                